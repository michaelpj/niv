{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Char (isSpace)
import Data.FileEmbed (embedFile)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.String.QQ (s)
import GHC.Exts (toList)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess)
import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import qualified System.Directory as Dir

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.header "NIV - Version manager for Nix projects"
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    Opts.command "init" parseCmdInit <>
    Opts.command "add"  parseCmdAdd <>
    Opts.command "show"  parseCmdShow <>
    Opts.command "update"  parseCmdUpdate <>
    Opts.command "drop"  parseCmdDrop )

newtype Sources = Sources
  { unSources :: HMS.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getSources :: IO Sources
getSources = do
    warnIfOutdated
    -- TODO: if doesn't exist: run niv init
    putStrLn $ "Reading sources file"
    decodeFileStrict pathNixSourcesJson >>= \case
      Just (Aeson.Object obj) ->
        fmap (Sources . mconcat) $
          forM (HMS.toList obj) $ \(k, v) ->
            case v of
              Aeson.Object v' ->
                pure $ HMS.singleton (PackageName k) (PackageSpec v')
              _ -> abortAttributeIsntAMap
      Just _ -> abortSourcesIsntAMap
      Nothing -> abortSourcesIsntJSON

setSources :: Sources -> IO ()
setSources sources = encodeFile pathNixSourcesJson sources

newtype PackageName = PackageName { unPackageName :: T.Text }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

newtype PackageSpec = PackageSpec { _unPackageSpec :: Aeson.Object }
  deriving newtype (FromJSON, ToJSON, Show, Semigroup, Monoid)

parsePackageSpec :: Opts.Parser PackageSpec
parsePackageSpec =
    (PackageSpec . HMS.fromList . fmap fixupAttributes) <$>
      many parseAttribute
  where
    parseAttribute :: Opts.Parser (T.Text, T.Text)
    parseAttribute =
      Opts.option (Opts.maybeReader parseKeyVal)
        ( Opts.long "attribute" <>
          Opts.short 'a' <>
          Opts.metavar "KEY=VAL" <>
          Opts.help "Set the package spec attribute <KEY> to <VAL>"
        ) <|> shortcutAttributes <|>
      (("url_template",) <$> Opts.strOption
        ( Opts.long "template" <>
          Opts.short 't' <>
          Opts.metavar "URL" <>
          Opts.help "Used during 'update' when building URL. Occurrences of <foo> are replaced with attribute 'foo'."
        )) <|>
      (("type",) <$> Opts.strOption
        ( Opts.long "type" <>
          Opts.short 'T' <>
          Opts.metavar "TYPE" <>
          Opts.help "The type of the URL target. The value can be either 'file' or 'tarball'. If not set, the value is inferred from the suffix of the URL."
        ))

    -- Parse "key=val" into ("key", "val")
    parseKeyVal :: String -> Maybe (T.Text, T.Text)
    parseKeyVal str = case span (/= '=') str of
      (key, '=':val) -> Just (T.pack key, T.pack val)
      _ -> Nothing

    -- Shortcuts for common attributes
    shortcutAttributes :: Opts.Parser (T.Text, T.Text)
    shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
      [ "branch", "owner", "repo", "version" ]

    mkShortcutAttribute :: T.Text -> Opts.Parser (T.Text, T.Text)
    mkShortcutAttribute = \case
      attr@(T.uncons -> Just (c,_)) -> (attr,) <$> Opts.strOption
        ( Opts.long (T.unpack attr) <>
          Opts.short c <>
          Opts.metavar (T.unpack $ T.toUpper attr) <>
          Opts.help
            ( T.unpack $
              "Equivalent to --attribute " <>
              attr <> "=<" <> (T.toUpper attr) <> ">"
            )
        )
      _ -> empty

    fixupAttributes :: (T.Text, T.Text) -> (T.Text, Aeson.Value)
    fixupAttributes (k, v) = (k, Aeson.String v)

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec

-------------------------------------------------------------------------------
-- PACKAGE SPEC OPS
-------------------------------------------------------------------------------

data Result a
  = Pure a
  | Check Check (Result a)
  | Failed Failure
  | Io (Io a)
  | Set T.Text (Result T.Text) (Result a)
  | Load T.Text (T.Text -> Result a)
  | App (App a)
  | Alt (Result a) (Result a)

check :: Result a -> (a -> Bool) -> Result b -> Result b
check r ch next = Check (Chec r ch) next

data Check = forall b. Chec (Result b) (b -> Bool)

data Io a = forall b. I (Result b) (b -> IO a)

instance Functor Io where
  fmap f (I r i) = I r (fmap f . i)

data App a = forall b. Ap (Result (b -> a)) (Result b)

instance Functor App where
  fmap f (Ap l r) = Ap (fmap (f .) l) r

instance Alternative Result where
  empty = Failed AltEmpty
  l <|> r = Alt l r

instance Functor Result where
  fmap f (Pure x) = Pure (f x)
  fmap f (Check c next) = Check c (f <$> next)
  fmap f (Io x) = Io $ f <$> x
  fmap f (Set k v x) = Set k v (f <$> x)
  fmap f (Load k n) = Load k (fmap f . n)
  fmap _ (Failed f) = Failed f
  fmap f (App t) = App (fmap f t)
  fmap f (Alt l r) = Alt (f <$> l) (f <$> r)

instance Applicative Result where
  pure = Pure
  l <*> r = App (Ap l r)

type Foo = HMS.HashMap T.Text Val

data Val
  = Locked T.Text
  | CLILocked T.Text
  | Setted T.Text
  | Free T.Text
  deriving (Eq, Show)

data Failure
  = AltEmpty
  | NoSuchKey T.Text
  deriving Show

execResult :: Foo -> Result a -> IO a
execResult foo r = snd <$> runResult foo r

runResult :: Foo -> Result a -> IO (Foo, a)
runResult foo r = runResult' foo r >>= \case
  Left f -> error $ show f
  Right v -> pure v

runResult' :: Foo -> Result a -> IO (Either Failure (Foo, a))
runResult' foo = \case
  (Pure x) -> pure $ Right (foo, x)
  (Check (Chec v ch) next) -> do
    runResult' foo v >>= \case
      Left f -> pure $ Left f
      Right (foo', v') ->
        if ch v'
        then runResult' foo' next
        else pure $ Left AltEmpty
  (Io (I r i)) -> do
    runResult' foo r >>= \case
      Left f -> pure $ Left f
      Right (foo', v) -> do
        v' <- i v
        runResult' foo' (Pure v')
  (Set k v x) -> do
    runResult' foo v >>= \case
      Left f -> pure $ Left f
      Right (foo', v') -> case HMS.lookup k foo' of
        Just (Locked {}) -> runResult' (foo') x
        _ -> runResult' (HMS.singleton k (Setted v') <> foo') x -- TODO
  (Load k f) -> case lookupVal k foo of
    Just v -> runResult' foo (f v) -- TODO
    Nothing -> pure $ Left (NoSuchKey k) -- TODO
  (App (Ap l r)) -> do
    runResult' foo l >>= \case
      Left f -> pure $ Left f
      Right (foo', f) -> fmap (fmap f) <$> runResult' foo' r
  Failed f -> pure $ Left f
  Alt l r -> do
    runResult' foo l >>= \case
      Left AltEmpty -> runResult' foo r
      Left f -> pure $ Left f
      Right v -> pure $ Right v

lookupVal :: T.Text -> Foo -> Maybe T.Text
lookupVal k m = case HMS.lookup k m of
  Nothing -> Nothing
  Just v -> Just $ case v of
    Locked t -> t
    CLILocked t -> t
    Setted t -> t
    Free t -> t

test :: IO ()
test = do
  test1
  test2
  test3
  test4
  test5
  test6
  test7

test1 :: IO ()
test1 = do
    res <- execResult foo $ pure 2
    print (res :: Int)
    unless (res == 2) $ error "bad value"
  where
    foo = HMS.empty

test2 :: IO ()
test2 = do
    res <- execResult foo $ do
      load "bar"
    T.putStrLn res
    unless (res == "baz") $ error "bad value"
  where
    foo = HMS.singleton "bar" (Free "baz")

test3 :: IO ()
test3 = do
    res <- execResult foo $ do
      3 <$ empty <|> pure 2
    print (res :: Int)
    unless (res == 2) $ error "bad value"
  where
    foo = HMS.empty

test4 :: IO ()
test4 = do
    (foo', ()) <- runResult foo $ do
      set "baz" (pure "foo")
    unless (HMS.lookup "baz" foo' == Just (Setted "foo")) $ error "bad value"
  where
    foo = HMS.singleton "baz" (Free "not-foo")

test5 :: IO ()
test5 = do
    (foo', ()) <- runResult foo $

      (check (load "val") (== "left") $
        set "res" (pure "I saw left")
        ) <|> (check (load "val") (== "right") $
        set "res" (pure "I saw right")
        )

    print foo'
    unless
      (HMS.lookup "res" foo' == Just (Setted "I saw right"))
      (error "bad value")
  where
    foo = HMS.singleton "val" (Free "right")

test6 :: IO ()
test6 = do
    (foo', ()) <- runResult foo $ do

      -- let owner = load "owner" -- LOAD, must exist already
      -- let repo = load "repo" -- LOAD, must exist already
      let branch = load "branch" <|> pure "master" -- LOAD or master
      set "branch" branch
      let url_template = "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

      set "url_template" (pure url_template) -- SET the url_template, even if it exists, unless locked

      set "rev" $ io (pure ()) $ \() ->  pure "foobar" -- ("some-rev" <> owner <> repo <> branch)

      pure ()

    print foo'
  where
    foo = HMS.fromList
      [ ("owner", Free "nmattia")
      , ("repo", Free "niv")
      , ("branch", Free "master")
      ]

test7 :: IO ()
test7 = do
    let fooz = do
          let
            owner = load "owner"
            repo = load "repo"
            branch = load "branch" <|> pure "master"
          set "branch" branch
          let url_template = pure "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

          set "url_template" url_template -- SET the url_template, even if it exists, unless locked

          set "rev" $ io ((,,) <$> owner <*> repo <*> branch) $
            \(_, _, _) -> pure "foobar"


          set "foo" (pure "bar")
          pure ()

    T.putStrLn $ describe fooz

    (foo', ()) <- runResult foo $ fooz
    print foo'
    unless (HMS.lookup "rev" foo' == Just (Locked "basic")) $ error "bad"
  where
    foo = HMS.fromList
      [ ("owner", Free "nmattia")
      , ("repo", Free "niv")
      , ("branch", Free "master")
      , ("rev", Locked "basic")
      ]

test8 :: IO ()
test8 = do
    let fooz = do
          let
            someio = io (pure ()) (\() -> pure "new")
          set "val1" $ fmap ("1-" <>) someio
          set "val2" $ fmap ("2-" <>) someio
          pure ()

    T.putStrLn $ describe fooz

    (foo', ()) <- runResult foo $ fooz
    print foo'
    unless
      ( HMS.lookup "val1" foo' == Just (Locked "1-old") &&
        HMS.lookup "val2" foo' == Just (Locked "2-old")
      ) $ error "bad"
  where
    foo = HMS.fromList
      [ ("val1", Free "1-old")
      , ("val2", Locked "2-old")
      ]

test9 :: IO ()
test9 = do
    let fooz = io (pure ()) (\() -> error "baz" >>pure ())

    T.putStrLn $ describe fooz

    (foo', ()) <- runResult HMS.empty $ fooz
    print foo'

load :: T.Text -> Result T.Text
load k = Load k Pure

set :: T.Text -> Result T.Text -> Result ()
set k v = Set k v (pure ())

io :: Result a -> (a -> IO b) -> Result b
io r f = Io (I r f)

describe :: Result a -> T.Text
describe = \case
  (Alt r l) -> "Alt!" <> describe l <> describe r
  (Check _ _) -> "Check!"
  (Pure _) -> "Pure!"
  (Io _) -> "IO!"
  (Set k v n) -> "Setting " <> k <> " to " <> describe v <> ", then " <> describe n
  (Load k n) -> T.unlines
    [ "Loading " <> k <> ","
    , "  " <> describe (n "bar")
    ]
  (App (Ap l r)) -> "App!" <>
    "(L " <> describe l <> ")" <>
    "(R " <> describe r <> ")"
  Failed f -> "Failed :(" <> tshow f

githubUpdate :: Result ()
githubUpdate = do
    -- let
      -- url = load "url"
      -- urlTemplate
    let branch = load "branch" <|> pure "master" -- LOAD or master
    set "branch" branch -- SET the branch
    let url_template = "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

    set "url_template" (pure url_template) -- SET the url_template, even if it exists, unless locked

    let rev = io (pure ()) $ \() -> pure "foo"

    -- SET the rev, even if it exists, unless the rev is locked
    set "rev" rev

    pure ()
    -- doUnpack <-
      -- (pure True)

    -- SET the sha256, only if the pure dependencies have change
    -- sha256 <- fmap T.pack . io $ nixPrefetchURL True "foo"
    -- set "sha256" sha256
    -- pure ()

updatePackageSpec :: PackageSpec -> IO PackageSpec
updatePackageSpec = execStateT $ do
    originalUrl <- getPackageSpecAttr "url"

    -- Figures out the URL from the template
    withPackageSpecAttr "url_template" (\case
      Aeson.String (T.unpack -> template) -> do
        packageSpec <- get
        let stringValues = packageSpecStringValues packageSpec
        case renderTemplate stringValues template of
          Just renderedURL ->
            setPackageSpecAttr "url" (Aeson.String $ T.pack renderedURL)
          Nothing -> pure ()
      _ -> pure ()
      )

    -- If the type attribute is not set, we try to infer its value based on the url suffix
    (,) <$> getPackageSpecAttr "type" <*> getPackageSpecAttr "url" >>= \case
      -- If an url type is set, we'll use it
      (Just _, _) -> pure ()
      -- We need an url to infer a url type
      (_, Nothing) -> pure ()
      (Nothing, Just (Aeson.String url)) -> do
         let urlType = if "tar.gz" `T.isSuffixOf` url
                       then "tarball"
                       else "file"
         setPackageSpecAttr "type" (Aeson.String $ T.pack urlType)
      -- If the JSON value is not a string, we ignore it
      (_, _) -> pure ()

    -- Updates the sha256 based on the URL contents
    (,) <$> getPackageSpecAttr "url" <*> getPackageSpecAttr "sha256" >>= \case
      -- If no URL is set, we simply can't prefetch
      (Nothing, _) -> pure ()

      -- If an URL is set and no sha is set, /do/ update
      (Just url, Nothing) -> prefetch url

      -- If both the URL and sha are set, update only if the url has changed
      (Just url, Just{}) -> when (Just url /= originalUrl) (prefetch url)
  where
    prefetch :: Aeson.Value -> StateT PackageSpec IO ()
    prefetch = \case
      Aeson.String (T.unpack -> url) -> do
        unpack <- getPackageSpecAttr "type" <&> \case
          -- Do not unpack if the url type is 'file'
          Just (Aeson.String urlType) -> not $ T.unpack urlType == "file"
          _ -> True
        sha256 <- liftIO $ nixPrefetchURL unpack url
        setPackageSpecAttr "sha256" (Aeson.String $ T.pack sha256)
      _ -> pure ()

completePackageSpec
  :: PackageSpec
  -> IO (PackageSpec)
completePackageSpec = execStateT $ do

    -- In case we have @owner@ and @repo@, pull some data from GitHub
    (,) <$> getPackageSpecAttr "owner" <*> getPackageSpecAttr "repo" >>= \case
      (Just (Aeson.String owner), Just (Aeson.String repo)) -> do
          liftIO (GH.executeRequest' $ GH.repositoryR (GH.N owner) (GH.N repo))
            >>= \case
              Left e ->
                liftIO $ warnCouldNotFetchGitHubRepo e (T.unpack owner, T.unpack repo)
              Right ghRepo -> do

                -- Description
                whenNotSet "description" $ case GH.repoDescription ghRepo of
                  Just descr ->
                    setPackageSpecAttr "description" (Aeson.String descr)
                  Nothing -> pure ()

                whenNotSet "homepage" $ case GH.repoHomepage ghRepo of
                  Just descr ->
                    setPackageSpecAttr "homepage" (Aeson.String descr)
                  Nothing -> pure ()

                -- Branch and rev
                whenNotSet "branch" $ case GH.repoDefaultBranch ghRepo of
                  Just branch ->
                    setPackageSpecAttr "branch" (Aeson.String branch)
                  Nothing -> pure ()

                withPackageSpecAttr "branch" (\case
                  Aeson.String branch -> do
                    liftIO (githubLatestRev owner repo branch) >>= \case
                      Just rev -> setPackageSpecAttr "rev" (Aeson.String rev)
                      Nothing -> pure ()
                  _ -> pure ()
                  )
      (_,_) -> pure ()

    -- Figures out the URL template
    whenNotSet "url_template" $
      setPackageSpecAttr
        "url_template"
        (Aeson.String githubURLTemplate)

  where
    githubURLTemplate :: T.Text
    githubURLTemplate =
      "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

-- | Get the latest revision for owner, repo and branch.
-- TODO: explain no error handling
githubLatestRev
  :: T.Text
  -- ^ owner
  -> T.Text
  -- ^ repo
  -> T.Text
  -- ^ branch
  -> IO (Maybe T.Text)
githubLatestRev owner repo branch =
    GH.executeRequest' (
      GH.commitsWithOptionsForR (GH.N owner) (GH.N repo) (GH.FetchAtLeast 1)
      [GH.CommitQuerySha branch]
      ) >>= \case
        Right (toList -> (commit:_)) -> do
          let GH.N rev = GH.commitSha commit
          pure $ Just rev
        _ -> pure Nothing

-------------------------------------------------------------------------------
-- PackageSpec State helpers
-------------------------------------------------------------------------------

whenNotSet
  :: T.Text
  -> StateT PackageSpec IO ()
  -> StateT PackageSpec IO ()
whenNotSet attrName act = getPackageSpecAttr attrName >>= \case
  Just _ -> pure ()
  Nothing -> act

withPackageSpecAttr
  :: T.Text
  -> (Aeson.Value -> StateT PackageSpec IO ())
  -> StateT PackageSpec IO ()
withPackageSpecAttr attrName act = getPackageSpecAttr attrName >>= \case
  Just v -> act v
  Nothing -> pure ()

getPackageSpecAttr
  :: T.Text
  -> StateT PackageSpec IO (Maybe Aeson.Value)
getPackageSpecAttr attrName = do
  PackageSpec obj <- get
  pure $ HMS.lookup attrName obj

setPackageSpecAttr
  :: T.Text -> Aeson.Value
  -> StateT PackageSpec IO ()
setPackageSpecAttr attrName attrValue = do
  PackageSpec obj <- get
  let obj' = HMS.insert attrName attrValue obj
  put (PackageSpec obj')

packageSpecStringValues :: PackageSpec -> [(String, String)]
packageSpecStringValues (PackageSpec m) = mapMaybe toVal (HMS.toList m)
  where
    toVal :: (T.Text, Aeson.Value) -> Maybe (String, String)
    toVal = \case
      (key, Aeson.String val) -> Just (T.unpack key, T.unpack val)
      _ -> Nothing

-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------

parseCmdInit :: Opts.ParserInfo (IO ())
parseCmdInit = Opts.info (pure cmdInit <**> Opts.helper) $ mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc
          "Initialize a Nix project. Existing files won't be modified."
      ]

cmdInit :: IO ()
cmdInit = do

    -- Writes all the default files
    -- a path, a "create" function and an update function for each file.
    forM_
      [ ( pathNixSourcesNix
        , (`createFile` initNixSourcesNixContent)
        , \path content -> do
            if shouldUpdateNixSourcesNix content
            then do
              putStrLn "Updating sources.nix"
              B.writeFile path initNixSourcesNixContent
            else putStrLn "Not updating sources.nix"
        )
      , ( pathNixSourcesJson
        , \path -> do
            createFile path initNixSourcesJsonContent
            -- Imports @niv@ and @nixpkgs@ (18.09)
            putStrLn "Importing 'niv' ..."
            cmdAdd Nothing (PackageName "nmattia/niv", PackageSpec HMS.empty)
            putStrLn "Importing 'nixpkgs' ..."
            cmdAdd
              (Just (PackageName "nixpkgs"))
              ( PackageName "NixOS/nixpkgs-channels"
              , PackageSpec (HMS.singleton "branch" "nixos-18.09"))
        , \path _content -> dontCreateFile path)
      ] $ \(path, onCreate, onUpdate) -> do
          exists <- Dir.doesFileExist path
          if exists then B.readFile path >>= onUpdate path else onCreate path
  where
    createFile :: FilePath -> B.ByteString -> IO ()
    createFile path content = do
      let dir = takeDirectory path
      Dir.createDirectoryIfMissing True dir
      putStrLn $ "Creating " <> path
      B.writeFile path content
    dontCreateFile :: FilePath -> IO ()
    dontCreateFile path = putStrLn $ "Not creating " <> path

-------------------------------------------------------------------------------
-- ADD
-------------------------------------------------------------------------------

parseCmdAdd :: Opts.ParserInfo (IO ())
parseCmdAdd =
    Opts.info ((cmdAdd <$> optName <*> parsePackage) <**> Opts.helper) $
      mconcat desc
  where
    optName :: Opts.Parser (Maybe PackageName)
    optName = Opts.optional $ PackageName <$>  Opts.strOption
      ( Opts.long "name" <>
        Opts.short 'n' <>
        Opts.metavar "NAME" <>
        Opts.help "Set the package name to <NAME>"
      )
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Add dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv add stedolan/jq" Opts.<$$>
          "  niv add NixOS/nixpkgs-channels -n nixpkgs -b nixos-18.09" Opts.<$$>
          "  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip"
      ]

cmdAdd :: Maybe PackageName -> (PackageName, PackageSpec) -> IO ()
cmdAdd mPackageName (PackageName str, spec) = do

    -- Figures out the owner and repo
    (packageName, spec') <- flip runStateT spec $ case T.span (/= '/') str of
          ( owner@(T.null -> False)
            , T.uncons -> Just ('/', repo@(T.null -> False))) -> do
            whenNotSet "owner" $
              setPackageSpecAttr "owner" (Aeson.String owner)
            whenNotSet "repo" $ do
                setPackageSpecAttr "repo" (Aeson.String repo)
            pure (PackageName repo)
          _ -> pure (PackageName str)

    sources <- unSources <$> getSources

    let packageName' = fromMaybe packageName mPackageName

    when (HMS.member packageName' sources) $
      abortCannotAddPackageExists packageName'

    spec'' <- updatePackageSpec =<< completePackageSpec spec'

    putStrLn $ "Writing new sources file"
    setSources $ Sources $
      HMS.insert packageName' spec'' sources

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow = Opts.info (pure cmdShow <**> Opts.helper) Opts.fullDesc

cmdShow :: IO ()
cmdShow = do
    putStrLn $ "Showing sources file"

    sources <- unSources <$> getSources

    forWithKeyM_ sources $ \key (PackageSpec spec) -> do
      T.putStrLn $ "Package: " <> unPackageName key
      forM_ (HMS.toList spec) $ \(attrName, attrValValue) -> do
        let attrValue = case attrValValue of
              Aeson.String str -> str
              _ -> "<barabajagal>"
        putStrLn $ "  " <> T.unpack attrName <> ": " <> T.unpack attrValue

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

parseCmdUpdate :: Opts.ParserInfo (IO ())
parseCmdUpdate =
    Opts.info
      ((cmdUpdate <$> Opts.optional parsePackage) <**> Opts.helper) $
      mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Update dependencies"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv update" Opts.<$$>
          "  niv update nixpkgs" Opts.<$$>
          "  niv update my-package -v beta-0.2"
      ]

cmdUpdate :: Maybe (PackageName, PackageSpec) -> IO ()
cmdUpdate = \case
    Just (packageName, packageSpec) -> do
      T.putStrLn $ "Updating single package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      packageSpec' <- case HMS.lookup packageName sources of
        Just packageSpec' -> do

          -- TODO: something fishy happening here
          pkgSpec <- completePackageSpec $ packageSpec <> packageSpec'
          updatePackageSpec $ pkgSpec

        Nothing -> abortCannotUpdateNoSuchPackage packageName

      setSources $ Sources $
        HMS.insert packageName packageSpec' sources

    Nothing -> do
      sources <- unSources <$> getSources

      sources' <- forWithKeyM sources $
        \packageName packageSpec -> do
          T.putStrLn $ "Package: " <> unPackageName packageName
          updatePackageSpec =<< completePackageSpec packageSpec

      setSources $ Sources sources'

-------------------------------------------------------------------------------
-- DROP
-------------------------------------------------------------------------------

parseCmdDrop :: Opts.ParserInfo (IO ())
parseCmdDrop =
    Opts.info
      ((cmdDrop <$> parsePackageName <*> parseDropAttributes) <**>
        Opts.helper) $
      mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Drop dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv drop jq" Opts.<$$>
          "  niv drop my-package version"
      ]
    parseDropAttributes :: Opts.Parser [T.Text]
    parseDropAttributes = many $
      Opts.argument Opts.str (Opts.metavar "ATTRIBUTE")

cmdDrop :: PackageName -> [T.Text] -> IO ()
cmdDrop packageName = \case
    [] -> do
      T.putStrLn $ "Dropping package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      when (not $ HMS.member packageName sources) $
        abortCannotDropNoSuchPackage packageName

      setSources $ Sources $
        HMS.delete packageName sources
    attrs -> do
      putStrLn $ "Dropping attributes :" <>
        (T.unpack (T.intercalate " " attrs))
      T.putStrLn $ "In package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      packageSpec <- case HMS.lookup packageName sources of
        Nothing ->
          abortCannotAttributesDropNoSuchPackage packageName
        Just (PackageSpec packageSpec) -> pure $ PackageSpec $
          HMS.mapMaybeWithKey
            (\k v -> if k `elem` attrs then Nothing else Just v) packageSpec

      setSources $ Sources $
        HMS.insert packageName packageSpec sources

-------------------------------------------------------------------------------
-- Aux
-------------------------------------------------------------------------------

--- Aeson

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decodeFileStrict :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileStrict = fmap Aeson.decodeStrict . B.readFile

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . AesonPretty.encodePretty' config
  where
    config =  AesonPretty.defConfig { AesonPretty.confTrailingNewline = True }

--- HashMap

forWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => HMS.HashMap k v1
  -> (k -> v1 -> m v2)
  -> m (HMS.HashMap k v2)
forWithKeyM = flip mapWithKeyM

forWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => HMS.HashMap k v1
  -> (k -> v1 -> m ())
  -> m ()
forWithKeyM_ = flip mapWithKeyM_

mapWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m v2)
  -> HMS.HashMap k v1
  -> m (HMS.HashMap k v2)
mapWithKeyM f m = do
    fmap mconcat $ forM (HMS.toList m) $ \(k, v) ->
      HMS.singleton k <$> f k v

mapWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m ())
  -> HMS.HashMap k v1
  -> m ()
mapWithKeyM_ f m = do
    forM_ (HMS.toList m) $ \(k, v) ->
      HMS.singleton k <$> f k v

-- | Renders the template. Returns 'Nothing' if some of the attributes are
-- missing.
--
--  renderTemplate [("foo", "bar")] "<foo>" == Just "bar"
--  renderTemplate [("foo", "bar")] "<baz>" == Nothing
renderTemplate :: [(String, String)] -> String -> Maybe String
renderTemplate vals = \case
    '<':str -> do
      case span (/= '>') str of
        (key, '>':rest) ->
          liftA2 (<>) (lookup key vals) (renderTemplate vals rest)
        _ -> Nothing
    c:str -> (c:) <$> renderTemplate vals str
    [] -> Just []

abort :: T.Text -> IO a
abort msg = do
    T.putStrLn msg
    exitFailure

nixPrefetchURL :: Bool -> String -> IO String
nixPrefetchURL unpack url =
    lines <$> readProcess "nix-prefetch-url" args "" >>=
      \case
        (l:_) -> pure l
        _ -> abortNixPrefetchExpectedOutput
  where args = if unpack then ["--unpack", url] else [url]

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- | Checks if content is different than default and if it does /not/ contain
-- a comment line with @niv: no_update@
shouldUpdateNixSourcesNix :: B.ByteString -> Bool
shouldUpdateNixSourcesNix content =
    content /= initNixSourcesNixContent &&
      not (any lineForbids (B8.lines content))
  where
    lineForbids :: B8.ByteString -> Bool
    lineForbids str =
      case B8.uncons (B8.dropWhile isSpace str) of
        Just ('#',rest) -> case B8.stripPrefix "niv:" (B8.dropWhile isSpace rest) of
          Just rest' -> case B8.stripPrefix "no_update" (B8.dropWhile isSpace rest') of
            Just{} -> True
            _ -> False
          _ -> False
        _ -> False

warnIfOutdated :: IO ()
warnIfOutdated = do
    tryAny (B.readFile pathNixSourcesNix) >>= \case
      Left e -> T.putStrLn $ T.unlines
        [ "Could not read " <> T.pack pathNixSourcesNix
        , "Error: " <> tshow e
        ]
      Right content ->
        if shouldUpdateNixSourcesNix content
        then
          T.putStrLn $ T.unlines
            [ "WARNING: " <> T.pack pathNixSourcesNix <> " is out of date."
            , "Please run"
            , "  niv init"
            , "or add the following line in the " <> T.pack pathNixSourcesNix <> "  file:"
            , "  # niv: no_update"
            ]
        else pure ()

-- | @nix/sources.nix@
pathNixSourcesNix :: FilePath
pathNixSourcesNix = "nix" </> "sources.nix"

-- | Glue code between nix and sources.json
initNixSourcesNixContent :: B.ByteString
initNixSourcesNixContent = $(embedFile "nix/sources.nix")

-- | @nix/sources.json"
pathNixSourcesJson :: FilePath
pathNixSourcesJson = "nix" </> "sources.json"

-- | Empty JSON map
initNixSourcesJsonContent :: B.ByteString
initNixSourcesJsonContent = "{}"

-------------------------------------------------------------------------------
-- Warn
-------------------------------------------------------------------------------

warnCouldNotFetchGitHubRepo :: GH.Error -> (String, String) -> IO ()
warnCouldNotFetchGitHubRepo e (owner, repo) =
    putStrLn $ unlines [ line1, line2, line3 ]
  where
    line1 = "WARNING: Could not read from GitHub repo: " <> owner <> "/" <> repo
    line2 = [s|
I assumed that your package was a GitHub repository. An error occurred while
gathering information from the repository. Check whether your package was added
correctly:

  niv show

If not, try re-adding it:

  niv drop <package>
  niv add <package-without-typo>

Make sure the repository exists.
|]
    line3 = unwords [ "(Error was:", show e, ")" ]

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortSourcesIsntAMap :: IO a
abortSourcesIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The sources file should be a JSON map from package name to package
specification, e.g.:
  { ... }
|]

abortAttributeIsntAMap :: IO a
abortAttributeIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The package specifications in the sources file should be JSON maps from
attribute name to attribute value, e.g.:
  { "nixpkgs": { "foo": "bar" } }
|]

abortSourcesIsntJSON :: IO a
abortSourcesIsntJSON = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = "The sources file should be JSON."

abortCannotAddPackageExists :: PackageName -> IO a
abortCannotAddPackageExists (PackageName n) = abort $ T.unlines
    [ "Cannot add package " <> n <> "."
    , "The package already exists. Use"
    , "  niv drop " <> n
    , "and then re-add the package. Alternatively use"
    , "  niv update " <> n <> " --attr foo=bar"
    , "to update the package's attributes."
    ]

abortCannotUpdateNoSuchPackage :: PackageName -> IO a
abortCannotUpdateNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot update package " <> n <> "."
    , "The package doesn't exist. Use"
    , "  niv add " <> n
    , "to add the package."
    ]

abortCannotDropNoSuchPackage :: PackageName -> IO a
abortCannotDropNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot drop package " <> n <> "."
    , "The package doesn't exist."
    ]

abortCannotAttributesDropNoSuchPackage :: PackageName -> IO a
abortCannotAttributesDropNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot drop attributes of package " <> n <> "."
    , "The package doesn't exist."
    ]

abortNixPrefetchExpectedOutput :: IO a
abortNixPrefetchExpectedOutput = abort [s|
Could not read the output of 'nix-prefetch-url'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]

tshow :: Show a => a -> T.Text
tshow = T.pack . show
