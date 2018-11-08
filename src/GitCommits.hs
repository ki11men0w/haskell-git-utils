{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE OverloadedStrings #-}
module GitCommits
    ( Textual
    , RepoName
    , RefName
    , GitHash
    , GitReference(..)
    , GitCommit(..)
    , GitVersion(..)
    , GitCommitsMap
    , RemoteRepo(..)
    , gitError
    , getGitVersion
    , getLogCommits
    , getLogCommitsMap
    , hashToCommit
    , hashToCommit'
    , hashesToCommits
    , toCommits
    , toHashes
    , getTopCommits
    , getTags
    , getBranches
    , getRemoteBranches
    , getRemoteRepos
    ) where

import Prelude hiding (takeWhile, take, lookup, hGetContents)
import System.Process
import Control.Monad (void)
import Data.Maybe (mapMaybe, maybe, listToMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate, sortOn)
import System.Exit (ExitCode(..))
import Data.Map (Map(..), fromList, elems, keys, lookup)
import Control.Applicative (many, (<|>))
import Data.Attoparsec.Combinator

import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString.Char8

import Data.Text as T hiding (count, head, null, filter)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)


import Data.Time.Clock.POSIX
import Data.Time.Clock

type InputData = C8.ByteString
type Textual = Text

data GitVersion = GitVersion {major :: Int, minor :: Int} deriving (Eq, Ord)

instance Show GitVersion where
  show v = show (major v) <> "." <> show (minor v)


type RepoName = Textual
type RefName = Textual
type GitHash = Textual
type UserName = Textual
type UserEmail = Textual
type MessageText = Textual
type GpgSignature = Textual


data GitReference = GitRef Textual (Maybe GitReference) | GitTag RefName | GitBranch RefName | GitRemoteBranch RepoName RefName
                  deriving (Show)

data OwnershipInfo = OwnershipInfo {userName :: UserName, userEmail :: UserEmail, utcTime :: UTCTime}
  deriving (Show)

data GitCommit = GitCommit {
    hash :: GitHash
  , tree :: GitHash
  , childrens :: [GitHash]
  , parents :: [GitHash]
  , author :: OwnershipInfo
  , committer :: OwnershipInfo
  , gpgsig :: Maybe GpgSignature
  , message :: Maybe MessageText
  , refs :: [GitReference]
  } deriving (Show)


type GitCommitsMap = Map GitHash GitCommit

data LogField = LogFieldParent GitHash
              | LogFieldTree GitHash
              | LogFieldAuthor OwnershipInfo
              | LogFieldCommitter OwnershipInfo
              | LogFieldGpgSignature GpgSignature
              | LogFieldUnused
                deriving (Show)

data RemoteRepo = RemoteRepo {
      getRemoteRepoName :: RepoName
    } deriving (Show)

gitError = error "Error running git process"

correctParseResult :: String -> Either String a -> Either String a
correctParseResult errorMsg parseResult =
  case parseResult of
    Left orig -> Left $ errorMsg <> ":\n" <> orig
    _ -> parseResult

decode :: InputData -> Textual
decode = decodeUtf8With lenientDecode

parseVersion :: Parser GitVersion
parseVersion = do
  string "git version "
  major <- many1 digit
  char '.'
  minor <- many1 digit
  return $ GitVersion (read major) (read minor)

getGitVersion :: IO (Either String GitVersion)
getGitVersion =
  withCreateProcess (shell "git --version"){std_out = CreatePipe} $ \_ (Just outh) _ ph -> do
    exitCode <- waitForProcess ph
    case exitCode of
      ExitFailure _ -> gitError
      ExitSuccess -> do
        rawVersionInput <- C8.hGetContents outh
        return $ correctParseResult "Can not realize GIT version" $ parseOnly parseVersion rawVersionInput

parseToken :: Parser Textual
parseToken = decode <$> takeWhile1 (\c -> notInClass "),\n\r" c && (not . isSpace) c)

parseGitCommitHash :: Parser GitHash
parseGitCommitHash = pack <$> count 40 (satisfy (inClass "0-9a-f"))

parseCommitLog :: [RemoteRepo] -> Parser [GitCommit]
parseCommitLog remoteRepos =
  ((parseCommit remoteRepos) `sepBy1` endOfLine) <* endOfInput


parseGitRef :: [RemoteRepo] -> Parser GitReference
parseGitRef remoteRepos = do
  name <- parseToken
  ref <- option Nothing $ lookAhead $ string " -> " *> (Just <$> parseReference remoteRepos)
  return $ GitRef name ref

parseGitBranch :: Parser GitReference
parseGitBranch = do
  string "refs/heads/"
  name <- parseToken
  return $ GitBranch name

parseGitRemoteBranch :: [RemoteRepo] -> Parser GitReference
parseGitRemoteBranch remoteRepos = do
  string "refs/remotes/"
  repo <- choice $ findStringFollowedBySlash . getRemoteRepoName <$> remoteRepos
  name <- parseToken
  return $ GitRemoteBranch repo name
  where
    findStringFollowedBySlash :: RepoName -> Parser RepoName
    findStringFollowedBySlash repoName =
       (pack . C8.unpack) <$> try (string ((C8.pack . unpack) repoName) <* char '/')

parseGitTag :: Parser GitReference
parseGitTag = GitTag <$> (string "tag: refs/tags/" *> parseToken)

parseReference :: [RemoteRepo] -> Parser GitReference
parseReference remoteRepos =
  parseGitBranch <|> (parseGitRemoteBranch remoteRepos) <|> parseGitTag <|> (parseGitRef remoteRepos)

parseReferences :: [RemoteRepo] -> Parser [GitReference]
parseReferences remoteRepos = do
  char '('
  result <- (parseReference remoteRepos) `sepBy1` (string ", " <|> string " -> ")
  char ')'
  return result

parseParent :: Parser LogField
parseParent = string "parent " *> (LogFieldParent <$> parseGitCommitHash) <* endOfLine

parseTree :: Parser LogField
parseTree = string "tree " *> (LogFieldTree <$> parseGitCommitHash) <* endOfLine

skipRestOfLine :: Parser ()
skipRestOfLine =
  takeWhile1 (notInClass "\n\r") *> endOfLine *> pure ()

parseOwnershipInfo :: Parser OwnershipInfo
parseOwnershipInfo = do
  name <- manyTill anyChar (lookAhead $ string " <")
  char ' '
  email <- char '<' *> many1 (notChar '>') <* char '>'
  char ' '
  posixTime <- many1 digit
  skipRestOfLine
  return OwnershipInfo {
                     userName = pack name
                   , userEmail = pack email
                   , utcTime = (posixSecondsToUTCTime . realToFrac . read) posixTime
                   }

parseAuthor :: Parser LogField
parseAuthor = do
  string "author "
  LogFieldAuthor <$> parseOwnershipInfo

parseCommitter :: Parser LogField
parseCommitter = do
  string "committer "
  LogFieldCommitter <$> parseOwnershipInfo

parseGpgSig :: Parser LogField
parseGpgSig = do
  LogFieldGpgSignature <$> (string "gpgsig" *> (decode . C8.intercalate "\n" <$> oneLine `sepBy` endOfLine) <* endOfLine)
  where
    padding = string " " *> return ()
    oneLine = C8.pack <$> (padding *> manyTill anyChar (lookAhead endOfLine))

parseLogFields :: Parser [LogField]
parseLogFields =
  many $ choice [parseParent, parseAuthor, parseCommitter, parseTree, parseGpgSig]


commitStart :: Parser ()
commitStart = void $ string "commit "

parseMessage :: Parser (Maybe Textual)
parseMessage =
  option Nothing $ endOfLine *> (Just . decode . C8.intercalate "\n" <$> oneLine `sepBy` endOfLine) <* endOfLine
  where
    padding = string "    " *> return ()

    anyMessageEndsWithIt = endOfLine *> (endOfInput <|> padding <|> (endOfLine *> commitStart))
    oneLine :: Parser InputData
    oneLine = padding *> (C8.pack <$> manyTill anyChar (lookAhead anyMessageEndsWithIt))

parseCommit :: [RemoteRepo] -> Parser GitCommit
parseCommit remoteRepos = do
  commitStart
  hash <- parseGitCommitHash
  childrens <- option [] $ char ' ' *> (parseGitCommitHash `sepBy1` char ' ')
  refs <- option [] (char ' ' *> (parseReferences remoteRepos))
  endOfLine
  logFields <- parseLogFields
  message <- parseMessage
  return GitCommit {
                hash = hash
              , tree = getTree logFields
              , childrens = childrens
              , parents = getParents logFields
              , author = getAuthor logFields
              , committer = getCommitter logFields
              , gpgsig = getGpgSignature logFields
              , message = message
              , refs = refs
              }
  where
    getParents :: [LogField] -> [GitHash]
    getParents =
      mapMaybe getParentHash
      where
        getParentHash :: LogField -> Maybe GitHash
        getParentHash (LogFieldParent x) = Just x
        getParentHash _ = Nothing
    
    getTree :: [LogField] -> GitHash
    getTree =
      head . mapMaybe getTree'
      where
        getTree' :: LogField -> Maybe GitHash
        getTree' (LogFieldTree x) = Just x
        getTree' _ = Nothing

    getAuthor :: [LogField] -> OwnershipInfo
    getAuthor =
      head . mapMaybe getAuthor'
      where
        getAuthor' :: LogField -> Maybe OwnershipInfo
        getAuthor' (LogFieldAuthor x) = Just x
        getAuthor' _ = Nothing

    getCommitter :: [LogField] -> OwnershipInfo
    getCommitter =
      head . mapMaybe getCommitter'
      where
        getCommitter' :: LogField -> Maybe OwnershipInfo
        getCommitter' (LogFieldCommitter x) = Just x
        getCommitter' _ = Nothing

    getGpgSignature :: [LogField] -> Maybe GpgSignature
    getGpgSignature =
      listToMaybe . mapMaybe getGpgSignature'
      where
        getGpgSignature' :: LogField -> Maybe GpgSignature
        getGpgSignature' (LogFieldGpgSignature x) = Just x
        getGpgSignature' _ = Nothing


-- | Возвращает все комиты репозитория расположенного
-- в текущем каталоге в виде списка.
getLogCommits :: IO (Either String [GitCommit])
getLogCommits = do
  remoteRepos <- Prelude.reverse . sortOn (T.length . getRemoteRepoName)  <$> getRemoteRepos'
  withCreateProcess (shell "git log --format=raw --decorate=full --all --full-history --children --encoding=utf-8"){std_out = CreatePipe} $ \_ (Just outh) _ ph -> do
    --hSetEncoding outh utf8
    rawVersionInput <- C8.hGetContents outh
    exitCode <- waitForProcess ph
    case exitCode of
      ExitFailure _ -> gitError
      ExitSuccess -> return $ correctParseResult "Can not parse GIT log" $ parseOnly (parseCommitLog remoteRepos) rawVersionInput

-- | Возвращает все комиты репозитория расположенного
-- в текущем каталоге в виде мэпа.
getLogCommitsMap :: IO (Either String GitCommitsMap)
getLogCommitsMap =
  fmap (fromList . fmap (\x -> (hash x, x))) <$> getLogCommits

hashToCommit :: GitCommitsMap -> GitHash -> Maybe GitCommit
hashToCommit commitsMap commit = commit `lookup` commitsMap

hashToCommit':: GitCommitsMap -> GitHash -> GitCommit
hashToCommit' commitsMap commit =
  case hashToCommit commitsMap commit of
    Just x -> x
    _ -> error $ "Commit with hash " <> show commit <> " not found"

hashesToCommits :: GitCommitsMap -> [GitHash] -> [GitCommit]
hashesToCommits = fmap . hashToCommit'

toCommits :: GitCommitsMap -> [GitCommit]
toCommits = elems

toHashes :: GitCommitsMap -> [GitHash]
toHashes = keys

-- | Return top heads found in repository
getTopCommits :: GitCommitsMap -> [GitCommit]
getTopCommits =
  filter hasNoChilds . toCommits
  where
    hasNoChilds = null . childrens

getTags :: GitCommit -> [RefName]
getTags =
  mapMaybe maybeGitTag . refs
  where
    maybeGitTag :: GitReference -> Maybe RefName
    maybeGitTag (GitTag refName) = Just refName
    maybeGitTag _ = Nothing

getBranches :: GitCommit -> [RefName]
getBranches =
  mapMaybe maybeGitBranches . refs
  where
    maybeGitBranches :: GitReference -> Maybe RefName
    maybeGitBranches (GitBranch refName) = Just refName
    maybeGitBranches _ = Nothing

getRemoteBranches :: GitCommit -> [(RepoName, RefName)]
getRemoteBranches =
  mapMaybe maybeRemoteBranch . refs
  where
    maybeRemoteBranch :: GitReference -> Maybe (RepoName, RefName)
    maybeRemoteBranch (GitRemoteBranch repoName refName) = Just (repoName, refName)
    maybeRemoteBranch _ = Nothing


-- | Возвращает все подключённые удалённые репозитории
getRemoteRepos :: IO (Either String [RemoteRepo])
getRemoteRepos =
  withCreateProcess (shell "git remote"){std_out = CreatePipe} $ \_ (Just outh) _ ph -> do
    --hSetEncoding outh utf8
    rawVersionInput <- C8.hGetContents outh
    exitCode <- waitForProcess ph
    case exitCode of
      ExitFailure _ -> gitError
      ExitSuccess -> return $ correctParseResult "Can not parse GIT remotes" $ parseOnly parseRemoteRepos rawVersionInput

getRemoteRepos' :: IO [RemoteRepo]
getRemoteRepos' = do
  remoteRepos <- getRemoteRepos
  return $ case remoteRepos of
             Left e -> error e
             Right s -> s

parseRemoteRepos :: Parser [RemoteRepo]
parseRemoteRepos =
  --return $ Prelude.map RemoteRepo ["origin", "import/pcct_cms_adapter"]
  (parseRemoteRepo `sepBy1` endOfLine) -- <* endOfInput

parseRemoteRepo :: Parser RemoteRepo
parseRemoteRepo =
 RemoteRepo . pack <$> manyTill anyChar (lookAhead endOfLine)
