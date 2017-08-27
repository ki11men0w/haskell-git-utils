{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE OverloadedStrings #-}
module GitCommits
    ( Text
    , RepoName
    , RefName
    , GitHash
    , GitReference(..)
    , GitCommit(..)
    , GitVersion(..)
    , GitCommitsMap
    , gitError
    , getGitVersion
    , getLogCommits
    , getLogCommitsMap
    , hashToCommit
    , hashToCommit'
    , hashesToCommits
    , toCommits
    , toHashes
    ) where

import Prelude hiding (takeWhile, take, lookup, hGetContents)
import System.Process
import Control.Monad (void)
import Data.Maybe (mapMaybe, maybe)
import Data.Monoid ((<>))
import Data.List (intercalate)
import System.Exit (ExitCode(..))
import Data.Map (Map(..), fromList, elems, keys, lookup)
import Control.Applicative (many, (<|>))
import Data.Attoparsec.Combinator

import Data.ByteString.Char8 as T hiding (map, count, head)
import Data.Attoparsec.ByteString.Char8

import Data.Time.Clock.POSIX
import Data.Time.Clock


type Text = ByteString

data GitVersion = GitVersion {major :: Int, minor :: Int} deriving (Eq, Ord)

instance Show GitVersion where
  show v = show (major v) <> "." <> show (minor v)


type RepoName = Text
type RefName = Text
type GitHash = Text
type UserName = Text
type UserEmail = Text


data GitReference = GitRef Text (Maybe GitReference) | GitTag RefName | GitBranch RefName | GitRemoteBranch RepoName RefName
                  deriving (Show)

data OwnershipInfo = OwnershipInfo {userName :: UserName, userEmail :: UserEmail, utcTime :: UTCTime}
  deriving (Show)

data GitCommit = GitCommit {
    hash :: GitHash
  , childrens :: [GitHash]
  , parents :: [GitHash]
  , author :: OwnershipInfo
  , committer :: OwnershipInfo
  , message :: Maybe Text
  , refs :: [GitReference]
  } deriving (Show)


type GitCommitsMap = Map GitHash GitCommit

data LogField = LogFieldParent GitHash
              | LogFieldAuthor OwnershipInfo
              | LogFieldCommitter OwnershipInfo
              | LogFieldUnused
                deriving (Show)


gitError = error "Error running git process"

correctParseResult :: String -> Either String a -> Either String a
correctParseResult errorMsg parseResult =
  case parseResult of
    Left orig -> Left $ errorMsg <> ":\n" <> orig
    _ -> parseResult

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
        rawVersionInput <- hGetContents outh
        return $ correctParseResult "Can not realize GIT version" $ parseOnly parseVersion rawVersionInput

parseToken :: Parser Text
parseToken = takeWhile1 (\c -> notInClass "),\n\r" c && (not . isSpace) c)

parseGitCommitHash :: Parser GitHash
parseGitCommitHash = pack <$> count 40 (satisfy (inClass "0-9a-f"))

parseCommitLog :: Parser [GitCommit]
parseCommitLog =
  (parseCommit `sepBy1` endOfLine) <* endOfInput


parseGitRef :: Parser GitReference
parseGitRef = do
  name <- parseToken
  ref <- option Nothing $ lookAhead $ string " -> " *> (Just <$> parseReference)
  return $ GitRef name ref

parseGitBranch :: Parser GitReference
parseGitBranch = do
  string "refs/heads/"
  name <- parseToken
  return $ GitBranch name

parseGitRemoteBranch :: Parser GitReference
parseGitRemoteBranch = do
  string "refs/remotes/"
  repo <- pack <$> many1 (notChar '/')
  char '/'
  name <- parseToken
  return $ GitRemoteBranch repo name

parseGitTag :: Parser GitReference
parseGitTag = GitTag <$> (string "tag: " *> parseToken)

parseReference :: Parser GitReference
parseReference =
  parseGitBranch <|> parseGitRemoteBranch <|> parseGitTag <|> parseGitRef

parseReferences :: Parser [GitReference]
parseReferences = do
  char '('
  result <- parseReference `sepBy1` (string ", " <|> string " -> ")
  char ')'
  return result

parseParent :: Parser LogField
parseParent = string "parent " *> (LogFieldParent <$> parseGitCommitHash) <* endOfLine

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

skipField :: Text -> Parser LogField
skipField field = string field *> char ' ' *> skipRestOfLine *> pure LogFieldUnused

parseLogFields :: Parser [LogField]
parseLogFields =
  many $ choice $ [parseParent, parseAuthor, parseCommitter] ++ map skipField ["tree"]


commitStart :: Parser ()
commitStart = void $ string "commit "

parseMessage :: Parser (Maybe Text)
parseMessage =
  option Nothing $ endOfLine *> (Just . T.intercalate "\n" <$> oneLine `sepBy` endOfLine) <* endOfLine
  where
    padding = string "    " *> return ()

    anyMessageEndsWithIt = endOfLine *> (endOfInput <|> padding <|> (endOfLine *> commitStart))
    oneLine :: Parser Text
    oneLine = padding *> (pack <$> manyTill anyChar (lookAhead anyMessageEndsWithIt))

parseCommit :: Parser GitCommit
parseCommit = do
  commitStart
  hash <- parseGitCommitHash
  childrens <- option [] $ char ' ' *> (parseGitCommitHash `sepBy1` char ' ')
  refs <- option [] (char ' ' *> parseReferences)
  endOfLine
  logFields <- parseLogFields
  message <- parseMessage
  return GitCommit {
                hash = hash
              , childrens = childrens
              , parents = getParents logFields
              , author = getAuthor logFields
              , committer = getCommitter logFields
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


-- | Возвращает все комиты репозитория расположенного
-- в текущем каталоге в виде списка.
getLogCommits :: IO (Either String [GitCommit])
getLogCommits =
  withCreateProcess (shell "git log --format=raw --decorate=full --all --full-history --children --encoding=utf-8"){std_out = CreatePipe} $ \_ (Just outh) _ ph -> do
    --hSetEncoding outh utf8
    rawVersionInput <- hGetContents outh
    exitCode <- waitForProcess ph
    case exitCode of
      ExitFailure _ -> gitError
      ExitSuccess -> return $ correctParseResult "Can not parse GIT log" $ parseOnly parseCommitLog rawVersionInput

-- | Возвращает все комиты репозитория расположенного
-- в текущем каталоге в виде мэпа.
getLogCommitsMap :: IO (Either String GitCommitsMap)
getLogCommitsMap =
  fmap (fromList . map (\x -> (hash x, x))) <$> getLogCommits

hashToCommit :: GitCommitsMap -> GitHash -> Maybe GitCommit
hashToCommit commitsMap commit = commit `lookup` commitsMap

hashToCommit':: GitCommitsMap -> GitHash -> GitCommit
hashToCommit' commitsMap commit =
  case hashToCommit commitsMap commit of
    Just x -> x
    _ -> error $ "Commit with hash " <> show commit <> " not found"

hashesToCommits :: GitCommitsMap -> [GitHash] -> [GitCommit]
hashesToCommits = map . hashToCommit'

toCommits :: GitCommitsMap -> [GitCommit]
toCommits = elems

toHashes :: GitCommitsMap -> [GitHash]
toHashes = keys

