{-# LANGUAGE OverloadedStrings #-}
module CLI (getOptions) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.String
import qualified Data.Attoparsec.Text as A
import Data.Semigroup ((<>))
import Options.Applicative as OP
import GitHub.Data.Name
import System.Environment


import Utils

defaultApiToken :: String
defaultApiToken = "GH_GITHUB_API_TOKEN"

options :: Parser Options
options =
   Opts <$> option (eitherReader eitherReadHex)
                ( long "auth-token"
                <> short 'a'
                <> metavar "HEX"
                <> value ""
                <> help "Hex github authentication token (defaults to env 'GH_GITHUB_API_TOKEN')"
                )
        <*> strOption
                ( long "username"
                <> short 'u'
                <> value ""
                <> help "Github user name (defaults to current user name, env 'USER')"
                )
        <*> hsubparser ( OP.command "fork" (info (Fork <$> parseRepo) (progDesc forkDesc))
                      <> OP.command "pull" (info (Pull <$> parsePull) (progDesc pullDesc))
                       )
  where
  forkDesc = "Fork a repository"
  pullDesc = "Create, query, or mirror a pull request"
  eitherReadHex s =
    case B16.decode (fromString s) of
        (m,_) | B.length m > 0 -> Right m
        _ -> Left $ "Invalid authentication token: " ++ show s

parseRepo :: Parser SimpleRepo
parseRepo =
    argument (eitherReader (A.parseOnly pRepo . T.pack))
               ( metavar "<username/repo>"
              <> help "Specify the repositry as 'username/repo' such as 'gh fork glguy/irc-core'"
               )
 where
 pRepo :: A.Parser SimpleRepo
 pRepo = do
    user <- A.takeWhile (/= '/')
    _ <- A.char '/'
    repo <- A.takeText
    pure (SimpleRepo (N user) (N repo))

parsePull :: Parser Pull
parsePull =
    hsubparser ( OP.command "get"    (info (uncurry PRInfo <$> parsePR) (progDesc prGetDesc))
              <> OP.command "mirror" (info (PRMirror <$> parseMirror) (progDesc prMirrorDesc))
               )
 where
 prGetDesc = "Get the latest comment from an existing pull request"
 prMirrorDesc = "Mirror one or more pull requests from a repository into an identically named repo under your user."

parsePR :: Parser (SimpleRepo,PullRequestNumber)
parsePR  =
    argument (eitherReader (A.parseOnly pPR . T.pack))
                ( metavar "<username/repo:pullRequestNumber>"
               <> help "Specify the pull request, ex 'glguy/irc-core:6'"
                )
  where
  pPR :: A.Parser (SimpleRepo,PullRequestNumber)
  pPR = do
    user <- A.takeWhile (/= '/')
    _ <- A.char '/'
    repo <- A.takeWhile (/= ':')
    _ <- A.char ':'
    number <- A.decimal
    pure (SimpleRepo (N user) (N repo), number)

parseMirror :: Parser PullMirror
parseMirror = PM <$> parseMirrorOptions <*> parseRepo
 where
 parseMirrorOptions :: Parser MirrorOptions
 parseMirrorOptions =
    argument (eitherReader (A.parseOnly readMirror . T.pack))
                          (metavar "<open|all|number>"
                         <> help "Mirror open pull requests, all pull requests, or a specific one.")
 readMirror :: A.Parser MirrorOptions
 readMirror
    =   (A.string "open" >>= \_ -> pure MirrorAllOpen)
    <|> (A.string "all" >>= \_ -> pure MirrorAll)
    <|> (MirrorOne <$> A.decimal)

getOptions :: IO Options
getOptions =
  do let runParser = customExecParser optPrefs
         optPrefs  = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
         opts      = info (options <**> helper) fullDesc
         reprTok obj val  | githubToken obj == mempty = obj { githubToken = val }
                          | otherwise = obj
         reprUser obj val | githubUser obj == mempty = obj { githubUser = val }
                          | otherwise = obj
     o <- runParser opts
     envTok  <- maybe "" BC.pack <$> lookupEnv defaultApiToken
     envUser <- maybe "" id <$> lookupEnv "USER"
     pure $ reprTok (reprUser o envUser) envTok

