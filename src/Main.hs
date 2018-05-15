{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad (void)
import Data.Foldable
import qualified Data.Text as T
import OpenSSL
import GitHub hiding (command)
import GitHub.Data.Name
import GitHub.Endpoints.Repos
import GitHub.Endpoints.PullRequests
import GHC.Exts as E
import System.FilePath
import System.IO.Temp

import Git
import Utils
import CLI

main :: IO ()
main = withOpenSSL $ do
  opts <- getOptions
  let auth = OAuth (githubToken opts)
  case (ghCommand opts) of
    Pull pr -> doPull (githubUser opts) auth pr
    Fork f  ->
       do eres <- executeRequest auth $ doFork f
          case eres of
            Right res -> putStrLn $ "New repository: " <> show (untagId (repoId res))
            Left err  -> error (show err)

doFork :: SimpleRepo -> Request 'RW Repo
doFork (SimpleRepo user proj) = forkExistingRepoR user proj Nothing

doPull :: GitHubUser -> Auth -> Pull -> IO ()
doPull _ghUser auth (PRInfo (SimpleRepo user project) issueid) =
 do evec <- executeRequest auth $ pullRequestCommentsR user project issueid FetchAll
    case evec of
        Left err -> error (show err)
        Right vec ->
            print vec -- XXX
doPull ghUser auth (PRMirror (PM mopt repo)) =
    case mopt of
        MirrorOne issueid -> void $ doMirror ghUser auth repo issueid
        MirrorAll         ->
               traverse_ (doMirror ghUser auth repo) =<< getOpenPulls repo
        MirrorAllOpen     ->
               traverse_ (doMirror ghUser auth repo) =<< getAllPulls repo

-- Mirror a pull request into the project under the given user name
doMirror :: GitHubUser -> Auth -> SimpleRepo -> Id PullRequest -> IO (Either String ())
doMirror ghUser auth repo@(SimpleRepo remoteU proj) pr = withSystemTempDirectory "gh" $ \tmpdir ->
 runExceptT $ do
  let codedir = tmpdir </> "code"
  -- 1. git clone
  -- 2. Create branches for source and destination
  -- 3. Get the git patch
  -- 4. Apply the git patch in a new branch name based on issue number
  -- 5. Push the new branch
  -- 6. Open the new pull request of this branch into the target (???) branch
  prObj <- either (throwE . show) pure =<< liftIO (pullRequest remoteU proj pr)
  let dstBranchName = "pr" ++ show (untagId pr) ++ "-dst"
      srcBranchName = "pr" ++ show (untagId pr) ++ "-src"
      dstCommit = T.unpack (pullRequestCommitSha (pullRequestBase prObj))
      originalBody = maybe "" id (pullRequestBody prObj) :: T.Text
  gitClone (Just auth) (httpsUrlOfRepo repo) codedir
  gitbranchNewTip codedir dstBranchName dstCommit
  gitbranchNewTip codedir srcBranchName dstCommit
  gitCheckout codedir dstBranchName
  gitPushu codedir
  gitCheckout codedir srcBranchName
  patchBytes <- liftIO $ getPatch auth repo pr
  gitAM codedir patchBytes
  gitremoteAdd codedir (Just auth) ghUser userGHUrl
  gitPushu codedir
  --  Now for step 6
  let title = "Mirror of " <> untagName remoteU <> "/" <> untagName proj <> "#" <> T.pack (show (untagId pr))
      cpr = CreatePullRequest title
                              (T.unlines [title, originalBody])
                              (T.pack srcBranchName)
                              (T.pack dstBranchName)
  _ <- either (throwE . show) pure =<< liftIO (createPullRequest auth (N $ fromString ghUser) proj cpr)
  pure ()
 where
 userGHUrl = "https://github.com/" <> ghUser <> "/" <> T.unpack (untagName proj)

getAllPulls :: SimpleRepo -> IO [Id PullRequest]
getAllPulls sr =
    map simplePullRequestId <$> getSimplePullRequest sr

getSimplePullRequest :: SimpleRepo -> IO [SimplePullRequest]
getSimplePullRequest (SimpleRepo u p) = E.toList <$> either (error . show) id <$> pullRequestsFor u p

getOpenPulls :: SimpleRepo -> IO [Id PullRequest]
getOpenPulls sr =
    map simplePullRequestId <$> filter ((StateOpen ==) . simplePullRequestState) <$> getSimplePullRequest sr

httpsUrlOfRepo :: SimpleRepo -> String
httpsUrlOfRepo (SimpleRepo u p) = "https://github.com/" <> T.unpack (untagName u) <> "/" <> T.unpack (untagName p)
