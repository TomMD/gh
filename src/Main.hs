{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE DataKinds         #-}
module Main where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad (void)
import Data.Foldable
import qualified Data.Text as T
import OpenSSL
import GitHub hiding (command)
import GitHub.Data.Name
import GitHub.Data.Id
import GitHub.Endpoints.Repos
import GitHub.Endpoints.PullRequests
import GitHub.Internal.Prelude (Vector)
import qualified GitHub.Endpoints.Issues.Comments as Issues
import GHC.Exts as E
import System.FilePath
import System.IO.Temp
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Git
import Utils
import CLI

main :: IO ()
main = withOpenSSL $ do
  opts <- getOptions
  let auth = OAuth (githubToken opts)
  case ghCommand opts of
    WebHook wh -> doWebHook auth wh
    Pull pr -> doPull (githubUser opts) auth pr
    Fork f  ->
       do eres <- executeRequest auth $ doFork f
          case eres of
            Right res -> putStrLn $ "New repository: " <> show (untagId (repoId res))
            Left err  -> error (show err)

doFork :: SimpleRepo -> Request 'RW Repo
doFork (SimpleRepo user proj) = forkExistingRepoR user proj Nothing

doWebHook :: Auth -> WebHook -> IO ()
doWebHook auth (WHAdd (SimpleRepo user project) nrwh) =
  do e <- executeRequest auth $ createRepoWebhookR user project nrwh
     case e of
        Left err -> die err
        Right _  -> pure ()
doWebHook auth (WHRm (SimpleRepo user project)) =
  do ehs <- executeRequest auth $ webhooksForR user project 1000
     let del :: RepoWebhook -> Request 'RW ()
         del = deleteRepoWebhookR user project . repoWebhookId
     case ehs of
        Left e   -> die e
        Right hs -> for_ hs (void . executeRequest auth . del)

doPull :: GitHubUser -> Auth -> Pull -> IO ()
doPull _ghUser _auth (PRInfo (SimpleRepo user project) issueid) =
 do evec <- Issues.comments user project (Id issueid)
    case evec of
        Left err -> error (show err)
        Right vec ->
         do putStrLn $ T.unpack (untagName user) <> "/" <> T.unpack (untagName project) <> ":" <> show issueid
            traverse_ pPrintComment vec
doPull ghUser auth (PRMirror (PM mopt repo)) =
 do r <- case mopt of
            MirrorOne issueid -> doMirror ghUser auth repo [issueid]
            MirrorAll         ->
                   doMirror ghUser auth repo =<< getAllPulls repo
            MirrorAllOpen     ->
                  doMirror ghUser auth repo =<< getOpenPulls repo
    either error pure r
doPull _ghUser auth (PRClose (SimpleRepo user project) issueid) =
  do let epr = EditPullRequest Nothing Nothing (Just StateClosed) Nothing Nothing
     _ <- either die (const (pure ())) =<< updatePullRequest auth user project (Id issueid) epr
     pure ()

-- Mirror a pull request into the project under the given user name
doMirror :: GitHubUser -> Auth -> SimpleRepo -> Vector PullRequestNumber -> IO (Either String ())
doMirror ghUser auth repo@(SimpleRepo remoteU proj) prs = withSystemTempDirectory "gh" $ \tmpdir ->
 runExceptT $ do
  let codedir = tmpdir </> "code"
      upstreamRemote = ghUser
      tryE f = catchE f (\_ -> pure ())
  -- 1. git clone
  -- 2. Create branches for source and destination
  -- 3. Get the git patch
  -- 4. Apply the git patch in a new branch name based on issue number
  -- 5. Push the new branch
  -- 6. Open the new pull request of this branch into the target (???) branch
  gitClone (Just auth) (httpsUrlOfRepo repo) codedir
  tryE $ gitremoteAdd codedir (Just auth) upstreamRemote userGHUrl
  let handlePR = \pr ->
        do prObj <- either (throwE . show) pure =<< liftIO (pullRequest' (Just auth) remoteU proj (Id pr))
           let dstBranchName = "pr" ++ show pr ++ "-dst"
               srcBranchName = "pr" ++ show pr ++ "-src"
               dstCommit = T.unpack (pullRequestCommitSha (pullRequestBase prObj))
               originalBody = maybe "" filterNotifications (pullRequestBody prObj) :: T.Text
           gitbranchNewTip codedir dstBranchName dstCommit
           gitbranchNewTip codedir srcBranchName dstCommit
           gitCheckout codedir dstBranchName
           tryE $ gitPushu codedir upstreamRemote dstBranchName
           gitCheckout codedir srcBranchName
           patchBytes <- liftIO $ getPatch auth repo (Id pr)
           catchE (gitAM codedir patchBytes) (\e -> gitAMabort codedir >> throwE ("'git am' failed. Patch likely does not apply cleanly: " ++ show e))
           tryE $ gitPushu codedir upstreamRemote srcBranchName
           --  Now for step 6
           let title = "Mirror of " <> untagName remoteU <> " " <> untagName proj <> "#" <> T.pack (show pr)
               cpr = CreatePullRequest title
                                       (T.unlines [title, originalBody])
                                       (T.pack srcBranchName)
                                       (T.pack dstBranchName)
           prRes <- either (throwE . show) pure =<< liftIO (createPullRequest auth (N $ fromString ghUser) proj cpr)
           liftIO $ putStrLn (show (pullRequestNumber prRes))
           pure ()
  for_ prs $ \pr ->
    catchE (handlePR pr)
           (\r -> liftIO $ putStrLn $ "--- --- ---\nFailed to mirror pull request: " <> show pr <> "\n" <> r)
 where
 userGHUrl = "https://github.com/" <> ghUser <> "/" <> T.unpack (untagName proj)

filterNotifications :: T.Text -> T.Text
filterNotifications x = T.replace "@" "<at>" x

getAllPulls :: SimpleRepo -> IO (Vector PullRequestNumber)
getAllPulls sr =
    fmap simplePullRequestNumber <$> getSimplePullRequest sr

getSimplePullRequest :: SimpleRepo -> IO (Vector SimplePullRequest)
getSimplePullRequest (SimpleRepo u p) = either (error . show) id <$> pullRequestsFor u p
-- XXX need a get all pull requeset command, this doesn't do that.

getOpenPulls :: SimpleRepo -> IO (Vector PullRequestNumber)
getOpenPulls sr =
    fmap simplePullRequestNumber <$> getSimplePullRequest sr

httpsUrlOfRepo :: SimpleRepo -> String
httpsUrlOfRepo (SimpleRepo u p) = "https://github.com/" <> T.unpack (untagName u) <> "/" <> T.unpack (untagName p)

pPrintComment :: IssueComment -> IO ()
pPrintComment ic =
    putStrLn $ unlines [ "----- ----- ----- ----- -----"
                       , ut (simpleUserLogin (issueCommentUser ic)) <> ":"
                       , T.unpack (issueCommentBody ic)
                       ]
 where
 ut = T.unpack . untagName

die :: Show a => a -> IO x
die e = hPutStrLn stderr (show e) >> exitFailure
