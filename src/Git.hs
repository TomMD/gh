module Git where

import GitHub.Endpoints.PullRequests
import qualified GitHub as GH
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.FilePath

import Utils

gitAM :: FilePath -> BS.ByteString -> ExceptT Err IO ()
gitAM codedir patch =
 do let patchFile = (codedir </> "somepatchforthecode.patch")
    liftIO $ BS.writeFile patchFile patch
    safeProcess_ "git" ["-C", codedir, "am", patchFile] "" id

gitPushu:: FilePath -> ExceptT Err IO ()
gitPushu codedir =
 do safeProcess_ "git" ["-C", codedir, "push", "-u"] "" id

gitClone :: Maybe Auth -> String -> FilePath -> ExceptT Err IO ()
gitClone mtok url dir =
   do u <- mkTokenUrl mtok url
      safeProcess_ "git" ["-C", "/tmp", "clone", u, dir] "" cleanError
         `catchE` (\_ -> throwE $ "Git clone of repo '" ++ url ++ "' failed")
 where
  cleanError = const $ "Git clone of repo '" ++ url ++ "' failed"

gitremoteAdd :: FilePath -> Maybe Auth -> String -> String -> ExceptT Err IO ()
gitremoteAdd codedir mtok name remoteurl =
  do u <- mkTokenUrl mtok remoteurl
     safeProcess_ "git" ["-C", codedir, "remote", "add", name, u] "" cleanError
 where
  cleanError = const $ "Git remote add of repo '" ++ remoteurl ++ "' failed"

gitCheckout :: FilePath -> String -> ExceptT Err IO ()
gitCheckout codedir branch =
    safeProcess_ "git" ["-C", codedir, "checkout", branch] "" id

gitbranchNewTip :: FilePath -> String -> String -> ExceptT Err IO ()
gitbranchNewTip codedir branch commit =
    safeProcess_ "git" ["-C", codedir, "branch", "-f", branch, commit] "" id

getPatch :: GH.Auth -> SimpleRepo -> Id PullRequest -> IO ByteString
getPatch auth sr pr = either error pure =<< runExceptT (getPatchE auth sr pr)

getPatchE :: GH.Auth -> SimpleRepo -> Id PullRequest -> ExceptT Err IO ByteString
getPatchE jwtauth (SimpleRepo owner repo) issue =
  do res <- liftIO $ pullRequestPatch' (Just jwtauth) owner repo issue
     case res of
         Left e -> throwE ("Error getting diff patch: " <> show e)
         Right x -> pure (BSL.toStrict x)

mkTokenUrl :: Monad m => Maybe Auth -> String -> ExceptT Err m String
mkTokenUrl mtok url
    | take 8 url == "https://" = pure $ "https://" <> tok <> drop 8 url
    | otherwise = throwE "Invalid github git url, should be an https url."
  where
   tok = maybe "" (\(OAuth t) -> "x-access-token:" <> BC.unpack t <> "@") mtok
