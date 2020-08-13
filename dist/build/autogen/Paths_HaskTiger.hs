module Paths_HaskTiger (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kalashnikov/.cabal/bin"
libdir     = "/home/kalashnikov/.cabal/lib/x86_64-linux-ghc-7.10.3/HaskTiger-0.1.0.0-GKAMJMwhHtbCyY7c0GBxTN"
datadir    = "/home/kalashnikov/.cabal/share/x86_64-linux-ghc-7.10.3/HaskTiger-0.1.0.0"
libexecdir = "/home/kalashnikov/.cabal/libexec"
sysconfdir = "/home/kalashnikov/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskTiger_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskTiger_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskTiger_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskTiger_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskTiger_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
