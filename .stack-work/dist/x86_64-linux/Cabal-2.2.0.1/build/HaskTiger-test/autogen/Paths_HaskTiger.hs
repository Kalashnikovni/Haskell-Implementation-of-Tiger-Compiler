{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskTiger (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/bin"
libdir     = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/lib/x86_64-linux-ghc-8.4.3/HaskTiger-0.1.0.0-93PuZDn01OgLIVAluCQCpr-HaskTiger-test"
dynlibdir  = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/share/x86_64-linux-ghc-8.4.3/HaskTiger-0.1.0.0"
libexecdir = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/libexec/x86_64-linux-ghc-8.4.3/HaskTiger-0.1.0.0"
sysconfdir = "/home/kalashnikov/Materias/2017/Compiladores/Haskell-Implementation-of-Tiger-Compiler/.stack-work/install/x86_64-linux/3f80d55a2733212adbd64489eb7e6c5402aba02024c499297820d2ef45287ae6/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskTiger_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskTiger_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskTiger_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskTiger_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskTiger_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskTiger_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
