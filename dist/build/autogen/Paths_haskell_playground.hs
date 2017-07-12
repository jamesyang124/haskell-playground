{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_playground (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/akaing/Library/Haskell/bin"
libdir     = "/Users/akaing/Library/Haskell/ghc-8.0.1-x86_64/lib/haskell-playground-0.1.0.0"
datadir    = "/Users/akaing/Library/Haskell/share/ghc-8.0.1-x86_64/haskell-playground-0.1.0.0"
libexecdir = "/Users/akaing/Library/Haskell/libexec"
sysconfdir = "/Users/akaing/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_playground_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_playground_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_playground_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_playground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_playground_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
