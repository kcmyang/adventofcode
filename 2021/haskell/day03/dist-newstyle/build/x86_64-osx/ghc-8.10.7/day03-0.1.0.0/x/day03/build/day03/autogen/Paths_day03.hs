{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_day03 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/marco/.cabal/bin"
libdir     = "/Users/marco/.cabal/lib/x86_64-osx-ghc-8.10.7/day03-0.1.0.0-inplace-day03"
dynlibdir  = "/Users/marco/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/marco/.cabal/share/x86_64-osx-ghc-8.10.7/day03-0.1.0.0"
libexecdir = "/Users/marco/.cabal/libexec/x86_64-osx-ghc-8.10.7/day03-0.1.0.0"
sysconfdir = "/Users/marco/.cabal/etc"

getBinDir     = catchIO (getEnv "day03_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "day03_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "day03_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "day03_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day03_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'