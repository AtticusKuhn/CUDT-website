{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cudt (
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/bin"
libdir     = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/lib/x86_64-linux-ghc-8.10.7/cudt-1.0.0.0-Gg15fQGTn5Hvu7F4rooVE-cudt-exe"
dynlibdir  = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/share/x86_64-linux-ghc-8.10.7/cudt-1.0.0.0"
libexecdir = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/libexec/x86_64-linux-ghc-8.10.7/cudt-1.0.0.0"
sysconfdir = "/home/atticusk/coding/cudt/.stack-work/install/x86_64-linux-nix/9478c0f56a81b28c9b76eb7856a5ecb882c715530d2da96a0d48d11005a2740f/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cudt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cudt_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cudt_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cudt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cudt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cudt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
