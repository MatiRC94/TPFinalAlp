module Paths_TPFinalAlp (
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

bindir     = "/home/chile/.cabal/bin"
libdir     = "/home/chile/.cabal/lib/x86_64-linux-ghc-7.10.3/TPFinalAlp-0.1.0.0-6RCNeCETr1jHa7OB1xPX1u"
datadir    = "/home/chile/.cabal/share/x86_64-linux-ghc-7.10.3/TPFinalAlp-0.1.0.0"
libexecdir = "/home/chile/.cabal/libexec"
sysconfdir = "/home/chile/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TPFinalAlp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TPFinalAlp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TPFinalAlp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TPFinalAlp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TPFinalAlp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
