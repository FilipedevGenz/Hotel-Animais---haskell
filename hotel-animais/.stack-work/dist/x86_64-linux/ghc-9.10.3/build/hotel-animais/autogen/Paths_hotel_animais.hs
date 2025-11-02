{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hotel_animais (
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
bindir     = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/bin"
libdir     = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/lib/x86_64-linux-ghc-9.10.3-b4c3/hotel-animais-0.1.0.0-1E3NpSAe5XC8Pvs43hD89p-hotel-animais"
dynlibdir  = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/lib/x86_64-linux-ghc-9.10.3-b4c3"
datadir    = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/share/x86_64-linux-ghc-9.10.3-b4c3/hotel-animais-0.1.0.0"
libexecdir = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/libexec/x86_64-linux-ghc-9.10.3-b4c3/hotel-animais-0.1.0.0"
sysconfdir = "/home/lipe/Documentos/GitHub/Hotel-Animais---haskell/hotel-animais/.stack-work/install/x86_64-linux/df23554419e7ab88ba795304fc39bc44a39aed6283ecb0e3b5663ba56613e527/9.10.3/etc"

getBinDir     = catchIO (getEnv "hotel_animais_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hotel_animais_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hotel_animais_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hotel_animais_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hotel_animais_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hotel_animais_sysconfdir") (\_ -> return sysconfdir)



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
