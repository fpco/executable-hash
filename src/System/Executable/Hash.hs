{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functions for accessing or computing a SHA1
-- hash of the program's executable.  Most users are expected to use
-- the 'executableHash' function.
--
-- To inject the hash into the executable, you can use the
-- @inject-executable-hash@ program installed along with this package.
-- Alternatively, to do this automatically with cabal, place this in
-- your @Setup.hs@:
--
-- @
--      import Distribution.Simple
--      import System.Executable.Hash.Internal (maybeInjectExecutableHash)
--
--      main :: IO ()
--      main = defaultMainWithHooks $ simpleUserHooks
--          { postBuild = \_ _ _ _ ->
--              maybeInjectExecutableHash "dist\/build\/path-to\/your-executable"
--          }
-- @
--
-- (Note: you'll need to change the executable path)
module System.Executable.Hash
    ( executableHash
    , computeExecutableHash
    ) where

import           Control.Applicative ((<$>))
import           Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import           System.Environment.Executable (getScriptPath, ScriptPath(..))
import           System.Executable.Hash.Internal

-- | If a SHA1 hash of the executable has been injected into it, then
-- it's directly yielded by this function.  Otherwise, a hash is
-- computed with 'computeExecutableHash'.
--
-- Note that you shouldn't rely on the result being the actual SHA1
-- hash of the executable, because injecting the hash modifies the
-- binary, and so changes the result of 'computeExecutableHash'.
-- Instead, this should only be used as a way to uniquely identify the
-- contents of the executable.
--
-- This yields 'Nothing' when run with @runhaskell@ or @ghci@.
executableHash :: IO (Maybe BS.ByteString)
executableHash =
    case injectedExecutableHash of
        Just x -> return (Just x)
        Nothing -> computeExecutableHash

-- | Computes the SHA1 hash of the program executable.
--
-- This yields 'Nothing' when run with @runhaskell@ or @ghci@.
computeExecutableHash :: IO (Maybe BS.ByteString)
computeExecutableHash = do
    sp <- getScriptPath
    case sp of
        Executable fp -> Just . hash <$> BS.readFile fp
        _ -> return Nothing
