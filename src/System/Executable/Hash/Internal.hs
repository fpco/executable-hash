{-# LANGUAGE TemplateHaskell #-}

-- | Internals related to reading and writing an injected executable
-- hash.
module System.Executable.Hash.Internal where

import           Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import           Data.FileEmbed (dummySpace, inject)

-- | Yields a 'Just' value of a hash which has been injected into the
-- executable via 'injectExecutableHash'.
injectedExecutableHash :: Maybe BS.ByteString
injectedExecutableHash
    | BS.all (== toEnum (fromEnum '0')) bs = Nothing
    | otherwise = Just bs
  where
    bs = $(dummySpace 20)

-- | Given the path to an executable, computes its hash and injects it
-- into the binary, such that when that program demands the value of
-- 'injectedExecutableHash', it yields a 'Just' value.
--
-- See the documentation in "System.Executable.Hash" for an example of
-- how to use this with a cabal @postBuild@ hook
injectExecutableHash :: FilePath -> IO ()
injectExecutableHash fp = do
    binary <- BS.readFile fp
    let sha1 = hash binary
    case inject sha1 binary of
        Nothing -> fail "Impossible: dummy space too small for executable-hash."
        Just binary' -> do
            BS.writeFile fp binary'
            putStrLn "Successfully wrote binary with injected hash."
