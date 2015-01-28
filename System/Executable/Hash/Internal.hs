{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internals related to reading and writing an injected executable
-- hash.
module System.Executable.Hash.Internal where

import           Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import           Data.FileEmbed (dummySpaceWith, injectWith)
import           System.Directory (doesFileExist)

-- | Yields a 'Just' value of a hash which has been injected into the
-- executable via 'injectExecutableHash'.
injectedExecutableHash :: Maybe BS.ByteString
injectedExecutableHash
    | BS.all (== toEnum (fromEnum '0')) bs = Nothing
    | otherwise = Just bs
  where
    bs = $(dummySpaceWith "executable-hash" 20)

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
    case injectWith "executable-hash" sha1 binary of
        Nothing -> fail "Impossible: dummy space too small for executable-hash."
        Just binary' -> do
            BS.writeFile fp binary'
            putStrLn $ "Successfully wrote " ++ fp ++ " with injected hash."

-- | Injects an executable hash into the specified binary.  If it
-- doesn't exist, then this prints a message to stdout indicating that
-- it failed to inject the hash.
maybeInjectExecutableHash :: FilePath -> IO ()
maybeInjectExecutableHash fp = do
    exists <- doesFileExist fp
    if exists
        then injectExecutableHash fp
        else putStrLn $ concat
            [ "Not injecting executable hash into "
            , fp
            , ", as it doesn't exist."
            ]
