# executable-hash

Provides the SHA1 hash of the executable.  This hash can either be
injected into the executable as a step after compilation, or
calculated at runtime.

## Usage

The main function expected to be used by the user is
`System.Executable.Hash.executableHash`.  This function yields a SHA1
hash determined by the contents of the executable.  However, note that
this may not be the actual SHA1 of the executable, since the hash can
be injected into the executable (which changes its "actual" hash).

Installing this package will also install the `inject-executable-hash`
executable.  Running this program on a binary, like
`inject-executable-hash <binary-name>` will replace a dummy
`ByteString` (via the `file-embed` package) in the binary with its
hash.

Alternatively, you can put this in a `Setup.hs` file, and set
`build-type: Custom` in your `.cabal`:

```haskell
import Distribution.Simple
import System.Executable.Hash.Internal (maybeInjectExecutableHash)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = \_ _ _ _ ->
        maybeInjectExecutableHash "dist/build/path-to/your-executable"
    }
```

(Note: you'll need to change the executable path)
