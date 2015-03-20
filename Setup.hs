import Distribution.Simple
import System.Executable.Hash.Internal (maybeInjectExecutableHash)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = \_ _ _ _ ->
        maybeInjectExecutableHash "dist/build/test-inject/test-inject"
    }
