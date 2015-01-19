import Distribution.Simple
import System.Executable.Hash.Internal (injectExecutableHash)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = \_ _ _ _ ->
        injectExecutableHash "dist/build/test-inject/test-inject"
    }
