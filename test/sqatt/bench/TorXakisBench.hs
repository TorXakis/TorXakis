{-# OPTIONS_GHC -Wno-all #-}
-- TODO: remove the above after finishing this task.
import           Criterion.Main
import           Sqatt

main :: IO ()
main = defaultMain [
    bgroup "fast benchmarks" [ bench "print hello world" $ nfIO $ print "Hello world!"
                             ]
    ]
