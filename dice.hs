module Main where
import System.Random
import System.Environment
import Control.Monad

handleArgs :: [String] -> [Int]
handleArgs [x, y] = handleArgs [x, y, "1"]
handleArgs [x, y, z] = map read [x, y, z]
handleArgs _ = error "Too few (or many!) arguments."

tossDice :: (RandomGen g) => Int -> Int -> g -> [Int]
tossDice x y = take x . randomRs (1, y)

main :: IO ()
main = do
  [x, y, z] <- liftM handleArgs getArgs
  replicateM_ z $ do
    r <- liftM (tossDice x y) newStdGen
    putStrLn $ "Result: " ++ show (sum r) ++ "\tDice: " ++ show r
