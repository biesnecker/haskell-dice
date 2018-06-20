{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad.Random
import Options.Applicative

import Control.Monad (forM_, replicateM)
import Data.Semigroup ((<>))
import System.Environment (getArgs)

data Args = Args
  { n :: Int
  , m :: Int
  , times :: Int
  }

instance Show Args where
  show Args{..} = unwords $
    [ "Rolling"
    , show n ++ "d" ++ show m
    , show times
    , "time(s)"
    ]

diceArgs :: Parser Args
diceArgs = Args
  <$> argument auto (metavar "N" <> value 1)
  <*> argument auto (metavar "M" <> value 6)
  <*> argument auto (metavar "TIMES" <> value 1)

opts :: ParserInfo Args
opts = info (diceArgs <**> helper)
  (  fullDesc
  <> progDesc "Roll NdM dice TIMES times"
  <> header "dice"
  )

tossDice :: MonadRandom m => Int -> Int -> m [Int]
tossDice n m = replicateM n $ getRandomR (1, m)

tossDiceMultiple :: MonadRandom m => Args -> m [[Int]]
tossDiceMultiple Args{..} = replicateM times $ tossDice n m

main :: IO ()
main = do
  args <- execParser opts
  putStrLn $ show args
  g <- newStdGen
  let rs = evalRand (tossDiceMultiple args) g
  forM_ rs $ \r ->
    putStrLn $ "Result: " ++ show (sum r) ++ "\tDice: " ++ show r
