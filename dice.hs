{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Options.Applicative

import Control.Monad (replicateM_)
import Data.Semigroup ((<>))
import System.Random (newStdGen, RandomGen, randomRs)
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

tossDice :: (RandomGen g) => Int -> Int -> g -> [Int]
tossDice x y = take x . randomRs (1, y)

main :: IO ()
main = do
  args@Args{..} <- execParser opts
  putStrLn $ show args
  replicateM_ times $ do
    r <- tossDice n m <$> newStdGen
    putStrLn $ "Result: " ++ show (sum r) ++ "\tDice: " ++ show r
