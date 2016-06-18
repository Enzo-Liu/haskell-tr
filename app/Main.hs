-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import           System.Environment (getArgs)
import           System.Exit

import           Tr

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Nothing -> putStrLn "Usage: tr [-d] charSet1 [charSet2]" >> exitWith (ExitFailure 1)
    Just (is, mos) -> interact (tr is mos)

parseArgs :: [String] -> Maybe (CharSet , Maybe CharSet)
parseArgs ["-d", cs1] = Just (cs1, Nothing)
parseArgs [cs1, cs2] = Just (cs1, Just cs2)
parseArgs _ = Nothing
