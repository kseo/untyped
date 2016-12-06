module Main where

import Language.LambdaCalculus

import System.Console.Haskeline

loop :: InputT IO ()
loop = do
  minput <- getInputLine "% "
  case minput of
    Nothing -> return ()
    Just input -> case parseLC input of
      Left _ -> do
        outputStrLn $ "Invalid lambda caluclus: " ++ input
        loop
      Right p -> do
        let term = eval [] p
        outputStrLn $ printTm [] term
        loop

main :: IO ()
main = runInputT defaultSettings loop
