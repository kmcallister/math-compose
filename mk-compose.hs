#!/usr/bin/env runhaskell

-- input compose.in on stdin
-- get   XCompose   on stdout

import Control.Applicative
import Data.List
import Data.Char
import Text.Printf

out :: String -> IO ()
out xs = case break (==":") $ words xs of
  (keys, [":",[char]]) -> do
    let keyss = ["<" ++ k ++ ">" | k <- keys]
    printf "<Multi_key> %s : \"%c\" U%04X\n"
      (intercalate " " keyss) char (fromEnum char)

main :: IO ()
main = do
  xs <- lines <$> getContents
  putStr "include \"%L\"\n\n"
  let keep xs | all isSpace xs = False
              | "#" `isPrefixOf` xs = False
      keep xs = True
  mapM_ out . filter keep $ xs
