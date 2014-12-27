{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List

import API
import Types


main :: IO ()
main = return ()


go :: IO (Either String (Message String))
go = execTraduisons $ do
  let text =  "Ik heb een huge kont."
  l <- detectLanguage text
  translate (Language "en") (Message l text)


data Command = Help
             | SetLanguages String String
             | Translate String
             | SwapLanguages
             | Exit

parse :: String -> [Command]
parse ('/':s) = [SwapLanguages, Translate s]
parse "?" = [Help]
parse "" = [Exit]
parse s
  | "/" `isSuffixOf` s = [SwapLanguages, Translate (init s)]
  | otherwise = [Translate s]
