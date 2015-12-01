module Main where

import Web.VKHS2


main :: IO ()
main = do
  runLogin defaultState test_login
  return ()
