module Main where

import           App
import qualified Adapter.PostgreSQL.UserData as P

main :: IO ()
main = P.main
