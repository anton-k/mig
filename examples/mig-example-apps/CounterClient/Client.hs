module Client
  ( runGet
  , runPut
  ) where

import Mig.Client
import Api

runGet :: ClientOr Int
runGet =
  getRespOrValue <$> fromClient counterClient.get

runPut :: Int -> ClientOr ()
runPut increment =
  getRespOrValue <$> fromClient counterClient.put increment

counterClient :: Routes Client
counterClient = Routes getClient putClient
  where
    getClient :| putClient = toClient (server counterClient)
