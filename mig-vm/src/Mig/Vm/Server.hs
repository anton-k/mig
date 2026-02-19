module Mig.Vm.Server where

import Mig.Vm.Types

renderServer :: Server m -> m Ops
renderServer = undefined

data VmOptions = VmOptions
  { port :: Int
  }

vmEval :: VmOptions -> Ops -> IO ()
vmEval = undefined

runServer :: VmOptions -> Server IO -> IO ()
runServer opts server = do
  vmEval opts =<< renderServer server
  
