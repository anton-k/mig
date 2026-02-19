module Mig.Vm.Server where

import Mig.Vm.Types
import Mig.Vm.Render

data VmOptions = VmOptions
  { port :: Int
  }

vmEval :: VmOptions -> Ops -> Ctx -> IO ()
vmEval = undefined

runServer :: VmOptions -> Server IO -> IO ()
runServer opts server = do
  uncurry (vmEval opts) =<< renderServer server
  
