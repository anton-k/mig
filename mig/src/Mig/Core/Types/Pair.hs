-- | Pair with infix constructor.
module Mig.Core.Types.Pair (
  (:|) (..),
) where

{-| Infix synonym for pair. It can be useful to stack together
many client functions in the output of @toClient@ function.
-}
data (:|) a b = a :| b
