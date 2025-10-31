module RuntimeCheckAutoImport (RuntimeCheckAutoImport.simpleFun) where

import Numeric.Natural (Natural)

import RuntimeCheckAutoImport.PostRtc

simpleFun :: Natural -> Natural
simpleFun x
  | x > 0 = RuntimeCheckAutoImport.PostRtc.simpleFun x
  | otherwise = error "Runtime check failed: x > 0"

