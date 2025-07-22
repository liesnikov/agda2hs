module RuntimeCheckUseCases (RuntimeCheckUseCases.subtractFromGreater, RuntimeCheckUseCases.headOfNonEmpty) where

import Numeric.Natural (Natural)

import RuntimeCheckUseCases.PostRtc

subtractFromGreater :: Natural -> Natural -> Natural
subtractFromGreater x y
  | not (x < y) =
    RuntimeCheckUseCases.PostRtc.subtractFromGreater x y
  | otherwise = error "Runtime check failed: not (x < y)"

headOfNonEmpty :: [Natural] -> Natural
headOfNonEmpty xs
  | case xs of
        [] -> False
        _ : _ -> True
    = RuntimeCheckUseCases.PostRtc.headOfNonEmpty xs
  | otherwise =
    error
      "Runtime check failed: case xs of\n    [] -> False\n    _ : _ -> True"

