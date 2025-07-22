module RuntimeCheckUseCases.PostRtc where

import Numeric.Natural (Natural)

subtractFromGreater :: Natural -> Natural -> Natural
subtractFromGreater x y = x - y

headOfNonEmpty :: [Natural] -> Natural
headOfNonEmpty (x : _) = x

