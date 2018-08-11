module Geometry.Path where

import Geometry.Intersection
import Image.Rendering
import Linear.Affine
import Linear.V3
import Linear.Vector

data Step a = Step
  { intersection :: {-# UNPACK #-} !(Intersection a)
  , emittance    :: {-# UNPACK #-} !(Point V3 a)
  , reflectance  :: {-# UNPACK #-} !(Point V3 a)
  }

data Path a
  = {-# UNPACK #-} !(Step a) :< !(Path a)
  | End


samplePath :: RealFloat a => Path a -> Sample a
samplePath (Step _ emittance reflectance :< rest)
  = let brdf = reflectance ^/ pi
        incoming = samplePath rest
    in  brdf `seq` incoming `seq` emittance + (brdf * incoming ^/ prob)
  where prob = recip (2 * pi)
samplePath End  = zero

{-# SPECIALIZE samplePath :: Path Float -> Sample Float #-}
{-# SPECIALIZE samplePath :: Path Double -> Sample Double #-}
