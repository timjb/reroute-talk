{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module HVect where

-- Teil eins: Heterogene Vektoren a.k.a. verallgemeinerte Tupel

data HVect (ts :: [*]) where
  HNil :: HVect '[]
  HCons :: t -> HVect ts -> HVect (t ': ts)

type family Append (as :: [*]) (bs :: [*]) :: [*] where
  Append '[] bs = bs
  Append (a ': as) bs = a ': (Append as bs)

hVectAppend :: HVect as -> HVect bs -> HVect (Append as bs)
hVectAppend HNil bs = bs
hVectAppend (HCons a as) bs = HCons a (hVectAppend as bs)

type family HVectElim (ts :: [*]) (a :: *) :: * where
  HVectElim '[] a = a
  HVectElim (t ': ts) a = t -> HVectElim ts a

hVectUncurry :: HVectElim ts a -> HVect ts -> a
hVectUncurry f HNil = f
hVectUncurry f (HCons x xs) = hVectUncurry (f x) xs

data Rep (ts :: [*]) where
  RNil :: Rep '[]
  RCons :: Rep ts -> Rep (t ': ts)

hVectCurryExpl :: Rep ts -> (HVect ts -> a) -> HVectElim ts a
hVectCurryExpl RNil f = f HNil
hVectCurryExpl (RCons r) f = \x -> hVectCurryExpl r (f . HCons x)

class HasRep (ts :: [*]) where
  hasRep :: Rep ts

instance HasRep '[] where
  hasRep = RNil

instance HasRep ts => HasRep (t ': ts) where
  hasRep = RCons hasRep

hVectCurry :: HasRep ts => (HVect ts -> a) -> HVectElim ts a
hVectCurry = hVectCurryExpl hasRep


