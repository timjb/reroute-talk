{-# LANGUAGE DataKinds #-}

Path :: [*] -> *

root :: Path '[]

static :: Text -> Path '[]

instance IsString (Path '[]) where
  fromString = static

-- `Append xs as` ist `xs ++ as` auf Typlevel

<//> :: Path as -> Path bs -> Path (Append as bs)

-- HVectElim '[a,b,c,d] x ≡ a -> (b -> (c -> (d -> x)))

renderRoute :: Path as -> HVectElim as Text

get :: MonadIO m => Path as -> HVectElim as (ActionT m ()) -> SpockT m ()