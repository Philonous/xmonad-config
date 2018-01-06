module Util where

import Data.Foldable

and' :: (Foldable t, Applicative f) => t (f Bool) -> f Bool
and' = foldl' (\b bs -> (&&) <$> b <*> bs) (pure True)

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

third (_,_,x) = x
