module Data.Variant.Either where

import Prelude

import Control.Alt (class Alt)
import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj, match)
import Data.Variant.Maybe (Maybe, nothing, just)
import Type.Proxy (Proxy(..))

newtype Either a b = Either (Variant (left :: a, right :: b))

derive instance newtypeEither :: Newtype (Either a b) _
derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive newtype instance showEither :: (Show a, Show b) => Show (Either a b)
derive newtype instance boundedEither :: (Bounded a, Bounded b) => Bounded (Either a b)
instance semigroupEither :: Semigroup b => Semigroup (Either a b) where
  append (Either fa) (Either a) = match
    { right: \fa' -> a # match
        { right: \a' -> right (fa' <> a')
        , left: \x -> left x
        }
    , left: \x -> left x
    }
    fa
instance altEither :: Alt (Either a) where
  alt l@(Either fa) r@(Either _) = match
    { right: \_ -> l
    , left: \_ -> r
    }
    fa

instance functorEither :: Functor (Either a) where
  map f (Either e) = match {
    left: \x -> left x,
    right: right <<< f
  } e
instance applyEither :: Apply (Either a) where
  apply (Either fa) (Either a) = match
    { right: \fa' -> a # match
        { right: \a' -> right (fa' a')
        , left: \x -> left x
        }
    , left: \x -> left x
    }
    fa
instance applicativeEither :: Applicative (Either a) where
  pure = right
instance bindEither :: Bind (Either a) where
  bind (Either ma) f = ma # match {
    left: \x -> left x,
    right: f
  }
instance monadEither :: Monad (Either a)
hush :: forall a b. Either a b -> Maybe b
hush (Either v) = v # match
  { left: \_ -> nothing
  , right: \a -> just a
  }

right :: forall a b. b -> Either a b
right = wrap <<< inj (Proxy :: Proxy "right")

left :: forall a b. a -> Either a b
left = wrap <<< inj (Proxy :: Proxy "left")

isLeft :: forall a b. Either a b -> Boolean
isLeft (Either e) = e # match { left: const true, right: const false }

isRight :: forall a b. Either a b -> Boolean
isRight = not <<< isLeft

either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either lf rf (Either v) = v # match
  { left: \a -> lf a
  , right: \b -> rf b
  }