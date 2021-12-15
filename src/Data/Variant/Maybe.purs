module Data.Variant.Maybe where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL)
import Data.Functor.Invariant (class Invariant)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Variant (Variant, default, match, on, inj)
import Type.Proxy (Proxy(..))

newtype Maybe a = Maybe (Variant (just :: a, nothing :: Unit))

derive instance newtypeMaybe :: Newtype (Maybe a) _
derive instance eqMaybe :: Eq a => Eq (Maybe a)
instance eq1Maybe :: Eq1 Maybe where
  eq1 a b = isNothing a == isNothing b
instance ord1Maybe :: Ord1 Maybe where
  compare1 a b = compare (isJust a) (isJust b)
instance boundedMaybe :: Bounded a => Bounded (Maybe a) where
  top = just top
  bottom = just bottom
instance foldableMaybe :: Foldable Maybe where
  foldl f b (Maybe a) = a # match
    { just: \a' -> f b a'
    , nothing: \_ -> b
    }
  foldr f b (Maybe a) = a # match
    { just: \a' -> f a' b
    , nothing: \_ -> b
    }
  foldMap = foldMapDefaultL
instance foldableWithIndexMaybe :: FoldableWithIndex Unit Maybe where
  foldlWithIndex f b (Maybe a) = a # match
    { just: \a' -> f unit b a'
    , nothing: \_ -> b
    }
  foldrWithIndex f b (Maybe a) = a # match
    { just: \a' -> f unit a' b
    , nothing: \_ -> b
    }
  foldMapWithIndex = foldMapWithIndexDefaultL
instance traversableMaybe :: Traversable Maybe where
  traverse f (Maybe a) = a # match
    { just: \a' -> just <$> f a'
    , nothing: \_ -> pure nothing
    }
  sequence = sequenceDefault
instance traversableWithIndexMaybe :: TraversableWithIndex Unit Maybe where
  traverseWithIndex f (Maybe a) = a # match
    { just: \a' -> just <$> f unit a'
    , nothing: \_ -> pure nothing
    }
instance functorWithIndexMaybe :: FunctorWithIndex Unit Maybe where
  mapWithIndex f (Maybe a) = a # match
    { just: \a' -> just (f unit a')
    , nothing: \_ -> nothing
    }
derive instance ordMaybe :: Ord a => Ord (Maybe a)
instance functorMaybe :: Functor Maybe where
  map f (Maybe v) = v # match
    { just: \a -> just $ f a
    , nothing: \_ -> nothing
    }
instance applyMaybe :: Apply Maybe where
  apply (Maybe fa) (Maybe a) = match
    { just: \fa' -> a # match
        { just: \a' -> just (fa' a')
        , nothing: const nothing
        }
    , nothing: const nothing
    }
    fa
instance applicativeMaybe :: Applicative Maybe where
  pure = just
instance bindMaybe :: Bind Maybe where
  bind (Maybe ma) f = (default nothing # onJust f) ma
instance monadMaybe :: Monad Maybe
instance altMaybe :: Alt Maybe where
  alt a b
    | isJust a = a
    | isJust b = b
    | otherwise = a
instance plusMaybe :: Plus Maybe where
  empty = nothing
instance alternativeMaybe :: Alternative Maybe
instance extendMaybe :: Extend Maybe where
  extend f x = just (f x)
instance invariantMaybe :: Invariant Maybe where
  imap f _ (Maybe v) = v # match
    { just: \a -> just $ f a
    , nothing: \_ -> nothing
    }
instance semigroupMaybe :: Semigroup a => Semigroup (Maybe a) where
  append _a@(Maybe ax) _b@(Maybe bx) = ax # match
    { just: \a -> bx # match
        { just: \b -> just (a <> b)
        , nothing: \_ -> _a
        }
    , nothing: \_ -> bx # match
        { just: \_ -> _b
        , nothing: \_ -> nothing
        }
    }
instance monoidMaybe :: Monoid a => Monoid (Maybe a) where
  mempty = nothing
derive newtype instance showMaybe :: Show a => Show (Maybe a)

just :: forall a. a -> Maybe a
just = Maybe <<< inj (Proxy :: _ "just")

nothing :: forall a. Maybe a
nothing = Maybe $ inj (Proxy :: _ "nothing") unit

onJust
  :: forall a b r
   . (a -> b)
  -> (Variant r -> b)
  -> Variant
       ( just :: a
       | r
       )
  -> b
onJust = on (Proxy :: _ "just")

isNothing :: forall a. Maybe a -> Boolean
isNothing = unwrap >>>
  match { just: const false, nothing: const true }

isJust :: forall a. Maybe a -> Boolean
isJust = unwrap >>>
  match { just: const true, nothing: const false }

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b f (Maybe v) = (default b # onJust f) v

maybe' :: forall a b. (Unit -> b) -> (a -> b) -> Maybe a -> b
maybe' fb f (Maybe v) = v # match
  { just: f
  , nothing: fb
  }

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a (Maybe v) = v # match
  { just: \b -> b
  , nothing: \_ -> a
  }
