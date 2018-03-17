{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f = foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse :: Applicative f => (a -> f b) -> ExactlyOne a -> f (ExactlyOne b)
  traverse f (ExactlyOne x) = ExactlyOne <$> f x

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse k (Full x) = Full <$> k x

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
  traverse k (Compose xs) = Compose <$> traverse (traverse k) xs

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a = Product (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
  (<$>) :: (a -> b) -> Product f g a -> Product f g b
  b_a <$> Product fa ga = Product (b_a <$> fa) (b_a <$> ga)

instance (Traversable f, Traversable g) => Traversable (Product f g) where
  traverse :: Applicative m => (a -> m b) -> Product f g a -> m (Product f g b)
  traverse k (Product fa ga) = lift2 Product (traverse k fa) (traverse k ga)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a = InL (f a) | InR (g a)

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  (<$>) :: (a -> b) -> Coproduct f g a -> Coproduct f g b
  b_a <$> InL fa = InL (b_a <$> fa)
  b_a <$> InR ga = InR (b_a <$> ga)

instance (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse :: Applicative m => (a -> m b) -> Coproduct f g a -> m (Coproduct f g b)
  traverse k (InL fa) = InL <$> traverse k fa
  traverse k (InR ga) = InR <$> traverse k ga
