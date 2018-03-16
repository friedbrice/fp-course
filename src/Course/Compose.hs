{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
  f <$> (Compose xs) = Compose ((f <$>) <$> xs)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  (Compose fs) <*> (Compose xs) = Compose (lift2 (<*>) fs xs)

instance (Monad f, Monad g) => Monad (Compose f g) where
  (=<<) = error "this one is not possible"
