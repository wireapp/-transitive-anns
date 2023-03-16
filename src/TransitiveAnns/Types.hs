{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module TransitiveAnns.Types where

import qualified Data.Set as S
import Data.Set (Set)
import Data.Data (Typeable, Data)
import GHC.TypeLits (Symbol)

data Location = Local | Remote
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

data Annotation = Annotation
  { ann_location :: Location
  , ann_api      :: String
  , ann_method   :: String
  }
  deriving (Eq, Ord, Show, Typeable, Data)


------------------------------------------------------------------------------
-- | 'AddAnnotation' constraints are automatically solved by this plugin, but
-- internally act as "unsolved" constraints, without you needing to propagate
-- them by hand.
--
-- The unsolved constraints can be reintroduced via a 'ToHasAnnotations'
-- constraint, which will automatically be solved and replaced with
-- a corresponding 'HasAnnotation' for every 'AddAnnotation' in the transitive
-- closure.
--
-- The @a@ parameter is intentionally ambiguous, existing as a unique skolem to
-- prevent GHC from caching the results of solving 'AddAnnotation'. Callers
-- needn't worry about it.
class AddAnnotation (loc :: Location) (api :: Symbol) (method :: Symbol) a


------------------------------------------------------------------------------
-- | A reintroduced version of 'AddAnnotation' for use in applicaton code. You
-- are expected to write your own instances for this class, as it is not solved
-- by transitive-anns.
--
-- The @a@ parameter is intentionally ambiguous, existing as a unique skolem to
-- prevent GHC from caching the results of solving 'HasAnnotation'. Callers
-- needn't worry about it.
class HasAnnotation (loc :: Location) (api :: Symbol) (method :: Symbol)


------------------------------------------------------------------------------
-- | Reintroduce every transitively-solved 'AddAnnotation' constraint,
--
-- The @a@ parameter is intentionally ambiguous, existing as a unique skolem to
-- prevent GHC from caching the results of solving 'ToHasAnnotation'. Callers
-- needn't worry about it.
class ToHasAnnotations a


------------------------------------------------------------------------------
-- | This function exists only to provide a convenient place for the
-- @transitive-anns@ plugin to solve the 'ToHasAnnotations' constraint. This is
-- highly magical and warrants a note.
--
-- The call @'exposeAnnotations' (some expr here)@ will expand to @some expr
-- here@, additionally generating wanted 'HasAnnotation' constraints for every
-- 'AddAnnotation' constraint in the _transitive call closure_ of @some expr
-- here@.
--
-- The @x@ parameter here is intentionally ambiguous, existing as a unique
-- skolem to prevent GHC from caching the results of solving
-- 'ToHasAnnotations'. Callers needn't worry about it.
exposeAnnotations :: ToHasAnnotations x => a -> a
exposeAnnotations = id


------------------------------------------------------------------------------
-- | This class is automatically solved in order to witness the transitive
-- annotations added via 'AddAnnotation'.
--
-- HOWEVER, the implementation of solving this constraint has a bug that breaks
-- referential transparency. Solving a 'KnownAnnotations' constraint witnesses
-- every 'AddAnnotation'' visible to the current definition, which means you
-- can differentiate between the two programs:
--
-- @@
-- foo1 = (annotated x, annotated y)
--
-- foo2 = (ax, ay)
--   where
--     ax = annotated x
--     ay = annotated y
-- @@
--
-- This is truly naughty and anethema to everything we hold sacred in Haskell.
-- Never use 'KnownAnnotations'. It exists only so we have something to write
-- tests against.
--
-- The @a@ parameter is intentionally ambiguous, existing as a unique skolem to
-- prevent GHC from caching the results of solving 'KnownAnnotations'. Callers
-- needn't worry about it.
class KnownAnnotations a where
  rawAnnotationsVal :: [Annotation]


annotationsVal :: forall x. KnownAnnotations x => Set Annotation
annotationsVal = S.fromList (rawAnnotationsVal @x)
{-# INLINE annotationsVal #-}


withAnnotations :: forall a x. KnownAnnotations x => a -> (Set Annotation, a)
withAnnotations a = (annotationsVal @x, a)
{-# INLINE withAnnotations #-}


annotated :: forall a x. KnownAnnotations x => a -> Set Annotation
annotated = fst . withAnnotations @a @x

