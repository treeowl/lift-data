{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase            #-}
#endif

{-|
Module:      Language.Haskell.TH.Lift.Data
Copyright:   (C) 2015-2017 Ryan Scott
             (C) 2021 David Feuer
License:     BSD-style (see the file LICENSE)
Maintainer:  David Feuer

"Data.Data"-based approach to implementing `lift`. Different functions are
available for use with different @template-haskell@ versions.

This package was a fun exercise to write, and I highly recommend
that you read its documentation and implement your own copy. You
can peek at the source code if you get stuck.

You probably don't actually want to use this package for programming. It is
inferior in almost every way to
<https://hackage.haskell.org/package/lift-generics lift-generics>.
It is also, generally speaking, inferior to
<https://hackage.haskell.org/package/th-lift th-lift>.
All of these are inferior to @-XDeriveLift@ for GHC 8.0 and later.

=== Why you might actually want to use this package

If you have a type with a `Data` instance, then you can use
this package to write its 'Lift' instance. You don't need any
of the components of the type to have pre-existing 'Lift' instances.

=== Why you probably don't

* Unlike 'GHC.Generics.Generic', 'Data' goes deep, so there are far
  fewer 'Data' instances in the wild than 'Generic' ones. Furthermore,
  polymorphism is even more limited than that might suggest! With
  @lift-generics@, you could write

  @
  instance 'Lift' a => 'Lift' ('Control.Applicative.Const' a b) where
    lift = genericLift@
  @

  Here, you'd need to write

  @
  instance ('Data' a, 'Typeable' b) => 'Lift' ('Control.Applicative.Const' a b) where
    lift = genericLift
  @

  constraining both type arguments.

* @lift-generics@ offers unpleasant but usable fall-back functions for GHC
  versions before 7.4 and for situations where 'Typeable' instances aren't
  available. Thanks to a GHC bug affecting many recent versions, we can't
  offer anything much like that here.

* @th-lift@ is more powerful than either @lift-generics@ or this package,
  supporting things like GADTs, at the cost of requiring splicing to
  produce the instances.

* The built-in @-XDeriveLift@ is of course more powerful than any other
  option.
-}
module Language.Haskell.TH.Lift.Data (
    -- * "Data.Data"-based 'lift' implementations
    --
    -- $friendlyFunctions
      genericLift
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTyped
    , genericLiftTypedTExp
    , genericLiftTypedCompat
#endif
    ) where

import Control.Monad (liftM2)

import Data.Data

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

import qualified Data.Typeable as T

-- We don't want to expand this in the Haddocks!
#undef CURRENT_PACKAGE_KEY

-- $friendlyFunctions
--
-- These functions do all of the work for you:
--
-- @
-- &#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;
-- module Foo where
--
-- import GHC.Generics
-- import Language.Haskell.Lift.Generics
--
-- data Foo = Foo Int Char String
--   deriving Generic
--
-- instance Lift Foo where
--   lift = genericLift
-- &#35;if MIN&#95;VERSION&#95;template_haskell(2,9,0)
--   liftTyped = genericLiftTypedCompat
-- &#35;endif
-- @
--
-- Now you can splice @Foo@ values directly into Haskell source code:
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- module Bar where
--
-- import Foo
-- import Language.Haskell.TH.Syntax
--
-- foo :: Foo
-- foo = $(lift (Foo 1 \'a\' \"baz\"))
-- @

-- | Produce a generic definition of 'lift'.
--
-- === Note
--
-- A @'Data.Typeable.Typeable' a@ instance is required.
genericLift :: forall m a. (Quote m, Data a) => a -> m Exp
genericLift a = gmapQl (liftM2 AppE) (return the_con) genericLift a
  where
    the_con = case constrRep (toConstr a) of
      AlgConstr{} -> ConE (mkNameG_d pkg md constr_name)
      IntConstr i -> LitE $ IntegerL i
      FloatConstr r -> LitE $ RationalL r
      CharConstr c -> LitE $ CharL c
    constr_name = case showConstr (toConstr a) of
      '(' : rest -> init rest
      plain -> plain
    pkg = T.tyConPackage tc
    md = T.tyConModule tc
#if MIN_VERSION_base (4,7,0)
    tc = T.typeRepTyCon (T.typeRep (Proxy :: Proxy a))
#else
    tc = T.typeRepTyCon (T.typeOf a)
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLift', but returns a 'Code' instead of an 'Exp'.
genericLiftTyped :: (Quote m, Data a) => a -> Code m a
genericLiftTyped = unsafeCodeCoerce . genericLift

-- | Like 'genericLift', but returns a 'TExp' instead of an 'Exp'.
genericLiftTypedTExp :: (Quote m, Data a) => a -> m (TExp a)
genericLiftTypedTExp = unsafeTExpCoerceQuote . genericLift

-- | Lift 'genericLift', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
genericLiftTypedCompat :: (Quote m, Data a) => a -> Splice m a
# if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompat = genericLiftTyped
# else
genericLiftTypedCompat = genericLiftTypedTExp
# endif
#endif
