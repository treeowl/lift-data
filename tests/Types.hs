{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
#endif

{-|
Module:      Types
Copyright:   (C) 2015-2017 Ryan Scott, (C) 2021 David Feuer
License:     BSD-style (see the file LICENSE)
Maintainer:  David Feuer

Data types for testing `lift-data`' capabilities.
-}
module Types (
    PureQ, runPureQ
  , Unit(..), Product(..), Sum(..)
  , p, s
  ) where

import Control.Monad.State
import Data.Data

import GHC.Exts

import Language.Haskell.TH.Lift.Data ( genericLift
#if MIN_VERSION_template_haskell(2,16,0)
                                     , genericLiftTypedCompat
#endif
                                     )
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat

import Prelude ()
import Prelude.Compat

newtype PureQ a = MkPureQ (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

runPureQ :: PureQ a -> a
runPureQ m = case m of MkPureQ m' -> evalState m' 0

instance Quote PureQ where
  newName s' = state $ \i -> (mkNameU s' i, i + 1)

data Unit = Unit
  deriving (Data, Typeable, Eq, Ord, Show)

data Product a b c d = Product a b c d
  deriving (Data, Typeable, Eq, Ord, Show)

data Sum a b = Inl a | Inr b
  deriving (Data, Typeable, Eq, Ord, Show)

p :: Product Char Int Bool String
p = Product 'a' 1 True "b"

s :: Sum Char Int
s = Inl 'c'

instance Lift Unit where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Data a, Data b, Data c, Data d) => Lift (Product a b c d) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Data a, Data b) => Lift (Sum a b) where
    lift = genericLift
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif
