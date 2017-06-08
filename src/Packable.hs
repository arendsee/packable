-- | Allow multiple parameters in one class
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Allow parameterized types in function context
{-# LANGUAGE FlexibleContexts #-}
-- | Allow parameterized types in instances
{-# LANGUAGE FlexibleInstances #-}
-- | Allow type synonyms in instance contexts
--   Haskell98 only allows type constructors made with newtype and data
{-# LANGUAGE TypeSynonymInstances #-}
-- | Needed for string literals to be interpreted as Text or ByteString
{-# LANGUAGE OverloadedStrings #-}

module Packable
(
    Packable(..)
  , unpackString
) where

import qualified Data.ByteString.Char8 as DBC
import qualified Data.String
import qualified Data.Text as DT

-- | This is a class of symetric unary functions.  Requires:
--   (1) MultiParamTypeClasses to have two types 
class Packable a b where
  gpack :: a -> b
  gunpack :: b -> a

-- | The use of String in the context requires
--   (1) FlexibleContexts to allow a non-type variable ([Char])
--   (2) TypeSynonymInstances to allow String (rather than [Char])
instance Packable String DBC.ByteString where
  gpack = DBC.pack
  gunpack = DBC.unpack

instance Packable String DT.Text where
  gpack = DT.pack
  gunpack = DT.unpack

-- | Requires FlexibleContexts
--
-- Without FlexibleContexts, emits this error message:
--
-- > Non type-variable argument in the constraint: Packable String a
-- > ||   (Use FlexibleContexts to permit this)
-- > || • In the type signature:
-- > ||     unpackString :: (Packable String a) => a -> String
unpackString :: (Packable String a) => a -> String
unpackString = gunpack
