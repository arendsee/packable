-- Allow multiple parameters in one class
{-# LANGUAGE MultiParamTypeClasses #-}
-- Allow parameterized types in function context
{-# LANGUAGE FlexibleContexts #-}
-- Allow parameterized types in instances
{-# LANGUAGE FlexibleInstances #-}
-- Allow type synonyms in instance contexts
-- Haskell98 only allows type constructors made with newtype and data
{-# LANGUAGE TypeSynonymInstances #-}

{-|

The package exports the typeclass 'Packable', which contains two main
functions: 'gpack' and 'gunpack'. these are symmetric functions, where @'gpack' .
'gunpack'@ and @'gunpack' . 'gpack'@ are both defined and both identical to @'id'@.
members of the 'Packable' typeclass are assumed to be things can be converted
to or from a reduced form. the packable typeclass takes two parameters, the
first is the unpacked type, the second is the packed type.  here are a few
examples of type pairs.

  * unencrypted data, encrypted data
  * uncompressed data, compressed data
  * a nested list of files, an archive
  * a haskell object, a binary serialization
  * a haskell object, a json text file
  * string, bytestring
  * string, text
  * bytestring, text

often times there can be many layers of packing, for example a haskell
object that has ben serialized, compressed, and encrypted. this typeclass
would ideally allow this to be opened with an @'gunpack' . 'gunpack' . 'gunpack'@
function.

-}

module Packable
(
    Packable(..)
  , unpackString
) where

import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as DT
import qualified Data.Binary as DB

-- Requires: MultiParamTypeClasses to have two types 

-- | A class for symmetric types 'a' and 'b', where 'a' is the unpacked form
-- and 'b' is the packed form.
class Packable a b where
  gpack   :: a -> b
  gunpack :: b -> a

-- The use of String in the context requires
-- (1) FlexibleContexts to allow a non-type variable ([Char])
-- (2) TypeSynonymInstances to allow String (rather than [Char])
instance Packable String DBC.ByteString where
  gpack   = DBC.pack
  gunpack = DBC.unpack

instance Packable String DT.Text where
  gpack   = DT.pack
  gunpack = DT.unpack

instance (DB.Binary a) => Packable a DBL.ByteString where
  gpack = DB.encode
  gunpack = DB.decode

-- Without FlexibleContexts, emits this error message:
--
-- > Non type-variable argument in the constraint: Packable String a
-- > ||   (Use FlexibleContexts to permit this)
-- > || • In the type signature:
-- > ||     unpackString :: (Packable String a) => a -> String
--
-- | 'unpackString' takes a Text or ByteString and converts it to String
--
-- @
-- ghci> let s = "this is Text" :: Text
-- ghci> let b = "this is a ByteString" :: ByteString
-- ghci> unpackString s
--   "this is Text"
--   it :: String
-- ghci> unpackString b
--   "this is a ByteString"
--   it :: String
-- @
unpackString :: (Packable String a) => a -> String
unpackString = gunpack
