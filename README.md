# Packable

This is not intended to be production code, rather is just a practice repo for
me to experiment with multi-parameter type classes.

# Practical use of Packable

TLDR: not so much

The purpose of this package it to provide a single interface to all things that
can reasonably be packed and unpacked.

The typeclass is:

```haskell
class Packable a b where
  gpack   :: a -> b
  gunpack :: b -> a
```

Where `gpack` converts unpacked data into packed data, and `gunpack` does the
opposite. There are many pairs of types that are symmetric in this way, for example:


 Unpacked               | Packed
 ---------------------- | ----------------------
 unencrypted data       | encrypted data
 uncompressed data      | compressed data
 a nested list of files | an archive
 a haskell object       | a binary serialization
 a haskell object       | a json text file
 string                 | bytestring
 string                 | text
 bytestring             | text

Now as glorious as it to simply use `gpack` and `gunpack` to automatically
switch between any of these, without having to remember all the particular
functions each library uses, there are a few little problems.

First of all, there is not a one-to-one relationship between these. For
example, a `String` can pack into `ByteString` or `Text` (or a bunch of other
things, really). So casting will require an explicit type annotation, e.g.

```haskell
ghci> let s = "this is a string"
ghci> let b = gpack s :: ByteString
ghci> let t = gpack s :: Text
```

This is still reasonably clean, though. A trickier problem is that some of
these pairs are the same type on both ends. A pure `compress` function will
likely map from a `ByteString` to a `ByteString`. Likewise for the
encryption/decryption pair. There may be some solution to this using phantom
types. However, the problem of different pack/unpack function name across
symmetric pairs is not so onerous as to merit drastic solutions.

Overall, I think there is a neat symmetry here that could be generalized. But
I am not sure the abstraction is worth the cost.

# Pedagogical use of Packable

This package introduces multi-parameter typeclasses and the related GHC
extensions. I will introduce the following extensions:

 * MultiParamTypeClasses - allow multi-parameter typeclasses
 * TypeSynonymInstances - allow type synonyms to be used in instance
 * FlexibleInstances - allow nested types in instance signatures
 * FlexibleContexts - allow nested types in contexts of instances or classes

The Packable typeclass defines two functions for mapping between two types. It
describes a symmetric relationship and so naturally requires two parameters.

```haskell
class Packable a b where
  gpack   :: a -> b
  gunpack :: b -> a
```

When we typecheck this, this error obtains:

```
Too many parameters for class ‘Packable’
||   (Use MultiParamTypeClasses to allow multi-parameter classes)
|| • In the class declaration for ‘Packable’
```

The compiler suggests we "Use MultiParamTypeClasses". Now you don't want to
willy-nilly do whatever the compiler suggests. Sometimes they suggest
extensions that "fix" problems by relaxing restrictions. And sometimes this is
dangerous. The best place to find solid information on what the extensions do
is the [GHC user
guide](https://downloads.haskell.org/~ghc/6.12.2/docs/html/users_guide/index.html).
Be sure to understand an extension before you enable it.

In this case, Haskell does not by default allow multi-parameter typeclasses.
The **MultiParamTypeClasses** extension enables this.

Now lets say we want to add an instance of `Packable` that converts between
`String` and `ByteString`. This is pretty easy, since it just involves stealing
existing functions.

```haskell
instance Packable String ByteString where
  gpack   = pack
  gunpack = unpack
```

But when we try to compile this, we get the following error:

```
Illegal instance declaration for ‘Packable String ByteString’
||     (All instance types must be of the form (T t1 ... tn)
||      where T is not a synonym.
||      Use TypeSynonymInstances if you want to disable this.)
|| • In the instance declaration for ‘Packable String ByteString’
```


The GHC User Guide offers a very readable discussion of the
`TypeSynonymInstances` and `FlexibleInstances` extensions. Basically, in
Haskell98, there are strong restrictions on what types my be used in instance
declarations. The reason for these restrictions is to avoid undecidable cases
(where the type algorithm fails). The specific rule is that an instance
declaration must have the form:

```
C ( T1 t1 t2 ..., T2 t1 t2 ... )
```

Where `C` is the class, T is a data type constructor, and t1, t2 etc are type
variables. GHC offers two extensions to weaken these restrictions:
`TypeSynonymInstances` and `FlexibleInstances`.

The problem here is that `String` is actually a type synonym for `[Char]`, and
type synonyms cannot be used in instance declarations. To allow this, you must,
as they suggest, add the **TypeSynonymInstances** extension.

After adding in this extension and re-running the typechecker, we get a new error

```
Illegal instance declaration for ‘Packable String ByteString’
||     (All instance types must be of the form (T a1 ... an)
||      where a1 ... an are *distinct type variables*,
||      and each type variable appears at most once in the instance head.
||      Use FlexibleInstances if you want to disable this.)
```

**FlexibleInstances** allows nested types in instance declarations. Another example, which the GHC manual gives, is:

```
instance C (Maybe Int) where ...
```

The use of `Maybe Int` would fail without this extension. In our case, we need the extension to allow `String` (i.e. `[Char]`) in

```haskell
instance Packable String ByteString where
```

Now the ByteString and Text instances all typecheck.

Let's say we want to create a function that unpacks something into a String. We can do this by specializing the fully generic Packable class:

```haskell
unpackString :: (Packable String a) => a -> String
unpackString = gunpack
```

Compiling again we get a new error message:

```
Non type-variable argument in the constraint: Packable String a
||   (Use FlexibleContexts to permit this)
|| • In the type signature:
||     unpackString :: (Packable String a) => a -> String
```

This is an issue very similar to the one `FlexibleInstances` solved earlier. The difference is that `FlexibleInstances` allowed complex types in the function signature, whereas `FlexibleContexts` loosens restrictions in the function (or instance) contexts.

Adding in **FlexibleContexts** allows everything to compile.
