{backmatter}


# Contekan Kelas Tipe

| Kelas Tipe         | Metoda          | Asal            | Diberikan              | Tujuan         |
|------------------ |--------------- |--------------- |---------------------- |-------------- |
| `InvariantFunctor` | `xmap`          | `F[A]`          | `A => B, B => A`       | `F[B]`         |
| `Contravariant`    | `contramap`     | `F[A]`          | `B => A`               | `F[B]`         |
| `Functor`          | `map`           | `F[A]`          | `A => B`               | `F[B]`         |
| `Apply`            | `ap` / `<*>`    | `F[A]`          | `F[A => B]`            | `F[B]`         |
|                    | `apply2`        | `F[A], F[B]`    | `(A, B) => C`          | `F[C]`         |
| `Alt`              | `altly2`        | `F[A], F[B]`    | `(A \/ B) => C`        | `F[C]`         |
| `Divide`           | `divide2`       | `F[A], F[B]`    | `C => (A, B)`          | `F[C]`         |
| `Decidable`        | `choose2`       | `F[A], F[B]`    | `C => (A \/ B)`        | `F[C]`         |
| `Bind`             | `bind` / `>>=`  | `F[A]`          | `A => F[B]`            | `F[B]`         |
|                    | `join`          | `F[F[A]]`       |                        | `F[A]`         |
| `Cobind`           | `cobind`        | `F[A]`          | `F[A] => B`            | `F[B]`         |
|                    | `cojoin`        | `F[A]`          |                        | `F[F[A]]`      |
| `Applicative`      | `point`         | `A`             |                        | `F[A]`         |
| `Divisible`        | `conquer`       |                 |                        | `F[A]`         |
| `Comonad`          | `copoint`       | `F[A]`          |                        | `A`            |
| `Semigroup`        | `append`        | `A, A`          |                        | `A`            |
| `Plus`             | `plus` / `<+>`  | `F[A], F[A]`    |                        | `F[A]`         |
| `MonadPlus`        | `withFilter`    | `F[A]`          | `A => Boolean`         | `F[A]`         |
| `Align`            | `align`         | `F[A], F[B]`    |                        | `F[A \&/ B]`   |
|                    | `merge`         | `F[A], F[A]`    |                        | `F[A]`         |
| `Zip`              | `zip`           | `F[A], F[B]`    |                        | `F[(A, B)]`    |
| `Unzip`            | `unzip`         | `F[(A, B)]`     |                        | `(F[A], F[B])` |
| `Cozip`            | `cozip`         | `F[A \/ B]`     |                        | `F[A] \/ F[B]` |
| `Foldable`         | `foldMap`       | `F[A]`          | `A => B`               | `B`            |
|                    | `foldMapM`      | `F[A]`          | `A => G[B]`            | `G[B]`         |
| `Traverse`         | `traverse`      | `F[A]`          | `A => G[B]`            | `G[F[B]]`      |
|                    | `sequence`      | `F[G[A]]`       |                        | `G[F[A]]`      |
| `Equal`            | `equal` / `===` | `A, A`          |                        | `Boolean`      |
| `Show`             | `shows`         | `A`             |                        | `String`       |
| `Bifunctor`        | `bimap`         | `F[A, B]`       | `A => C, B => D`       | `F[C, D]`      |
|                    | `leftMap`       | `F[A, B]`       | `A => C`               | `F[C, B]`      |
|                    | `rightMap`      | `F[A, B]`       | `B => C`               | `F[A, C]`      |
| `Bifoldable`       | `bifoldMap`     | `F[A, B]`       | `A => C, B => C`       | `C`            |
| (with `MonadPlus`) | `separate`      | `F[G[A, B]]`    |                        | `(F[A], F[B])` |
| `Bitraverse`       | `bitraverse`    | `F[A, B]`       | `A => G[C], B => G[D]` | `G[F[C, D]]`   |
|                    | `bisequence`    | `F[G[A], G[B]]` |                        | `G[F[A, B]]`   |


# Haskell

Scalaz documentation often cites libraries or papers written in the Haskell
programming language. In this short chapter, we will learn enough Haskell to be
able to understand the source material, and to attend Haskell talks at
functional programming conferences.

Dokumentasi Scalaz sering kali mengutip pustaka atau makalah yang ditulis dengan
bahasa pemrograman Haskell. Pada bab pendek ini, kita akan mempelajari Haskell
agar dapat memahami materi sumber, dan dapat mengunjungi pembahasan haskell pada
konferensi pemrograman fungsional.

## Data

Haskell memiliki sintaks yang jelas untuk Tipe Data Aljabaris. Berikut adalah
struktur senarai berantai:

{lang="text"}
~~~~~~~~
  data List a = Nil | Cons a (List a)
~~~~~~~~

`List`merupakan *konstruktor tipe*, `a` merupakan *parameter tipe*, `|` memisahkan
*konstruktor data*, yang terdiri dari: `Nil` yang merupakan senarai kosong dan
`Cons` yang menerima dua parameter yang dipisahkan ruang putih: tanpa koma dan
tanpa pengurung parameter. Selain itu, Haskell juga tidak memiliki anak-tipe.

Bila diterjemahkan ke Scala, kurang lebih sebagai berikut:

{lang="text"}
~~~~~~~~
  sealed abstract class List[A]
  object Nil {
    def apply[A]: List[A] = ...
    def unapply[A](as: List[A]): Option[Unit] = ...
  }
  object Cons {
    def apply[A](head: A, tail: List[A]): List[A] = ...
    def unapply[A](as: List[A]): Option[(A, List[A])] = ...
  }
~~~~~~~~

mis., konstruktor tipe bisa kurang lebih seperti `sealed abstract class`, dan
tiap konstruktor data sebagai `.apply` / `.unapply`. Harap diperhatikan bahwa
Scala tidak melakukan pencocokan pola pada penyandian semacam ini. Dengan
demikian, Scalaz juga tidak menggunakannya.

Bila kita ingin mendefinisikan `List` yang lebih rapi, kita dapat menggunakan
simbol infiks `:.` sebagai ganti `Cons`

{lang="text"}
~~~~~~~~
  data List t = Nil | t :. List t
  infixr 5 :.
~~~~~~~~

dimana kita menentukan *ketetapan* (*fixity*) dimana `infix` untuk menentukan
tidak adanya hubungan asosiatif, `infixl` untuk hubungan asosiatif kiri, dan
`infixr` untuk hubungan asosiatif kanan. Angka dari 0 (longgar) sampai 9 (ketat)
menentukan presedensi. Sekarang kita dapat membuat senarai integer dengan menulis

{lang="text"}
~~~~~~~~
  1 :. 2 :. Nil
~~~~~~~~

Haskell sudah mengikut sertakan dukungan senarai berantai, yang sangat fundamental
pada pemrograman fungsional, sampai pada tingkat bahasa dengan memberikan sintaks
kurung siku sehingga dilambangkan dengan `[a]`

{lang="text"}
~~~~~~~~
  data [] a = [] | a : [a]
  infixr 5 :
~~~~~~~~

dan pembantu konstruktor nilai argumen jamak: `[1, 2, 3]`, bukan `1 : 2 : 3 : []`.

Dan utamanya, Tipe Data Aljabaris kita harus menampung nilai primitif.  Tipe
data primitif yang paling jamak digunakan adalah:

-   `Char` karakter unikode
-   `Text` untuk blok teks unikode
-   `Int` integer tertanda dengan presisi tetap yang bergantung pada mesin
-   `Word` `Int` tanpa tanda, dan `Word8` / `Word16` / `Word32` / `Word64` dengan ukuran tetap
-   `Float` / `Double` bilangan presisi tunggal dan ganda berstandar IEEE
-   `Integer` / `Natural` integer tertanda presisi arbiter / non-negatif
-   `(,)` tuple, dari 0 (disebut juga *unit*) sampai 62 bidang
-   `IO` inspirasi dari `IO` Scalaz, diimplementasikan pada waktu-jalan.

dengan sebutan kehormatan untuk

{lang="text"}
~~~~~~~~
  data Bool       = True | False
  data Maybe a    = Nothing | Just a
  data Either a b = Left a  | Right b
  data Ordering   = LT | EQ | GT
~~~~~~~~

Seperti Scala, Haskell memiliki alias tepe: sebuah alias atau bentuk terjabarkannya
dapat digunakan secara bergantian. Dikarenakan alasan peninggalan, `String`
didefinisikan sebagai senarai berantai dari `Char`


{lang="text"}
~~~~~~~~
  type String = [Char]
~~~~~~~~

yang sangat tidak efisien. Kami sangat menyarankan untuk menggunakan `Text`
sebagai gantinya.

Dan pada akhirnya, kita dapat mendefinisikan nama bidang pada TDA dengan
menggunakan *sintaks rekor*, yang juga berarti, kita dapat menampung konstruktor
data didalam kurung kurawal dan menggunakan *anotasi tipe* dua titik dua untuk
mengindikasikan tipe dari bidang tersebut

{lang="text"}
~~~~~~~~
  -- raw ADT
  data Resource = Human Int String
  data Company  = Company String [Resource]
  
  -- with record syntax
  data Resource = Human { serial :: Int, humanName :: String }
  data Company  = Company { companyName :: String, employees :: [Resource] }
~~~~~~~~

Harap perhatikan bahwa konstruktor data `Human` dan tipe `Resource` tidak harus
memiliki nama yang sama. Sintaks rekor membuat ekuivalen dari metoda pengakses
bidang dan penyalinan. 

{lang="text"}
~~~~~~~~
  -- construct
  adam = Human 0 Adam
  -- field access
  serial adam
  -- copy
  eve = adam { humanName = "Eve" }
~~~~~~~~

Alternatif yang lebih efisien untuk pendefinisian `data` dengan satu bidang
saja adalah dengan menggunakan `newtype` yang tidak meminta beban tambahan
saat waktu-jalan:

{lang="text"}
~~~~~~~~
  newtype Alpha = Alpha { underlying :: Double }
~~~~~~~~

yang ekuivalen dengan `extends AnyVal` namun tanpa kekurangannya.

A> Batasan dari sintaks rekor Haskell adalah nama dari sebuah bidang tidak dapat
A> digunakan pada tipe data lain. Namun, kita dapat menyiasatinya dengan
A> menggunakan ekstensi `LANGUAGE` yang memperkenankan kita untuk menggunakan
A> `name` pada `Human` dan `Company`:
A>
A> {lang="text"}
A> ~~~~~~~~
A>   {-# LANGUAGE DuplicateRecordFields #-}
A>   
A>   data Resource = Human { serial :: Int, name :: String }
A>   data Company  = Company { name :: String, employees :: [Resource] }
A> ~~~~~~~~
A>
A> Ada beberapa ekstensi bahasa dan tidak jarang untuk menggunakan 20 atau lebih
A> pada sebuah proyek kecil. Haskell sangat konservatif dan fitur bahasa biasanya
A> berupa pilihan tambahan untuk waktu yang lama sebelum fitur tersebut diterima
A> menjadi fitur utama.


## Functions

Although not necessary, it is good practice to explicitly write the type
signature of a function: its name followed by its type. For example `foldl`
specialised for a linked list

{lang="text"}
~~~~~~~~
  foldl :: (b -> a -> b) -> b -> [a] -> b
~~~~~~~~

All functions are *curried* in Haskell, each parameter is separated by a `->`
and the final type is the return type. This is equivalent to the following Scala
signature:

{lang="text"}
~~~~~~~~
  def foldLeft[A, B](f: (B, A) => B)(b: B)(as: List[A]): B
~~~~~~~~

Some observations:

-   there is no keyword
-   there is no need to declare the types that are introduced
-   there is no need to name the parameters

which makes for terse code.

Infix functions are defined in parentheses and need a fixity definition:

{lang="text"}
~~~~~~~~
  (++) :: [a] -> [a] -> [a]
  infixr 5 ++
~~~~~~~~

Regular functions can be called in infix position by surrounding their name with
backticks, and an infix function can be called like a regular function if we
keep it surrounded by brackets. The following are equivalent:

{lang="text"}
~~~~~~~~
  a `foo` b
  foo a b
~~~~~~~~

An infix function can be curried on either the left or the right, often giving
different semantics:

{lang="text"}
~~~~~~~~
  invert = (1.0 /)
  half   = (/ 2.0)
~~~~~~~~

Functions are typically written with the most general parameter first, to enable
maximum reuse of the curried forms.

The definition of a function may use pattern matching, with one line per case.
This is where we may name the parameters, using the data constructors to extract
parameters much like a Scala `case` clause:

{lang="text"}
~~~~~~~~
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing
~~~~~~~~

Underscores are a placeholder for ignored parameters and function names can be
in infix position:

{lang="text"}
~~~~~~~~
  (<+>) :: Maybe a -> Maybe a -> Maybe a
  Just a <+> _      = Just a
  Empty  <+> Just a = Just a
  Empty  <+> Empty  = Empty
~~~~~~~~

We can define anonymous lambda functions with a backslash, which looks like the
Greek letter λ. The following are equivalent:

{lang="text"}
~~~~~~~~
  (*)
  (\a1 -> \a2 -> a1 * a2)
  (\a1 a2     -> a1 * a2)
~~~~~~~~

Pattern matched Haskell functions are just syntax sugar for nested lambda
functions. Consider a simple function that creates a tuple when given three
inputs:

{lang="text"}
~~~~~~~~
  tuple :: a -> b -> c -> (a, b, c)
~~~~~~~~

The implementation

{lang="text"}
~~~~~~~~
  tuple a b c = (a, b, c)
~~~~~~~~

desugars into

{lang="text"}
~~~~~~~~
  tuple = \a -> \b -> \c -> (a, b, c)
~~~~~~~~

In the body of a function we can create local value bindings with `let` or
`where` clauses. The following are equivalent definitions of `map` for a linked
list (an apostrophe is a valid identifier name):

{lang="text"}
~~~~~~~~
  map :: (a -> b) -> [a] -> [b]
  
  -- explicit
  map f as = foldr map' [] as
             where map' a bs = f a : bs
  
  -- terser, making use of currying
  map f    = foldr map' []
             where map' a = (f a :)
  
  -- let binding
  map f    = let map' a = (f a :)
             in foldr map' []
  
  -- actual implementation
  map _ []       = []
  map f (x : xs) = f x : map f xs
~~~~~~~~

`if` / `then` / `else` are keywords for conditional statements:

{lang="text"}
~~~~~~~~
  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter f (head : tail) = if f head
                           then head : filter f tail
                           else filter f tail
~~~~~~~~

But it is considered better style to use *case guards*

{lang="text"}
~~~~~~~~
  filter f (head : tail) | f head    = head : filter f tail
                         | otherwise = filter f tail
~~~~~~~~

Pattern matching on any term is with `case ... of`

{lang="text"}
~~~~~~~~
  unfoldr :: (a -> Maybe (b, a)) -> a -> [b]
  unfoldr f b = case f b of
                  Just (b', a') -> b' : unfoldr f a'
                  Nothing       -> []
~~~~~~~~

Guards can be used within matches. For example, say we want to special case
zeros:

{lang="text"}
~~~~~~~~
  unfoldrInt :: (a -> Maybe (Int, a)) -> a -> [Int]
  unfoldrInt f b = case f b of
                     Just (i, a') | i == 0    -> unfoldrInt f a'
                                  | otherwise -> i : unfoldrInt f a'
                     Nothing                  -> []
~~~~~~~~

Finally, two functions that are worth noting are `($)` and `(.)`

{lang="text"}
~~~~~~~~
  -- application operator
  ($) :: (a -> b) -> a -> b
  infixr 0
  
  -- function composition
  (.) :: (b -> c) -> (a -> b) -> a -> c
  infixr 9
~~~~~~~~

Both of these functions are stylistic alternatives to nested parentheses.

The following are equivalent:

{lang="text"}
~~~~~~~~
  Just (f a)
  Just $ f a
~~~~~~~~

as are

{lang="text"}
~~~~~~~~
  putStrLn (show (1 + 1))
  putStrLn $ show $ 1 + 1
~~~~~~~~

There is a tendency to prefer function composition with `.` instead of multiple
`$`

{lang="text"}
~~~~~~~~
  (putStrLn . show) $ 1 + 1
~~~~~~~~


## Typeclasses

To define a typeclass we use the `class` keyword, followed by the name of the
typeclass, its type parameter, then the required members in a `where` clause. If
there are dependencies between typeclasses, i.e. `Applicative` requires a
`Functor`, use `=>` notation

{lang="text"}
~~~~~~~~
  class Functor f where
    (<$>) :: (a -> b) -> f a -> f b
    infixl 4 <$>
  
  class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    infixl 4 <*>
  
  class Applicative f => Monad f where
    (=<<) :: (a -> f b) -> f a -> f b
    infixr 1 =<<
~~~~~~~~

We provide an implementation of a typeclass with the `instance` keyword. If we
wish to repeat the type signature on instance functions, useful for clarity, we
must enable the `InstanceSigs` language extension.

{lang="text"}
~~~~~~~~
  {-# LANGUAGE InstanceSigs #-}
  
  data List a = Nil | a :. List a
  
  -- defined elsewhere
  (++) :: List a -> List a -> List a
  map :: (a -> b) -> List a -> List b
  flatMap :: (a -> List b) -> List a -> List b
  foldLeft :: (b -> a -> b) -> b -> List a -> b
  
  instance Functor List where
    (<$>) :: (a -> b) -> List a -> List b
    f <$> as = map f as
  
  instance Applicative List where
    pure a = a :. Nil
  
    Nil <*> _  = Nil
    fs  <*> as = foldLeft (++) Nil $ (<$> as) <$> fs
  
  instance Monad List where
    f =<< list = flatMap f list
~~~~~~~~

If we want to make use of a typeclass in a function we require it with `=>`. For
example we can define something similar to Scalaz's `Apply.apply2`

{lang="text"}
~~~~~~~~
  apply2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  apply2 f fa fb = f <$> fa <*> fb
~~~~~~~~

Since we have introduced `Monad`, it is a good time to introduce `do` notation,
which was the inspiration for Scala's `for` comprehensions:

{lang="text"}
~~~~~~~~
  do
    a <- f
    b <- g
    c <- h
    return (a, b, c)
~~~~~~~~

desugars to

{lang="text"}
~~~~~~~~
  f >>= \a ->
    g >>= \b ->
      h >>= \c ->
        return (a, b, c)
~~~~~~~~

where `>>=` is `=<<` with parameters flipped

{lang="text"}
~~~~~~~~
  (>>=) :: Monad f => f a -> (a -> f b) -> f b
  (>>=) = flip (=<<)
  infixl 1 >>=
  
  -- from the stdlib
  flip :: (a -> b -> c) -> b -> a -> c
~~~~~~~~

and `return` is a synonym for `pure`.

Unlike Scala, we do not need to bind unit values, or provide a `yield` if we are
returning `()`. For example

{lang="text"}
~~~~~~~~
  for {
    _ <- putStr("hello")
    _ <- putStr(" world")
  } yield ()
~~~~~~~~

translates to

{lang="text"}
~~~~~~~~
  do putStr "hello"
     putStr " world"
~~~~~~~~

Non-monadic values can be bound with the `let` keyword:

{lang="text"}
~~~~~~~~
  nameReturn :: IO String
  nameReturn = do putStr "What is your first name? "
                  first <- getLine
                  putStr "And your last name? "
                  last  <- getLine
                  let full = first ++ " " ++ last
                  putStrLn ("Pleased to meet you, " ++ full ++ "!")
                  return full
~~~~~~~~

Finally, Haskell has typeclass derivation with the `deriving` keyword, the
inspiration for `@scalaz.deriving`. Defining the derivation rules is an advanced
topic, but it is easy to derive a typeclass for an ADT:

{lang="text"}
~~~~~~~~
  data List a = Nil | a :. List a
                deriving (Eq, Ord)
~~~~~~~~


## Modules

Haskell source code is arranged into hierarchical modules with the restriction
that all contents of a `module` must live in a single file. The top of a file
declares the `module` name

{lang="text"}
~~~~~~~~
  module Silly.Tree where
~~~~~~~~

Directories are used on disk to organise the code, so this file would go into
`Silly/Tree.hs`.

By default all symbols in the file are exported but we can choose to export
specific members, for example the `Tree` type and data constructors, and a
`fringe` function, omitting `sapling`:

{lang="text"}
~~~~~~~~
  module Silly.Tree (Tree(Leaf, Branch), fringe) where
  
  data Tree a = Leaf a | Branch (Tree a) (Tree a)
  
  fringe :: Tree a -> [a]
  fringe (Leaf x)            = [x]
  fringe (Branch left right) = fringe left ++ fringe right
  
  sapling :: Tree String
  sapling = Leaf ""
~~~~~~~~

Interestingly, we can export symbols that are imported into the module, allowing
library authors to package up their entire API into a single module, regardless
of how it is implemented.

In a different file we can import all the exported members from `Silly.Tree`

{lang="text"}
~~~~~~~~
  import Silly.Tree
~~~~~~~~

which is roughly equivalent to Scala's `import silly.tree._` syntax. If we want
to restrict the symbols that we import we can provide an explicit list in
parentheses after the import

{lang="text"}
~~~~~~~~
  import Silly.Tree (Tree, fringe)
~~~~~~~~

If we have a name collision on a symbol we can use a `qualified` import, with an
optional list of symbols to import

{lang="text"}
~~~~~~~~
  import qualified Silly.Tree (fringe)
~~~~~~~~

and now to call the `fringe` function we have to type `Silly.Tree.fringe`
instead of just `fringe`. We can also change the name of the module when
importing it

{lang="text"}
~~~~~~~~
  import qualified Silly.Tree as T
~~~~~~~~

The `fringe` function is now `T.fringe`.

Alternatively, rather than select what we want to import, we can choose what to
**not** import

{lang="text"}
~~~~~~~~
  import Silly.Tree hiding (fringe)
~~~~~~~~

By default the `Prelude` module is implicitly imported but if we add an explicit
import from the `Prelude` module, only our version is used. We can use this
technique to hide unsafe legacy functions

{lang="text"}
~~~~~~~~
  import Prelude hiding ((!!), head)
~~~~~~~~

or use a custom prelude and disable the default prelude with a language extension

{lang="text"}
~~~~~~~~
  {-# LANGUAGE NoImplicitPrelude #-}
~~~~~~~~


## Evaluation

Haskell compiles to native code, there is no virtual machine, but there is a
garbage collector. A fundamental aspect of the runtime is that all parameters
are **lazily evaluated** by default. Haskell treats all terms as a promise to
provide a value when needed, called a *thunk*. Thunks get reduced only as much
as necessary to proceed, no more.

A huge advantage of lazy evaluation is that it is much harder to trigger a stack
overflow! A disadvantage is that there is an overhead compared to strict
evaluation, which is why Haskell allows us to opt in to strict evaluation on a
per parameter basis.

Haskell is also nuanced about what strict evaluation means: a term is said to be
in *weak head normal-form* (WHNF) if the outermost code blocks cannot be reduced
further, and *normal form* if the term is fully evaluated. Scala's default
evaluation strategy roughly corresponds to normal form.

For example, these terms are normal form:

{lang="text"}
~~~~~~~~
  42
  (2, "foo")
  \x -> x + 1
~~~~~~~~

whereas these are not in normal form (they can be reduced further):

{lang="text"}
~~~~~~~~
  1 + 2            -- reduces to 3
  (\x -> x + 1) 2  -- reduces to 3
  "foo" ++ "bar"   -- reduces to "foobar"
  (1 + 1, "foo")   -- reduces to (2, "foo")
~~~~~~~~

The following terms are in WHNF because the outer code cannot be reduced further
(even though the inner parts can be):

{lang="text"}
~~~~~~~~
  (1 + 1, "foo")
  \x -> 2 + 2
  'f' : ("oo" ++ "bar")
~~~~~~~~

and the following are not in WHNF

{lang="text"}
~~~~~~~~
  1 + 1              -- reduces to 2
  (\x y -> x + y) 2  -- reduces to \y -> 2 + y
  "foo" ++ "bar"     -- reduces to "foobar"
~~~~~~~~

The default evaluation strategy is to perform no reductions when passing a term
as a parameter. Language level support allows us to request WHNF for any term
with `($!)`

{lang="text"}
~~~~~~~~
  -- evaluates `a` to WHNF, then calls the function with that value
  ($!) :: (a -> b) -> a -> b
  infixr 0
~~~~~~~~

We can use an exclamation mark `!` on `data` parameters

{lang="text"}
~~~~~~~~
  data StrictList t = StrictNil | !t :. !(StrictList t)
  
  data Employee = Employee { name :: !Text, age :: !Int}
~~~~~~~~

The `StrictData` language extension enables strict parameters for all data in
the module.

Another extension, `BangPatterns`, allows `!` to be used on the arguments of
functions. The `Strict` language extension makes all functions and data
parameters in the module strict by default.

Going to the extreme we can use `($!!)` and the `NFData` typeclass for normal
form evaluation:

{lang="text"}
~~~~~~~~
  class NFData a where
    rnf :: a -> ()
  
  ($!!) :: (NFData a) => (a -> b) -> a -> b
~~~~~~~~

which is subject to the availability of an `NFData` instance.

The cost of strictness is that Haskell behaves like any other strict language
and may perform unnecessary work. Opting in to strictness must therefore be done
with great care, and only for measured performance improvements. If in doubt, be
lazy and stick with the defaults.

A> There is a big gotcha with lazy evaluation: if an I/O action is performed that
A> populates a lazy data structure, the action will be performed when the data
A> structure is evaluated, which can fail in unexpected parts of the code and
A> outside of the resource handling logic. To avoid this gotcha, only read into
A> strict data structures when performing I/O.
A> 
A> Thankfully this gotcha only affects developers writing low-level I/O code. Third
A> party libraries such as `pipes-safe` and `conduits` provide safe abstractions
A> for the typical Haskeller. Most raw byte and `Text` primitives are strict, with
A> `Lazy` variants.


## Next Steps

Haskell is a faster, safer and simpler language than Scala and has proven itself
in industry. Consider taking the [data61 course on functional programming](https://github.com/data61/fp-course), and
ask questions in the `#qfpl` chat room on `freenode.net`.

Some additional learning materials are:

-   [Haskell Book](http://haskellbook.com/) a very comprehensive introduction, or [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html) for
    a faster ride.
-   [Parallel and Concurrent Programming in Haskell](http://shop.oreilly.com/product/0636920026365.do) and [What I Wish I Knew When
    Learning Haskell](http://dev.stephendiehl.com/hask/#data-kinds) for intermediate wisdom.
-   [Glasgow Haskell Compiler User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/) and [HaskellWiki](https://wiki.haskell.org) for the cold hard facts.
-   [Eta](https://eta-lang.org/), i.e. Haskell for the JVM.

If you enjoy using Haskell and understand the value that it would bring to your
business, then tell your managers! That way, the small percentage of managers
who commission Haskell projects will be able to attract functional programming
talent from the many teams who do not, and everybody will be happy.



# Lisensi Pihak Ketiga

Beberapa sumber kode pada buku ini disalin dari proyek perangkat lunak
bebas. Lisensi proyek tersebut meminta teks berikut didistribusikan
bersama sumber yang diditunjukkan pada buku ini.


## Lisensi Scala

{lang="text"}
~~~~~~~~
  Copyright (c) 2002-2017 EPFL
  Copyright (c) 2011-2017 Lightbend, Inc.
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of the EPFL nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
~~~~~~~~


## Lisensi Scalaz

{lang="text"}
~~~~~~~~
  Copyright (c) 2009-2014 Tony Morris, Runar Bjarnason, Tom Adams,
                          Kristian Domagala, Brad Clow, Ricky Clarkson,
                          Paul Chiusano, Trygve Laugstøl, Nick Partridge,
                          Jason Zaugg
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of the copyright holder nor the names of
     its contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
~~~~~~~~


