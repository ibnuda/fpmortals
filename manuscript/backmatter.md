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


## Fungsi

Walaupun tidak wajib, menuliskan penanda tipe dari sebuah fungsi secara eksplisit
merupakan kebiasaan yang bagus: nama fungsi diikuti tipenya. Sebagai contoh
`foldl` yang dispesialisasikan untuk senarai berantai

{lang="text"}
~~~~~~~~
  foldl :: (b -> a -> b) -> b -> [a] -> b
~~~~~~~~

Semua fungsi di-*curry*-kan pada Haskell, tiap parameter dipisahkan oleh sebuah
`->` dan tipe paling akhir merupakan tipe kembalian. Penanda tipe diatas ekuvalen
dengan penanda tipe pada Scala:

{lang="text"}
~~~~~~~~
  def foldLeft[A, B](f: (B, A) => B)(b: B)(as: List[A]): B
~~~~~~~~

Bebarapa pengamatan:

-   tidak ada kata kunci
-   tidak diperlukannya tipe yang digunakan
-   tidak diperlukannya nama parameter

yang membuat kode ringkas

Fungsi infiks didefinisikan dalam tanda kurung dan membutuhkan definisi ketetapan:

{lang="text"}
~~~~~~~~
  (++) :: [a] -> [a] -> [a]
  infixr 5 ++
~~~~~~~~

Regular functions can be called in infix position by surrounding their name with
backticks, and an infix function can be called like a regular function if we
keep it surrounded by brackets. The following are equivalent:

Fungsi biasa dapat dipanggil pada posisi infiks dengan mengurung nama fungsi
tersebut dengan tanda petik. Begitu juga dengan fungsi infiks yang bisa dipanggil
sebagaimana fungsi pada umumnya dengan mengurungnya dengan tanda kurung. Berikut
adalah ekuivalen:

{lang="text"}
~~~~~~~~
  a `foo` b
  foo a b
~~~~~~~~

Sebuah fungsi infiks dapat di-*curry*-kan pada bagian kiri maupun kanan yang
sering kali memberikan semantik berbeda:

{lang="text"}
~~~~~~~~
  invert = (1.0 /)
  half   = (/ 2.0)
~~~~~~~~

Fungsi biasanya ditulis dengan parameter yang paling umum sebagai parameter awal,
agar dapat digunakan berulang kali dalam bentuk ter-*curry*.

Definisi dari sebuah fungsi bisa digunakan untuk pencocokan pola, dengan satu
baris untuk tiap kasus. Disinilah kita menamai parameter dengan menggunakan
konstruktor data untuk mengekstrak parameter, seperti klausa `case` milik Scala:

{lang="text"}
~~~~~~~~
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing
~~~~~~~~

Garis bawah merupakan *placeholder* untuk parameter yang diabaikan dan nama
fungsi bisa diletakkan pada posisi infiks:

{lang="text"}
~~~~~~~~
  (<+>) :: Maybe a -> Maybe a -> Maybe a
  Just a <+> _      = Just a
  Empty  <+> Just a = Just a
  Empty  <+> Empty  = Empty
~~~~~~~~

Kita dapat mendefinisikan fungsi lambda anonim dengan sebuah garis miring terbalik,
yang bila kita maksa akan terlihat seperti huruf Yunani λ. Tiap baris berikut
adalah ekuivalen:

{lang="text"}
~~~~~~~~
  (*)
  (\a1 -> \a2 -> a1 * a2)
  (\a1 a2     -> a1 * a2)
~~~~~~~~

Fungsi Haskell yang tercocokkan berdasarkan pola hanya merupakan pemanis sintaks
dari fungsi lambda berlapis. Anggap fungsi sederhana berikut yang membuat sebuah
tupel ketika diberikan tiga buah masukan:

{lang="text"}
~~~~~~~~
  tuple :: a -> b -> c -> (a, b, c)
~~~~~~~~

Implementasi

{lang="text"}
~~~~~~~~
  tuple a b c = (a, b, c)
~~~~~~~~

dijabarkan menjadi

{lang="text"}
~~~~~~~~
  tuple = \a -> \b -> \c -> (a, b, c)
~~~~~~~~

Pada isi sebuah fungsi, kita dapat membuat sebuah penetapan fungsi lokal dengan
menggunkan klausa `let` maupun `where`. Potongan kode berikut merupakan definisi
yang ekuivalen dari map untuk senarai berantai (tanpa petik merupakan nama
identifier yang valid):

{lang="text"}
~~~~~~~~
  map :: (a -> b) -> [a] -> [b]
  
  -- eksplisit
  map f as = foldr map' [] as
             where map' a bs = f a : bs
  
  -- lebih ringkas, menggunakan *curry*
  map f    = foldr map' []
             where map' a = (f a :)
  
  -- penetapan menggunakan let
  map f    = let map' a = (f a :)
             in foldr map' []
  
  -- implementasi sebenarnya
  map _ []       = []
  map f (x : xs) = f x : map f xs
~~~~~~~~

`if` / `then` / `else` merupakan kata kunci untuk statemen kondisional:

{lang="text"}
~~~~~~~~
  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter f (head : tail) = if f head
                           then head : filter f tail
                           else filter f tail
~~~~~~~~

Namun penggunaan *pembatas case* dianggap sebagai gaya superior

{lang="text"}
~~~~~~~~
  filter f (head : tail) | f head    = head : filter f tail
                         | otherwise = filter f tail
~~~~~~~~

Pencocokan pola pada tiap istilah dilakukan dengan menggunakan `case ... of`

{lang="text"}
~~~~~~~~
  unfoldr :: (a -> Maybe (b, a)) -> a -> [b]
  unfoldr f b = case f b of
                  Just (b', a') -> b' : unfoldr f a'
                  Nothing       -> []
~~~~~~~~

Pembatas juga dapat digunakan pada pencocokan. Misalkan, kita ingin mengkhususkan
nol:

{lang="text"}
~~~~~~~~
  unfoldrInt :: (a -> Maybe (Int, a)) -> a -> [Int]
  unfoldrInt f b = case f b of
                     Just (i, a') | i == 0    -> unfoldrInt f a'
                                  | otherwise -> i : unfoldrInt f a'
                     Nothing                  -> []
~~~~~~~~

Dan pada akhirnya, dua fungsi yang patut diperhatikan adalah `($)` dan `(.)`

{lang="text"}
~~~~~~~~
  -- operator aplikasi
  ($) :: (a -> b) -> a -> b
  infixr 0
  
  -- komposisi fungsi
  (.) :: (b -> c) -> (a -> b) -> a -> c
  infixr 9
~~~~~~~~

Kedua fungsi ini merupakan gaya penggunaan alternatif dari penggunaan
tanda kurung berlapis.

Potongan berikut setara:

{lang="text"}
~~~~~~~~
  Just (f a)
  Just $ f a
~~~~~~~~

sebagaimana 

{lang="text"}
~~~~~~~~
  putStrLn (show (1 + 1))
  putStrLn $ show $ 1 + 1
~~~~~~~~

Ada kecenderungan untuk menggunakan komposisi fungsi dengan `.` bila dibandingkan
dengan penggunaan `$` jamak

{lang="text"}
~~~~~~~~
  (putStrLn . show) $ 1 + 1
~~~~~~~~


## Kelas Tipe

Untuk mendefiniskan kelas tipe, kita menggunakan kata kunci `class` yang diteruskan
dengan nama kelas, parameter tipenya, dan anggota yang dibutuhkan pada klausa
`where`. Bila ada ketergantungan antar kelas tipe, misalkan `Applicative`
membutuhkan `Functor`, gunakan notasi `=>`

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

Kita menyediakan implementasi dari kelas tipe dengan kata kunci `instance`. Bila
kita ingin mengulang penanda tipe pada fungsi instans, demi kejelasan, kita
harus menggunakan ekstensi `InstanceSigs`.

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

Bila kita ingin menggunakan kelas tipe pada fungsi, kita menggunakan `=>` pada
penanda tipenya. Sebagai contoh, kita dapat mendefinisikan fungsi yang mirip
dengan `Apply.apply2` milik Scalaz sebagai berikut

{lang="text"}
~~~~~~~~
  apply2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  apply2 f fa fb = f <$> fa <*> fb
~~~~~~~~

Karena kita telah memperkenalkan `Monad`, saatnya memperkenalkan notasi `do`
yang merupakan inspirasi untuk komprehensi *for* Scala:

{lang="text"}
~~~~~~~~
  do
    a <- f
    b <- g
    c <- h
    return (a, b, c)
~~~~~~~~

yang dijabarkan menjadi

{lang="text"}
~~~~~~~~
  f >>= \a ->
    g >>= \b ->
      h >>= \c ->
        return (a, b, c)
~~~~~~~~

dimana `>>=` adalah `=<<` dengan parameter yang dibalik

{lang="text"}
~~~~~~~~
  (>>=) :: Monad f => f a -> (a -> f b) -> f b
  (>>=) = flip (=<<)
  infixl 1 >>=
  
  -- from the stdlib
  flip :: (a -> b -> c) -> b -> a -> c
~~~~~~~~

dan `return` sebagai sinonim untuk `pure`.

Tidak seperti Scala, kita tidak perlu mengikat nilai unit, atau menyediakan `yield`
bilakita mengembalikan `()`. Sebagai contoh

{lang="text"}
~~~~~~~~
  for {
    _ <- putStr("hello")
    _ <- putStr(" world")
  } yield ()
~~~~~~~~

menjadi

{lang="text"}
~~~~~~~~
  do putStr "hello"
     putStr " world"
~~~~~~~~

Nilai non-monadik dapat ditetapkan dengan kata kunci `let`:

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

Haskell juga mempunyai derivasi kelas tipe yang menggunakan kata kunci `deriving`
yang juga merupakan inspirasi untuk `@scalaz.deriving`. Mendefinisikan aturan
derivasi merupakan topik lanjutan. Namun, untuk menderivasi kelas tipe untuk
sebuah tipe data aljabaris sangat mudah:

{lang="text"}
~~~~~~~~
  data List a = Nil | a :. List a
                deriving (Eq, Ord)
~~~~~~~~


## Modul

Sumber kode Haskell diatur menjadi modul hierarkis dengan batasan bahwa semua
konten dari sebuah `module` harus ada pada sebuah berkas. Pada bagian atas sebuah
berkas, nama `module` dideklarasikan

{lang="text"}
~~~~~~~~
  module Silly.Tree where
~~~~~~~~

Direktori digunakan pada diska untuk mengelompokkan kode, jadi berkas ini
harus berada pada `Silly/Tree.hs`.

Secara default, semua simbol pada berkas akan diekspor. Namun, kita dapat
menentukan mana yang akan diekspor. Sebagai contoh, tipe `Tree` dan konstruktor
data, fungsi `fringe`, dan melewatkan `sapling`:

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

Yang menarik adalah, kita dapat mengekspor simbol yang juga diimpor ke modul
tersebut. Hal ini memperkenankan penulis pustaka untuk mengemas seluruh APA
mereka menjadi satu modul, terlepas bagaimana APA tersebut diimplementasikan.

Pada berkas yang berbeda, kita dapat mengimpor semua anggota dari `Silly.Tree`

{lang="text"}
~~~~~~~~
  import Silly.Tree
~~~~~~~~

yang kurang lebih setara dengan sintaks `import silly.tree._` milik Scala. Bila
kita ingin membatasi simbol yang kita impor, kita dapat menyediakan daftar eksplisit
didalam tanda kurung setelah impor tersebut

{lang="text"}
~~~~~~~~
  import Silly.Tree (Tree, fringe)
~~~~~~~~

Bila kita mendapati nama yang bentrok pada sebuah simbol, kita dapat menggunakan
impor `qualified` dengan daftar opsional dari simbol yang diimpor

{lang="text"}
~~~~~~~~
  import qualified Silly.Tree (fringe)
~~~~~~~~

dan sekarang bila kita memanggil fungsi `fringe`, kita dapat menuliskan `Silly.Tree.fringe`
sebagai ganti dari `fringe`. Kita juga dapat mengubah nama modul saat mengimpornya
dengan

{lang="text"}
~~~~~~~~
  import qualified Silly.Tree as T
~~~~~~~~

Fungsi `fringe` sekarang dapat dipanggil dengan `T.fringe`

Bisa juga kita memilih untuk **tidak** mengimpor simbol tertentu

{lang="text"}
~~~~~~~~
  import Silly.Tree hiding (fringe)
~~~~~~~~

Secara default, module `Prelude` selalu diimpor secara implisit. Namun, bila kita
secara eksplisit mengimpor module `Prelude`, maka versi kita yang akan dipakai.
Kita bisa menggunakan teknik ini bila kita ingin menyembunyikan fungsi peninggalan

{lang="text"}
~~~~~~~~
  import Prelude hiding ((!!), head)
~~~~~~~~

atau menggunakan mukadimah (`Prelude`) khusus dan menon-aktifkan mukadimah default
dengan ekstensi bahasa

{lang="text"}
~~~~~~~~
  {-# LANGUAGE NoImplicitPrelude #-}
~~~~~~~~


## Evaluasi

Haskell mengkompilasi kode menjadi kode yang berjalan tanpa mesin virtual. Namun,
ada sebuah pengoleksi sampah. Aspek fundamental dari waktu-jalan Haskell adalah
semua parameter **dievaluasi secara landung** secara default. Haskell hanya menjanjikan
sebuah nilai hanya disediakan bila diperlukan dalam bentuk sub-rutin *thunk*.
*Thunk* hanya dikurangi bila diperlukan.

Keuntungan utama dari evaluasi landung adalah *stack overflow* akan lebih sulit
terpicu. Kerugiannya adalah akan ada beban tambahan bila dibandingkan dengan
evaluasi tegas adalah. Sebagai penyiasatan seperti ini, kita dapat memilih
evaluasi tugas per parameter.

Haskell juga agak sedikit berbeda mengenai arti dari evaluasi tegas: sebuah
istilah dikatakan **weak head normal-form** (WHNF) bila blok kode terluar tidak
dapat direduksi lebih lanjut, dan *normal form* bila sebuah istilah dapat dievaluasi
seutuhnya. Strategi evaluasi default Scala kurang lebih sesuai dengan *normal form*.

Sebagai contoh, istilah berikut merupakan *normal form*:

{lang="text"}
~~~~~~~~
  42
  (2, "foo")
  \x -> x + 1
~~~~~~~~

sedangkan istilah berikut bukan dalam bentuk normal (dapat direduksi lebih lanjut):

{lang="text"}
~~~~~~~~
  1 + 2            -- direduksi menjadi 3
  (\x -> x + 1) 2  -- direduksi menjadi 3
  "foo" ++ "bar"   -- direduksi menjadi "foobar"
  (1 + 1, "foo")   -- direduksi menjadi (2, "foo")
~~~~~~~~

Istilah berikut berbentuk WHNF karena blok kode terluar tidak dapat direduksi
lebih lanjut (walaupun bagian dalam dapat direduksi):

{lang="text"}
~~~~~~~~
  (1 + 1, "foo")
  \x -> 2 + 2
  'f' : ("oo" ++ "bar")
~~~~~~~~

dan potongan berikut tidak dalam bentuk WHNF

{lang="text"}
~~~~~~~~
  1 + 1              -- direduksi menjadi 2
  (\x y -> x + y) 2  -- direduksi menjadi \y -> 2 + y
  "foo" ++ "bar"     -- direduksi menjadi "foobar"
~~~~~~~~

Strategi evaluasi default adalah dengan dengan tidak melakukan reduksi ketika
mengumpankan sebuah istilah sebagai parameter. Dukungan bahasa memperkenankan
kita untuk meminta WHNF untuk semua istilah dengan `($!)`

{lang="text"}
~~~~~~~~
  -- mengevaluasi `a` menjadi WHNF, lalu memanggil fungsi dengan nilai tersebut
  ($!) :: (a -> b) -> a -> b
  infixr 0
~~~~~~~~

Kita juga dapat menggunakan tanda seru `!` pada parameter `data`

{lang="text"}
~~~~~~~~
  data StrictList t = StrictNil | !t :. !(StrictList t)
  
  data Employee = Employee { name :: !Text, age :: !Int}
~~~~~~~~

Ekstensi bahasa `StrictData` menjadikan semua parameter data pada modul menjadi
*tegas*.

Ekstensi lain, `BangPatterns`, memperkenankan `!` digunakan pada argumen fungsi.
Ekstensi bahasa `Strict` membuat semua parameter fungsi dan data pada modul menjadi
*tegas* secara default.

Bila kita *maksa*, kita dapat menggunakan `($!!)` dan kelastipe `NFData` untuk
evaluasi bentuk normal:

{lang="text"}
~~~~~~~~
  class NFData a where
    rnf :: a -> ()
  
  ($!!) :: (NFData a) => (a -> b) -> a -> b
~~~~~~~~

yang menjadi subjek ketersedian dari sebuah instans `NFData`.

Beban dari ketegasan semacam ini adalah Haskell berperilaku sebagaimana bahasa
tegas lainnya dan mungkin saja melakukan tugas yang tak perlu. Memilih ketegasan
harus dilakukan secara hati hati dan setelah dibuktikan adanya peningkatan performa.
Bila masih ragu, mundur saja.

A> Ada jebakan betmen bila kita menggunakan evaluasi landung: bila I/O dilakukan
A> dan menghasilkan struktur data landung, maka perbuatan tersebut akan dilakukan
A> saat struktur data dievaluasi, dan harap diingat bahwa adanya kemungkinan gagal
A> pada bagian kode yang tak terduga dan berada di luar cakupan logika penanganan
A> sumber daya. Untuk menghindari jebakan betmen ini, hanya lakukan pembacaan
A> struktur data tegas saat melakukan I/O.
A> 
A> Untungnya, jebakan betmen ini hanya berlaku bagi pengembang yang menulis kode
A> I/O tingkat rendah. Pustaka pihak ketiga seperti `pipes-safe` dan `conduits`
A> menyediakan abstraksi yang aman untuk pengguna Haskell pada umumnya. Kebanyankan
A> bita mentah dan primitif `Text` memiliki versi default evaluasi tegas, dengan
A> varian landung (`Lazy`).


## Langkah Selanjutnya

Haskell merupakan bahasa yang lebih cepat, aman, dan sederhana bila dibandingkan
dengan Scala. Selain itu, Haskell sudah terbukti di industri. Pertimbangkan untuk
mengambil [kursus pemrograman fungsional data61](https://github.com/data61/fp-course)
dan bertanya pada ruang obrolan `#qfpl` di `freenode.net`.

Beberapa materi pelajaran tambahan adalah:

-   [Haskell Book](http://haskellbook.com/) merupakan materi pengenalan yang sangat
    komprehensif, atau [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html)
    untuk pengantar yang lebih singkat.
    a faster ride.
-   [Parallel and Concurrent Programming in Haskell](http://shop.oreilly.com/product/0636920026365.do) dan [What I Wish I Knew When
    Learning Haskell](http://dev.stephendiehl.com/hask/#data-kinds) untuk petuah bijak.
-   [Glasgow Haskell Compiler User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/) dan [HaskellWiki](https://wiki.haskell.org) untuk dokumentasi mendetail.
-   [Eta](https://eta-lang.org/), Haskell untuk JVM.

Bila pembaca budiman menggunakan Haskell dan memahami nilai yang ditawarkan pada
bisnis pembaca, maka silakan sampaikan kepada manajer. Dengan demikian, beberapa
manajer yang membawahi proyek Haskell akan menarik bakat-bakat pemrograman fungisonal
dari banyak tim yang tidak. Dan pada akhirnya, bapak senang, ibu senang, di sini
senang, di mana senang, semua senang!


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


