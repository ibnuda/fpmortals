
# Komprehensi *For*

Komprehensi `for` pada Scala merupakan abstraksi ideal pada pemrograman fungsional
untuk program-program yang berjalan secara berurutan serta berinteraksi dengan dunia luar.
Lebih lanjut, dikarenakan kita akan menggunakan kata kunci ini secara
intensif, kita akan mempelajari ulang prinsip `for` dan bagaimana
Scalaz membantu kita untuk menulis kode yang lebih bersih.

Bab ini tidak akan membahas bagaimana cara menulis program murni
dan teknik teknik yang bisa diterapkan di basis kode non-PF

## Pemanis Sintaksis

Pada dasarnya, `for` pada Scala hanya merupakan aturan penulisan
ulang sederhana, atau *pemanis sintaksis*, yang tidak memiliki
informasi kontekstual.

Untuk melihat apa yang terjadi pada `for`, kita akan menggunakan fitur
`show` dan `reify` pada REPL untuk mencetak bentuk kode setelah pendugaan
tipe.

{lang="text"}
~~~~~~~~
  scala> import scala.reflect.runtime.universe._
  scala> val a, b, c = Option(1)
  scala> show { reify {
           for { i <- a ; j <- b ; k <- c } yield (i + j + k)
         } }
  
  res:
  $read.a.flatMap(
    ((i) => $read.b.flatMap(
      ((j) => $read.c.map(
        ((k) => i.$plus(j).$plus(k)))))))
~~~~~~~~

Sebagaimana yang terlihat pada potongan kode diatas, terdapat banyak
derau yang disebabkan oleh pemanis sintaksis seperti `+` menjadi `$plus`.
Selain itu, supaya ringkas dan terfokus, kita akan mengabaikan
`show` dan `reify` saat baris REPL berupa `reify>` dan juga akan
merapikan hasil kode secara manual.

{lang="text"}
~~~~~~~~
  reify> for { i <- a ; j <- b ; k <- c } yield (i + j + k)
  
  a.flatMap {
    i => b.flatMap {
      j => c.map {
        k => i + j + k }}}
~~~~~~~~

Yang menjadi patokan adalah, setiap `<-`, biasa disebut *generator*, merupakan
eksekusi `flatMap` yang bisa jadi berisi `flatMap` lain, dengan
generator akhir berupa `map` yang berisi konstruk `yield`.

### Penetapan Nilai

Pada `for`, kita bisa membuat atau menetapkan sebuah nilai tanpa harus
secara spesifik menggunakan `val`.
Dengan kata lain, kita bisa langsung menuliskan `ij = i + j` sebagaimana
pada potongan kode berikut.

{lang="text"}
~~~~~~~~
  reify> for {
           i <- a
           j <- b
           ij = i + j
           k <- c
         } yield (ij + k)
  
  a.flatMap {
    i => b.map { j => (j, i + j) }.flatMap {
      case (j, ij) => c.map {
        k => ij + k }}}
~~~~~~~~

Pada hasil REPL di potongan diatas, selain munculnya `j`, hasil dari
pemetaan (dengan `.map`) `b`, juga muncul `ij` yang merupakan hasil dari
operasi `i + j`. Kedua nilai diatas, `j` dan `ij`, akan dipetakan menggunakan
kode pada `yield`.

Sayangnya, kita tidak dapat melakukan penetapan nilai sebelum generator.
Walau belum diterapkan, hal ini sudah dibicarakan pada:
<https://github.com/scala/bug/issues/907>

{lang="text"}
~~~~~~~~
  scala> for {
           initial = getDefault
           i <- a
         } yield initial + i
  <console>:1: error: '<-' expected but '=' found.
~~~~~~~~

Untuk menyiasatinya kita bisa membuat `val` di luar `for`

{lang="text"}
~~~~~~~~
  scala> val initial = getDefault
  scala> for { i <- a } yield initial + i
~~~~~~~~

atau membuat `Option` sebagai assignment pertama.

{lang="text"}
~~~~~~~~
  scala> for {
           initial <- Option(getDefault)
           i <- a
         } yield initial + i
~~~~~~~~

A> `val` tidak harus berupa penetapan sebuah nilai. `val` bisa berupa
A> apapun yang bisa digunakan pada `case` di pencocokan pola.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> val (first, second) = ("hello", "world")
A>   first: String = hello
A>   second: String = world
A>   
A>   scala> val list: List[Int] = ...
A>   scala> val head :: tail = list
A>   head: Int = 1
A>   tail: List[Int] = List(2, 3)
A> ~~~~~~~~
A> 
A> Hal yang sama juga berlaku untuk assignment pada `for`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> val maybe = Option(("hello", "world"))
A>   scala> for {
A>            entry <- maybe
A>            (first, _) = entry
A>          } yield first
A>   res: Some(hello)
A> ~~~~~~~~
A> 
A> Harap berhati-hati agar tidak ada yang terlewat agar tidak terjadi eksepsi
A> pada saat eksekusi (galat *totalitas*).
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> val a :: tail = list
A>   caught scala.MatchError: List()
A> ~~~~~~~~


### Filter

Bisa juga bila kita menggunakan pernyataan `if` setelah generator
untuk menyaring nilai berdasarkan predikat tertentu.

{lang="text"}
~~~~~~~~
  reify> for {
           i  <- a
           j  <- b
           if i > j
           k  <- c
         } yield (i + j + k)
  
  a.flatMap {
    i => b.withFilter {
      j => i > j }.flatMap {
        j => c.map {
          k => i + j + k }}}
~~~~~~~~

Dahulu kala, Scala menggunakan `filter`. Namun, dikarenakan `Traversable.filter`
selalu membuat koleksi objek baru untuk setiap predikat, dibuatlah `withFilter`
sebagai alternatif.
Patut diperhatikan, kita juga bisa secara tanpa sengaja menggunakan `withFilter`
dengan menambahkan informasi mengenai tipe.
Alasannya, informasi tersebut digunakan untuk case pencocokan pola.

{lang="text"}
~~~~~~~~
  reify> for { i: Int <- a } yield i
  
  a.withFilter {
    case i: Int => true
    case _      => false
  }.map { case i: Int => i }
~~~~~~~~

Sebagaimana penetapan nilai, generator bisa menggunakan pencocokan pola pada persamaan
bagian kiri. Namun berbeda dengan assignment, yang melempar `MatchError` saat terjadi
galat, generator akan *menyaring* operasi tersebut sehingga akan terhindar dari galat.


A> Colok-masuk kompilator [`better-monadic-for`](https://github.com/oleg-py/better-monadic-for)
A> penjabaran pemanis sintaks yang lebih rapi dibandingkan kompilator Scala.
A> Contoh berikut akan diterjemahkan menjadi:
A>
A> {lang="text"}
A> ~~~~~~~~
A>   reify> for { i: Int <- a } yield i
A>   
A>   a.map { (i: Int) => i}
A> ~~~~~~~~
A> 
A> yang lebih efisien dibandingkan pencocokan pola ganda atau penyaringan terbungkam
A> saat waktu jalan.


### For Each

Bila tidak ditemukan `yield`, kompilator akan menggunakan `foreach`
sebagai pengganti `flatMap`.

{lang="text"}
~~~~~~~~
  reify> for { i <- a ; j <- b } println(s"$i $j")
  
  a.foreach { i => b.foreach { j => println(s"$i $j") } }
~~~~~~~~


### Rangkuman

Tidak ada tipe super umum yang mempunyai metoda umum yang digunakan pada
`for`; setiap potongan dikompilasi tersendiri.
Misalkan, ada `trait` umum, kurang lebih akan terlihat sebagai berikut:

{lang="text"}
~~~~~~~~
  trait ForComprehensible[C[_]] {
    def map[A, B](f: A => B): C[B]
    def flatMap[A, B](f: A => C[B]): C[B]
    def withFilter[A](p: A => Boolean): C[A]
    def foreach[A](f: A => Unit): Unit
  }
~~~~~~~~

Adalah mu'bah bila konteks (`C[_]`) dari `for` tidak menyediakan `map`
dan `flatMap` atau metoda lainnya. Jika `scalaz.Bind[T]` tersedia untuk `T`,
`bind` tersebut akan menyediakan apa yang konteks tadi tidak miliki.

A> Acap kali pemrogram terkejut ketika komputasi `Future` di dalam baris
A> komprehensi `for` tidak berjalan secara paralel:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   import scala.concurrent._
A>   import ExecutionContext.Implicits.global
A>   
A>   for {
A>     i <- Future { expensiveCalc() }
A>     j <- Future { anotherExpensiveCalc() }
A>   } yield (i + j)
A> ~~~~~~~~
A> 
A> Musabab dari hal diatas adalah `flatMap` menelurkan `anotherExpensiveCalc`
A> pasti **setelah** `expensiveCalc`. Untuk memastikan bahwa dua komputasi
A> `Future` tersebut berjalan secara paralel, jalankan keduanya
A> di luar `for`.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   val a = Future { expensiveCalc() }
A>   val b = Future { anotherExpensiveCalc() }
A>   for { i <- a ; j <- b } yield (i + j)
A> ~~~~~~~~
A> 
A> Pada dasarnya, komprehensi `for` digunakan untuk mendefinisikan program
A> secara berurutan. Kita akan menunjukkan cara yang jauh lebih unggul
A> dan tepat guna untuk mendefinisikan komputasi paralel di bab selanjutnya.
A> Bisik-bisik: bukan `Future`.


## Senam

Walaupun penulisan kode berurutan untuk komprehensi `for` mudah,
kadang terjadi hal hal yang menyebabkan kita berpikir keras. Bagian
ini berisi contoh-contoh mengenai hal semacam itu dan bagaimana cara kita
menyiasatinya.

### Logika Cadangan

Anggap kata kita memanggil sebuah metoda yang mengembalikan `Option`.
Bila pemanggilan ini gagal, tentu kita ingin ada metoda lain yang menangani
galat tersebut. Seperti saat kita membaca tembolok

{lang="text"}
~~~~~~~~
  def getFromRedis(s: String): Option[String]
  def getFromSql(s: String): Option[String]
  
  getFromRedis(key) orElse getFromSql(key)
~~~~~~~~

Bilamana kita harus munggunakan versi asinkronus dari antarmuka
pemrograman aplikasi,

{lang="text"}
~~~~~~~~
  def getFromRedis(s: String): Future[Option[String]]
  def getFromSql(s: String): Future[Option[String]]
~~~~~~~~

maka kita harus hati hati betul agar jangan sampai menambah pekerjaan karena

{lang="text"}
~~~~~~~~
  for {
    cache <- getFromRedis(key)
    sql   <- getFromSql(key)
  } yield cache orElse sql
~~~~~~~~

will run both queries. We can pattern match on the first result but
the type is wrong

akan menjalankan kedua kueri secara bersamaan. Kita dapat mencocokkan pola
pada hasil pertama namun tipe hasil tersebut salah

{lang="text"}
~~~~~~~~
  for {
    cache <- getFromRedis(key)
    res   <- cache match {
               case Some(_) => cache !!! wrong type !!!
               case None    => getFromSql(key)
             }
  } yield res
~~~~~~~~

Kita harus membuat `Future` dari `cache`

{lang="text"}
~~~~~~~~
  for {
    cache <- getFromRedis(key)
    res   <- cache match {
               case Some(_) => Future.successful(cache)
               case None    => getFromSql(key)
             }
  } yield res
~~~~~~~~

`Future.successful` membuat objek `Future` baru, sebagaimana konstruktor
`Option` maupun `List`.


### Pulang Duluan

Misalkan kita punya sebuah keadaan dimana harus selesai di tengah tengah
dan mengembalikan nilai sukses.

Standar praktik pada OOP ketika kita harus keluar dari komputasi lebih awal
adalah dengan melempar eksepsi

{lang="text"}
~~~~~~~~
  def getA: Int = ...
  
  val a = getA
  require(a > 0, s"$a must be positive")
  a * 10
~~~~~~~~

Yang dapat ditulas ulang secara asinkronus.

{lang="text"}
~~~~~~~~
  def getA: Future[Int] = ...
  def error(msg: String): Future[Nothing] =
    Future.failed(new RuntimeException(msg))
  
  for {
    a <- getA
    b <- if (a <= 0) error(s"$a must be positive")
         else Future.successful(a)
  } yield b * 10
~~~~~~~~

Namun, bila kita ingin keluar lebih awal dari komputasi dengan nilai yang
ok, kode sinkronus yang sederhana semacam ini:

{lang="text"}
~~~~~~~~
  def getB: Int = ...
  
  val a = getA
  if (a <= 0) 0
  else a * getB
~~~~~~~~

ketika diterjemahkan menjadi komprehensi `for` berlapis saat kode
tersebut mempunyai ketergantungan asinkronus:

{lang="text"}
~~~~~~~~
  def getB: Future[Int] = ...
  
  for {
    a <- getA
    c <- if (a <= 0) Future.successful(0)
         else for { b <- getB } yield a * b
  } yield c
~~~~~~~~

A> Jika ada konteks `Monad[T]` implisit untuk `T[_]` maka Scalaz akan
A> memperkenankan kita membuat `T[A]` dari nilai `a: A` dengan memanggil
A> `a.Pure[T]`.
A>
A> Scalaz juga menyediakan `Monad[Future]` dengan tambahan `.pure[Future]` yang
A> memanggil `Future.successful`. Selain `pure` sedikit lebih pendek untuk ditulis
A> `pure` juga mencakup konsep umum yang lebih luas dibandingkan `Future`.
A> Maka dari itu, `pure` lebih direkomendasikan untuk digunakan.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   for {
A>     a <- getA
A>     c <- if (a <= 0) 0.pure[Future]
A>          else for { b <- getB } yield a * b
A>   } yield c
A> ~~~~~~~~


## Jalan Penuh Derita

Sampai saat ini, kita baru membahas ingat mengenai aturaan penulisan ulang
dan belum membahas mengenai `map` dan `flatMap`.
Kadang-kadang, ada kondisi dimana `for` harus berhenti di tengah-tengah. Apa
yang terjadi?

Pada contoh `Option`, `yield` hanya dipanggil jika dan hanya jika `i, j, k`
berhasil terdefinisi.

{lang="text"}
~~~~~~~~
  for {
    i <- a
    j <- b
    k <- c
  } yield (i + j + k)
~~~~~~~~

Misalkan salah satu dari `a, b, c` adalah `None`, akan terjadi hubungan pendek
pada komprehensi tersebut dan nilai `None` akan dikembalikan tanpa memberikan
konteks tentang nilai mana yang berupa `None`.

A> Sering ditemui fungsi yang menerima parameter berupa `Option` walaupun pada
A> kenyataannya, mereka berharap semua parameter mempunya nilai. Layaknya
A> yang kita tahu saat sesuatu tidak berjalan sesuai ekspektasi, kebanyakan
A> orang akan marah marah dan melempar barang; dalam konteks ini eksepsi.
A> Solusi alternatif pada contoh diatas adalah menggunakan komprehensi `for`
A> yang tidak saja menyediakan cara tanpa marah marah, juga memberikan kita
A> *totalitas* yang berarti kita pasti mendapatkan nilai untuk semua
A> parameter yang kita berikan kepada sebuah fungsi.
A>
A> {lang="text"}
A> ~~~~~~~~
A>   def namedThings(
A>     someName  : Option[String],
A>     someNumber: Option[Int]
A>   ): Option[String] = for {
A>     name   <- someName
A>     number <- someNumber
A>   } yield s"$number ${name}s"
A> ~~~~~~~~
A> 
A> yang sebagaimana terlihat pada potongan di atas, ada beberapa kekurangan.
A> Seperti bertele-tele, kaku, dan secara teknis, kurang bagus bila diteruskan.
A> Jika sebuah fungsi berharap setiap masukan mempunyai nilai, maka fungsi
A> tersebut harus secara langsung menyatakan apa yang ia minta dan menyerahkan
A> parameter opsional kepada yang membutuhkan.
A>
A> {lang="text"}
A> ~~~~~~~~
A>   def namedThings(name: String, num: Int) = s"$num ${name}s"
A> ~~~~~~~~

Di sisi lain, bila kita menggunakan `Either`, seperti `None`, `Left` akan
menyebabkan arus-pendek namun memberikan informasi tambahan. Dengan demikian,
`Either` merupakan pilihan yang jauh lebih baik daripada `Option`.

{lang="text"}
~~~~~~~~
  scala> val a = Right(1)
  scala> val b = Right(2)
  scala> val c: Either[String, Int] = Left("sorry, no c")
  scala> for { i <- a ; j <- b ; k <- c } yield (i + j + k)
  
  Left(sorry, no c)
~~~~~~~~

Mari kita lihat apa yang terjadi bila `Future` gagal:

{lang="text"}
~~~~~~~~
  scala> import scala.concurrent._
  scala> import ExecutionContext.Implicits.global
  scala> for {
           i <- Future.failed[Int](new Throwable)
           j <- Future { println("hello") ; 1 }
         } yield (i + j)
  scala> Await.result(f, duration.Duration.Inf)
  caught java.lang.Throwable
~~~~~~~~

`Future` yang bertugas untuk mencetak ke terminal tidak akan pernah dipanggil
sebagaimana `Option` dan `Either` dikarenakan `for` selesai lebih awal.

Penggunaan fungsi-arus-pendek adalah hal yang jamak dilakukan, penting
malah, pada alur kejadian yang tidak menyenangkan.
Hal yang juga patut diperhatikan adalah komprehensi `for` tidak dapat
melakukan melepas sumber daya yang disebabkan tidak ada `try` maupun
`finally`.
Secara prinsip, pemrograman fungsional memancang dengan jelas siapa yang
bertanggung jawab ketika terjadi galat yang tak terduga.
Kewajiban tersebut jatuh kepada konteks eksekusi program, yang biasanya berupa
`Monad`, bukan logika bisnis.

## Ngelantur

Adalah haram untuk mencampur-adukkan konteks saat menggunakan komprehensi `for`
seperti pada cuplikan di bawah.

{lang="text"}
~~~~~~~~
  scala> def option: Option[Int] = ...
  scala> def future: Future[Int] = ...
  scala> for {
           a <- option
           b <- future
         } yield a * b
  <console>:23: error: type mismatch;
   found   : Future[Int]
   required: Option[?]
           b <- future
                ^
~~~~~~~~

Bahkan, ketika kita kode yang ada di depan kita memiliki konteks berlapis,
kompilator acap kali tidak paham maksud kode tersebut.
Walau maksud dari kode tersebut terlihat jelas bagi kita.

{lang="text"}
~~~~~~~~
  scala> def getA: Future[Option[Int]] = ...
  scala> def getB: Future[Option[Int]] = ...
  scala> for {
           a <- getA
           b <- getB
         } yield a * b
                   ^
  <console>:30: error: value * is not a member of Option[Int]
~~~~~~~~

Di atas, kita bermaksud agar `for` mengurus mengenai konteks yang
melapisi `Option` di dalam namun apa yang terjadi? Sesuai yang diduga
kompilator gagal menerka maksud kita.
Yang kita lakukan diatas, menghiraukan konteks bagian luar, biasa
dicapai dengan menggunakan *transformator monad* yang oleh Scalaz
disediakan implementasi untuk `Option` dan `Either` dengan nama
`OptionT` dan `EitherT`.

Pada dasarnya, apapun yang bisa digunakan pada komprehensi `for`
bisa digunakan sebagain konteks bagian luar, selama konsisten
sepanjang komprehensi tersebut.

Kita juga bisa menggunakan `OptionT` untuk mengubah konteks `for` dari
`Future[Option[_]]` menjadi `OptionT[Future, _]` yang ditunjukkan
pada REPL di bawah.

{lang="text"}
~~~~~~~~
  scala> val result = for {
           a <- OptionT(getA)
           b <- OptionT(getB)
         } yield a * b
  result: OptionT[Future, Int] = OptionT(Future(<not completed>))
~~~~~~~~

Dan dengan memanggil `.run`, konteks yang semula berubah akan kembali
muncul.

{lang="text"}
~~~~~~~~
  scala> result.run
  res: Future[Option[Int]] = Future(<not completed>)
~~~~~~~~

Selain itu, transformator moda juga memperkenankan kita untuk menggunakan
`Future[Option[_]]` dengan metoda-metoda yang hanya mengembalikan nilai
`Future` saja melalui `.liftM[OptionT]` yang disediakan oleh Scalaz.
Untuk lebih jelasnya, silakan simak contoh di bawah:

{lang="text"}
~~~~~~~~
  scala> def getC: Future[Int] = ...
  scala> val result = for {
           a <- OptionT(getA)
           b <- OptionT(getB)
           c <- getC.liftM[OptionT]
         } yield a * b / c
  result: OptionT[Future, Int] = OptionT(Future(<not completed>))
~~~~~~~~

terlebih lagi, kita dapat mencampur penggunaan metoda yang mengembalikan
`Option` dengan melapisinya dengan `Future.successful` (`.pure[Future]`,
bila menggunakan Scalaz) dan disambung dengan `OptionT`

{lang="text"}
~~~~~~~~
  scala> def getD: Option[Int] = ...
  scala> val result = for {
           a <- OptionT(getA)
           b <- OptionT(getB)
           c <- getC.liftM[OptionT]
           d <- OptionT(getD.pure[Future])
         } yield (a * b) / (c * d)
  result: OptionT[Future, Int] = OptionT(Future(<not completed>))
~~~~~~~~

Sudah barang tentu dengan mencampur banyak konteks akan menghasilkan
kode yang "berisik". Akan tetapi, hal ini jauh lebih baik bila dibandingkan
dengan menulis `flatMap` dan `map` berlapis secara manual.
Selain itu, kita juga bisa membersihkannya dengan DSL yang menangani
pengubahan-pengubahan yang dibutuhkan agar menjadi `OptionT[Future, _]`.

{lang="text"}
~~~~~~~~
  def liftFutureOption[A](f: Future[Option[A]]) = OptionT(f)
  def liftFuture[A](f: Future[A]) = f.liftM[OptionT]
  def liftOption[A](o: Option[A]) = OptionT(o.pure[Future])
  def lift[A](a: A)               = liftOption(Option(a))
~~~~~~~~

Ditambah lagi, dengan menggunakan operator `|>` yang melewatkan nilai
di sebelah kiri ke fungsi di sebelah kanan operator tersebut, pembatasan
antara logika bisnis dengan transformator monad akan terlihat lebih jelas.

{lang="text"}
~~~~~~~~
  scala> val result = for {
           a <- getA       |> liftFutureOption
           b <- getB       |> liftFutureOption
           c <- getC       |> liftFuture
           d <- getD       |> liftOption
           e <- 10         |> lift
         } yield e * (a * b) / (c * d)
  result: OptionT[Future, Int] = OptionT(Future(<not completed>))
~~~~~~~~

Pendekatan ini juga bisa digunakan untuk `Either` dan transformator lainnya
sebagai konteks bagian dalam. Namun, metoda pengangkatan transformator
lebih kompleks dan membutuhkan parameter tambahan.
Scalaz menyediakan monad transformator bagi kebanyakan tipe yang dimilikinya.
Silakan periksa bila ada.

# Desain Aplikasi

Pada bab ini, kita akan menulis logika bisnis dan tes tes untuk aplikasi
server fungsional murni. Kode sumber untuk aplikasi ini
bisa dilihat pada direktori `example` bersama kode sumber buku ini.
Namun, sangat disarankan untuk tidak membacanya sampai kita sampai di bab akhir
dikarenakan akan ada refaktorisasi sepanjang pembelajaran kita tentang PF.


## Spesifikasi

Aplikasi kita akan mengurus kompilasi-tepat-waktu "build farm" dengan
pendanaan yang sangat mepet. Aplikasi ini akan memperhatikan ke sebuah
server integrasi berkelanjutan [Drone](https://github.com/drone/drone)
dan akan menelurkan "worker agent" menggunakan [Google Container Engine](https://cloud.google.com/container-engine)
(GKE) untuk memenuhi permintaan dari antrian kerja.

{width=60%}
![](images/architecture.png)

Drone menerima kerja ketika kontributor membuat sebuah permintaan tarik
pada github ke proyek yang dimanage. Drone menetapkan beban kerja ke agen-agennya,
dan pada akhirnya, tiap agen akan memproses satu tugas pada satu waktu.

Tujuan dari aplikasi kita adalah memastikan bahwa agen-agen akan selalu
cukup untuk menyelesaikan tugas, dengan batasan-batasan pada jumlah agen
dan pada saat yang bersamaan, menekan biaya keseluruhan. Aplikasi ini
harus tahu jumlah barang yang ada pada *backlog* dan jumlah agen yang tersedia.

Google dapat menelurkan banyak *simpul* yang masing masing mampu mempunyai
beberapa agen drone. Ketika sebuah agen mulai nyala, agen tersebut akan
memberitahu kepada drone yang pada akhirnya mengambil alih siklus hidup
(termasuk panggilan "keep-alive" untuk mendeteksi agen yang telah mati)

GKE menarik biaya berdasarkan uptime dalam hitungan menit, dibulatkan keatas
ke jam terdekat untuk tiap node. Maka dari itu, kita tidak bisa secara
sembarangan untuk menyalakan node baru untuk tiap tugas di antrian kerja.
Kita harus menggunakan ulang node dan memaksa mereka untuk bekerja sampai
sampai ke menit 58 agar tetap ekonomis.

Aplikasi kita harus bisa memulai dan mematikan simpul dan juga memeriksa
status mereka, seperti waktu nyala dan daftar node yang tidak aktif, dan
memastikan waktu saat ini menurut GKE.

Sebagai tambahan, tidak ada API yang secara langsung berkomunikasi ke
sebuah *agen* yang mengakibatkan kita tidak dapat secara tahu secara
langsung apakah sebuah agen sedang melakukan sesuatu untuk drone server
atau tidak. Bila kita mematikan sebuah agen tanpa memastikan bahwa
agen tersebut sedang menganggur, bisa jadi si agen tadi mati di tengah
medan. Tentu sungguh menyesakkan bila harus memulai ulang agen tersebut
secara manual.

Kontributor juga bisa menambahkan agen ke farm secara manual, sehingga
menghitung agen tidak selalu sama dengan node. Kita tidak perlu menambah
node bila ada agen yang tersedia.

Mode gagal harus selalu diambil sebagai opsi paling murah.

Drone dan GKE keduanya mempunya antarmuka JSON yang menggunakan antarmuka
REST dengan otentikasi OAuth 2.0.


## Antarmuka / Aljabar

Pada bab ini, kita akan mengkodifikasi diagram arsitektur pada bagian sebelumnya.
Pertama, karena pada pustaka standar Java maupun Scala tidak memiliki tipe data
timestamp kita akan membuat sebuah tipe data sederhana untuk keperluan ini.

{lang="text"}
~~~~~~~~
  import scala.concurrent.duration._
  
  final case class Epoch(millis: Long) extends AnyVal {
    def +(d: FiniteDuration): Epoch = Epoch(millis + d.toMillis)
    def -(e: Epoch): FiniteDuration = (millis - e.millis).millis
  }
~~~~~~~~

Pada PF, sebuah *aljabar* mempunyai kedudukan yang sama dengan `interface`
di Java yang kurang lebih juga sama dengan pesan-pesan yang dianggap valid
oleh `Actor` Akka. Aljabar ini pula-lah dimana kita mendefinisikan semua
interaksi yang mempunyai efek samping pada sistem kita.

Pada proses desain sistem, kita akan sering melakukan iterasi yang padat saat
menulis logika bisnis dan aljabarnya: hal semacam ini merupakan tingkat abstraksi
yang bagus untuk mendesain sebuah sistem.

{lang="text"}
~~~~~~~~
  trait Drone[F[_]] {
    def getBacklog: F[Int]
    def getAgents: F[Int]
  }
  
  final case class MachineNode(id: String)
  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[Map[MachineNode, Epoch]]
    def start(node: MachineNode): F[MachineNode]
    def stop(node: MachineNode): F[MachineNode]
  }
~~~~~~~~

Di sini, kita menggunakan `NonEmptyList` yang dibuat dengan memanggil `.toNel`
pada tipe data `List` yang ada pada pustaka standar.
Walaupun nilai yang dikembalikan adalah `Option[NonEmptyList]` (karena `List`
bisa saja kosong), hal hal lain tidak berubah.

A> Sesungguhnya, adalah praktik yang ideal untuk menyandikan batasan batasan pada
A> tipe data paramater **dan** nilai kembalian, yang berarti kita tidak akan
A> mengurus kejadian tak mungkin. Walaupun sering kali hal ini tidak sesuai dengan
A> *Hukum Postel* yang menyatakan bahwa, "bila dikasih terima aja."
A> 
A> Walaupun sudah disepakati bahwa parameter harus seumum mungkin, adalah sebuah
A> kenyataan bahwa kita menolak sebuah fungsi harus bisa menerima `Seq` tanpa bisa
A> menerima `Seq` kosong. Bilamana hal tersebut terjadi, tentu yang terjadi adalah
A> sebuah pengecualian yang menghancurkan totalitas dan menyebabkan efek samping
A> yang berlanjut.
A> 
A> Adalah lebih disukai untuk menggunakan `NonEmptyList`, bukan karena ini adalah
A> `List`, tetapi karena propertinya yang tidak boleh kosong.
A> Saat kita mempelajari hierarki kelas tipe dari Scalaz, kita akan
A> tahu cara yang lebih disukai untuk meminta "yang isi".

## Logika Bisnis

Sekarang, kita akan menulis logika bisnis yang menentukan perilaku
dari aplikasi ini, yang saat ini tidak mengindahkan sumber sumber
penderitaan.

Untuk membungkus apa yang kita tahu mengenai situasi saat ini, kita akan
membuat sebuah kelas dengan nama `WorldView` yang apabila kita mendesain
aplikasi ini di Akka, `WorldView` bisa jadi merupakan sebuah `var` pada
sebuah `Actor` yang *dapat berubah-ubah*.

`WorldView` menyatukan semua nilai kembalian dari semua metoda pada
aljabar-aljabar dan menambah sebuah bidang *tertunda* yang ditujukan
untuk menelusuri request mana saja yang belum terpenuhi.

{lang="text"}
~~~~~~~~
  final case class WorldView(
    backlog: Int,
    agents: Int,
    managed: NonEmptyList[MachineNode],
    alive: Map[MachineNode, Epoch],
    pending: Map[MachineNode, Epoch],
    time: Epoch
  )
~~~~~~~~

Walaupun kita sudah siap menulis logika bisnis kita, kita harus menunjukkan
secara eksplisit bahwa kita bergantung pada `Drone` dan `Machines`.

Kita bisa menulis antarmuka untuk logika bisnis

{lang="text"}
~~~~~~~~
  trait DynAgents[F[_]] {
    def initial: F[WorldView]
    def update(old: WorldView): F[WorldView]
    def act(world: WorldView): F[WorldView]
  }
~~~~~~~~

dan mengimplementasikannya dengan sebuah *modul*.
Sebuah modul yang hanya bergantung ke modul-modul lain, aljabar dan fungsi
murni, dan dapat diabstraksikan melalui `F`.  Jika sebuah implementasi dari
sebuah antarmuka aljabaris terikat spesifik
pada sebuah tipe, misalkan `IO`, implementasi tersebut disebut sebagai
sebuah penerjemah

{lang="text"}
~~~~~~~~
  final class DynAgentsModule[F[_]: Monad](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {
~~~~~~~~

Pembatasan konteks `Monad` menunjukkan bahwa `F` bersifat monad, memperkenankan
kita menggunakan `map`, `pure`, dan `flatMap` melalui komprehensi `for`.

Kita punya akses ke aljabar yang dimiliki oleh `Drone` dan `Machines` dengan
simbol `D` dan `M`. Penggunaan simbol satu huruf kapital merupakan ijtima
untuk implementasi monad dan aljabar.

Logika bisnis kita akan berjalan pada sebuah ikalan tak-hingga

{lang="text"}
~~~~~~~~
  state = initial()
  while True:
    state = update(state)
    state = act(state)
~~~~~~~~


### initial

Pada `initial`, kita memanggil semua layanan eksternal dan mengagregasi
semua hasilnya menjadi sebuah `WorldView`.
Untuk nilai bawaan `pending`, kita akan mengisinya dengan sebuah `Map` kosong.

{lang="text"}
~~~~~~~~
  def initial: F[WorldView] = for {
    db <- D.getBacklog
    da <- D.getAgents
    mm <- M.getManaged
    ma <- M.getAlive
    mt <- M.getTime
  } yield WorldView(db, da, mm, ma, Map.empty, mt)
~~~~~~~~

Sebagaimana yang sudah dibahas pada bab 1, `flatMap` memperkenankan kita
melakukan operasi pada nilai yang dihasilkan pada waktu jalan.
Saat kita mengembalikan sebuah `F[_]`, kita mengembalikan sebuah program
lain yang akan diterjemahkan saat waktu secara berurutan dan pada akhirnya,
kita bisa melakukan `flatMap` padanya.
Beginilah cara kita menyambung beberapa kode yang berefek samping yang berurutan
secara aman. Saat itu pula, kita juga bisa menyediakan implementasi murni
untuk tes.

### update

`update` harus memanggil `initial` untuk memperbarui `worldview` kita
sembari mempertahankan tindakan tindakan yang masih `pending`.

Ketika sebuah node mengalami perubahan keadaan, kita akan
menghapusnya dari `pending`. Dan bila sebuah tindakan yang masih tertunda (pending)
masih belum mengerjakan apapun setelah menunggu 10 menit, maka kita
akan menganggapnya sebagai sebuah kegagalan.

{lang="text"}
~~~~~~~~
  def update(old: WorldView): F[WorldView] = for {
    snap <- initial
    changed = symdiff(old.alive.keySet, snap.alive.keySet)
    pending = (old.pending -- changed).filterNot {
      case (_, started) => (snap.time - started) >= 10.minutes
    }
    update = snap.copy(pending = pending)
  } yield update
  
  private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
    (a union b) -- (a intersect b)
~~~~~~~~

Fungsi konkret semacam `.symdiff` tidak memerlukan test dikarenakan mereka
mempunyai masukan dan keluaran yang eksplisit. Sehingga, kita dapat memindahkan
semua kode murni ke metoda metoda mandiri pada `object` independen.
Testing metoda publik akan dengan senang hati kita lakukan.

### act

Metoda `act` sedikit lebih kompleks dibandingkan dengan metoda sebelumnya.
Untuk memperjelas maksud dan memudahkan pemahaman, kita akan membaginya
menjadi dua bagian. Pertama, mendeteksi kapankah sebuah aksi harus diambil.
Dan kedua, melakukan aksi yang sudah ditentukan.
Penyederhanaan ini juga berarti bahwa kita hanya bisa melakukan satu aksi
dalam sekali penyelawatan. Namun, hal tersebut cukup masuk
akal dikarenakan kita dapat mengontrol penyelawatan dan bisa juga menjalankan
ulang `act` sampai tidak ada lagi yang perlu dilakukan.

Sebagai pengekstrak untuk `WorldView`, kita akan menulis pendeteksi skenario
yang tidak lain dan tidak bukan hanyalah penulisan percabangan `if` dan `else`
yang jauh lebih ekspresif dibandingkan yang biasa!

Adalah sebuah keharusan untuk menambah agen ke peternakan bila ada
timbunan pekerjaan, atau saat kita tidak punya agen, atau ketika tak ada
node yang menyala, juga tidak ada aksi aksi yang sedang ditunda
Caranya? Tentu dengan mengembalikan kandidat node yang ingin kita jalankan:

{lang="text"}
~~~~~~~~
  private object NeedsAgent {
    def unapply(world: WorldView): Option[MachineNode] = world match {
      case WorldView(backlog, 0, managed, alive, pending, _)
           if backlog > 0 && alive.isEmpty && pending.isEmpty
             => Option(managed.head)
      case _ => None
    }
  }
~~~~~~~~

Bila tidak ada timbunan pekerjaan, kita harus menghentikan semua node yang
sudah basi (pengangguran / tidak punya pekerjaan). Akan tetapi, jangan lupa
bahwa Google menarik bayaran berdasarkan waktu penggunaan (dalam hitungan jam),
maka kita akan mematikan mesin tersebut pada menit ke 58 agar kita IRIT!
Disini, kita akan mengembalikan daftar non-kosong dari node untuk dihentikan.

Agar IRIT, semua node harus mati sebelum 5 jam.

{lang="text"}
~~~~~~~~
  private object Stale {
    def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] = world match {
      case WorldView(backlog, _, _, alive, pending, time) if alive.nonEmpty =>
        (alive -- pending.keys).collect {
          case (n, started) if backlog == 0 && (time - started).toMinutes % 60 >= 58 => n
          case (n, started) if (time - started) >= 5.hours => n
        }.toList.toNel
  
      case _ => None
    }
  }
~~~~~~~~

Setelah kita berhasil mendeteksi skenario-skenario yang mungkin terjadi,
kita bisa melanjutkan dengan menulis metoda `act`.
Saat kita menjadwalkan sebuah node untuk dijalankan atau dibunuh, kita
bisa menambahkan node tersebut ke `pending` sambil mencatat waktu yang
penjadwalan aksi tadi.

{lang="text"}
~~~~~~~~
  def act(world: WorldView): F[WorldView] = world match {
    case NeedsAgent(node) =>
      for {
        _ <- M.start(node)
        update = world.copy(pending = Map(node -> world.time))
      } yield update
  
    case Stale(nodes) =>
      nodes.foldLeftM(world) { (world, n) =>
        for {
          _ <- M.stop(n)
          update = world.copy(pending = world.pending + (n -> world.time))
        } yield update
      }
  
    case _ => world.pure[F]
  }
~~~~~~~~

Karena `NeedsAgent` dan `Stale` tidak menutup semua kemungkinan yang bisa
terjadi, maka kita butuh jaring pengaman `cace _` yang sebenarnya tidak
melakukan apapun.
Saudara bisa mengingat kembali bab 2 dimana `.pure` menciptakan konteks monadik
dari sebah nilai (`for`).

`foldLeftM` sendiri sebenarnya mirip dengan `foldLeft`. Namun, tiap iterasi
dari penekukan ("fold") bisa saja menghasilkan sebuah nilai monadik.
Pada kasus ini, tiap iterasi dari tiap tekukan mengembalikan `F[WorldView]`.
Simbol `M` pada `M.stop(n)`, misal, melambangkan bahwa ekspresi tersebut
bersifat monadik. Kita akan banyak menemukan banyak metoda "lifted"
seperti ini yang hanya mau menerima nilai nilai monadik, bukan nilai biasa.


## Tes Unit

Pendekatan seperti ini, yang digunakan pada pemrograman fungsional,
adalah hal yang diimpikan oleh seorang arsitek dimana detail implementasi
atas aljabar-aljabar diserahkan kepada anggota tim dan sang arsitek
fokus dalam menentukan logika bisnis untuk memenuhi tuntutan bisnis.

Aplikasi kita ini sangat bergantung pada tempo dan layanan web pihak ketiga.
Bila kita sedang menulis aplikasi ini dengan metodologi OOP tradisionil,
kita akan membuat tiruan untuk semua metoda yang digunakan untuk memanggil
layanan tersebut ataupun aktor aktor untuk pesan pesan keluar.
Peniruan yang digunakan pada pemrograman fungsional dilakukan dengan cara
membuat implementasi alternatif dari aljabar yang digunakan.
Tiap aljabar tiruan tadi, mengisolasi bagian bagian dari sistem yang harus
ditiru.

Kita bisa memulainya dengan data data yang dikhususkan untuk testing.

{lang="text"}
~~~~~~~~
  object Data {
    val node1   = MachineNode("1243d1af-828f-4ba3-9fc0-a19d86852b5a")
    val node2   = MachineNode("550c4943-229e-47b0-b6be-3d686c5f013f")
    val managed = NonEmptyList(node1, node2)
  
    val time1: Epoch = epoch"2017-03-03T18:07:00Z"
    val time2: Epoch = epoch"2017-03-03T18:59:00Z" // +52 mins
    val time3: Epoch = epoch"2017-03-03T19:06:00Z" // +59 mins
    val time4: Epoch = epoch"2017-03-03T23:07:00Z" // +5 hours
  
    val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
  }
  import Data._
~~~~~~~~

A> Penambah string `.epoch`, yang ditulis menggunakan pustaka [contextual](https://github.com/propensive/contextual)
A> milik Jon Pretty, memastikan keamanan saat kompilasi atas pembuatan
A> string dari sebuah tipe.
A>
A> {lang="text"}
A> ~~~~~~~~
A>   import java.time.Instant
A>   object EpochInterpolator extends Verifier[Epoch] {
A>     def check(s: String): Either[(Int, String), Epoch] =
A>       try Right(Epoch(Instant.parse(s).toEpochMilli))
A>       catch { case _ => Left((0, "not in ISO-8601 format")) }
A>   }
A>   implicit class EpochMillisStringContext(sc: StringContext) {
A>     val epoch = Prefix(EpochInterpolator, sc)
A>   }
A> ~~~~~~~~

Kita bisa mengimplementasikan aljabar-aljabar yang akan ditiru dengan
mengekstensi `Drone` dan `Machines` dengan konteks monadik spesifik,
seperti `Id` sebagai contoh konteks yang paling sederhana.

Implementasi tiruan kita hanya memutar ulang sebuah `WorldView` yang tetap.
Kita mengisolasi keadaan sistem kita sehingga kita dapat menggunakan
`var` untuk menyimpan keadaan tersebut.

{lang="text"}
~~~~~~~~
  class Mutable(state: WorldView) {
    var started, stopped: Int = 0
  
    private val D: Drone[Id] = new Drone[Id] {
      def getBacklog: Int = state.backlog
      def getAgents: Int = state.agents
    }
  
    private val M: Machines[Id] = new Machines[Id] {
      def getAlive: Map[MachineNode, Epoch] = state.alive
      def getManaged: NonEmptyList[MachineNode] = state.managed
      def getTime: Epoch = state.time
      def start(node: MachineNode): MachineNode = { started += 1 ; node }
      def stop(node: MachineNode): MachineNode = { stopped += 1 ; node }
    }
  
    val program = new DynAgentsModule[Id](D, M)
  }
~~~~~~~~

A> Kita nanti akan kembali ke kode ini dan mengganti `var` dengan sesuatu
A> yang lebih aman.

Ketika kita menulis sebuah unit tes, kita akan membuat sebuan instans `Mutable`
dan mengimpor semua anggotanya.

Baik `drone` maupun `machines` kita, menggunakan konteks eksekusi
`Id`. Sehingga, program ini akan mengembalikan sebuah `Id[WorldView]` 
yang bisa kita tegaskan.

Sebenarnya, pada kasus remeh seperti ini, kita tinggal memeriksa
apakah metoda `initial` memang betul mengembalikan nilai yang sama
dengan yang kita gunakan dalam implementasi statik.

{lang="text"}
~~~~~~~~
  "Business Logic" should "generate an initial world view" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
  
    program.initial shouldBe needsAgents
  }
~~~~~~~~

Kita juga bisa membuat tes yang lebih rumit untuk metoda `update`
dan `act` untuk membantu kita menghilangkan kutu-kutu dan memperhalus
persyaratan.

{lang="text"}
~~~~~~~~
  it should "remove changed nodes from pending" in {
    val world = WorldView(0, 0, managed, Map(node1 -> time3), Map.empty, time3)
    val mutable = new Mutable(world)
    import mutable._
  
    val old = world.copy(alive = Map.empty,
                         pending = Map(node1 -> time2),
                         time = time2)
    program.update(old) shouldBe world
  }
  
  it should "request agents when needed" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
  
    val expected = needsAgents.copy(
      pending = Map(node1 -> time1)
    )
  
    program.act(needsAgents) shouldBe expected
  
    mutable.stopped shouldBe 0
    mutable.started shouldBe 1
  }
~~~~~~~~

Akan menjadi sebuah kebosanan yang nyata bila kita harus berbincang secara
panjang dan lebar mengenai semua rangkaian tes.
Tes-tes berikut sebenarnya bisa dengan mudah diimplementasikan dengan menggunakan
pendekatan yang sama.

-   jangan meminta agen saat pending.
-   jangan mematikan agen bila node masih muda.
-   matikan agen bila tidak ada timbunan pekerjaan dan node akan makan biaya lagi.
-   jangan matikan agen bila masih ada tindakan yang pending.
-   matikan agen bila tidak ada backlog jika terlalu tua.
-   matikan agen bila sudah tua, termasuk yang sedang mengerjakan sesuatu.
-   abaikan tindakan-tindakan yang tidak responsif saat pemutakhiran.

Semua tes di atas dijalankan secara berurutan dan terisolasi terhadap
ulir penjalan test (yang bisa jadi dijalankan secara
paralel). Bilamana kita mendesain rangkaian tes kita di Akka, tes-tes kita
bisa jadi menjadi korban atas kesewenangan habisnya waktu. Belum lagi
dengan disembunyikannya galat-galat pada berkas log. 

Bukan melebih-lebihkan, namun kenyataan bahwa testing untuk logika
bisnis pada aplikasi kita memang meningkat drastis.
Anggap saja bahwa 90% waktu yang digunakan oleh pengembang aplikasi
bersama dengan pelanggan dihabiskan untuk memperhalus, memperbarui,
dan memperbaiki aturan aturan bisnis ini, tentu yang lainnya merupakan
detail saja.

## Paralel

Saat ini, aplikasi yang sudah kita desain menjalankan metoda-metoda
aljabar secara berurutan. Namun, ada beberapa bagian-bagian yang bisa
dijalankan secara paralel.


### initial

Pada definisi kita atas `initial`, kita dapat meminta semua informasi
yang kita butuhkan pada saat yang sama. Sehingga, kita tidak perlu
melakukan satu kueri dalam satu waktu.

Berbeda halnya dengan `flatMap` untuk operasi berurutan, Scalaz menggunakan
sintaksis `Apply` untuk operasi paralel:

{lang="text"}
~~~~~~~~
  ^^^^(D.getBacklog, D.getAgents, M.getManaged, M.getAlive, M.getTime)
~~~~~~~~

yang bisa juga dituliskan menggunakan notasi infiks:

{lang="text"}
~~~~~~~~
  (D.getBacklog |@| D.getAgents |@| M.getManaged |@| M.getAlive |@| M.getTime)
~~~~~~~~

Bila setiap operasi paralel mengembalikan sebuah nilai pada konteks
monadik yang sama, kita dapat menerapkan sebuah fungsi ke hasil-hasilya
saat mereka kembali.
Metoda `initial` bisa ditulis ulang sebagai berikut.

{lang="text"}
~~~~~~~~
  def initial: F[WorldView] =
    ^^^^(D.getBacklog, D.getAgents, M.getManaged, M.getAlive, M.getTime) {
      case (db, da, mm, ma, mt) => WorldView(db, da, mm, ma, Map.empty, mt)
    }
~~~~~~~~


### act

Pada logika yang saat ini digunakan untuk `act`, kita menghentikan
setiap node secara berurutan sembari menunggu hasil proses penghentian
tersebut, baru melanjutkan penghentian node lainnya.
Padahal, kita dapat menghentikan semua node bersamaan dan dilanjutkan
dengan memutakhirkan `worldview` kita.

Salah satu kekurangan dari cara ini adalah ketika sebuah operasi gagal
dilakukan, maka proses akan berhenti lebih awal sebelum kita memutakhirkan
bidang `pending`.
Sebenarnya kompromi semacam ini masih masuk akal karena metoda `update`
kita akan dengan anggun menangani kondisi dimana sebuah
`node` mati mendadak.

Untuk tipe data `NonEmptyList`, kita butuh sebuah metoda yang mampu
melakukan pemetaan (`map`ping) atas semua elemennya ke sebuah
`F[MachineNode]` dan menghasilkan sebuah `F[NonEmptyList[MachineNode]]`.
Cara ini disebut sebagai `traverse` (pelintang) dan ketika kita melakukan
`flatMap`, kita akan mendapatkan sebuah `NonEmptyList[MachineNode]`
yang bisa kita tangani dengan cara yang sederhana.

{lang="text"}
~~~~~~~~
  for {
    stopped <- nodes.traverse(M.stop)
    updates = stopped.map(_ -> world.time).toList.toMap
    update = world.copy(pending = world.pending ++ updates)
  } yield update
~~~~~~~~

Saya kira, cuplikan diatas lebih mudah dipahami bila dibandingkan
dengan versi yang berurutan.


## Kesimpulan

1.  *aljabar* mendefinisikan antarmuka antar sistem.
2.  *modul* merupakan implementasi dari sebuah aljabar dalam bentuk aljabar lain.
3.  *interpreter* merupakan implementasi konkret dari sebuah aljabar untuk sebuah `F[_]` tetap.
4.  Interpreter tes dapat mengganti bagian bagian yang mempunyai efek samping pada sistem
    dan memberikan cakupan tes yang lebih tinggi.


# Data dan Fungsionalitas

Pada OOP, kita biasa berpikir mengenai data dan fungsionalitas
dalam satu bentuk, kelas. Hierarki kelas berisi metoda-metoda dan
trait yang memaksa bidang bidang data ada pada kelas yang memakainya.
Polimorfisme dari sebuah objek saat waktu-jalan dilihat dengan kacamata
hubungan "merupakan", yang mengkehendaki kelas kelas untuk mewariskan
dari antarmuka-antarmuka umum.
Hal semacam ini bisa dapat memperkeruh bila basis kode menjadi besar.
Tipe data tipe data sederhana bisa jadi kabur batasannya karena ratusan
baris metoda, trait menjadi kacau karena urutan inisiasi yang salah,
dan testing maupun peniruan komponen yang melekat kuat menjadi kelagepan.

PF mengambil pendekatan yang berbeda dengan mendefinisikan data dan
fungsionalitas secara terpisah.
Pada bab ini, kita akan membahas tipe data tipe data dasar dan keuntungan
dari pembatasan diri kita kepada subset Scala.
Kita juga akan menemukan *tipe kelas* sebagai sebuah cara untuk mencapai
polimorfisme waktu-jalan dengan melihat fungsionalitas dari sebuah
struktur data dengan kacamata hubungan "memiliki" bukan hubungan "merupakan".


## Data

Blok bangunan mendasar dari tipe data adalah

-   `final case class` yang juga dikenal sebagai *produk*
-   `sealed abstract class` yang juga dikenal sebagai *ko-produk*
-   `case object` dan `Int`, `Double`, `String` (dll) sebagai *nilai*

tanpa metoda ataupun bidang selain parameter konstruktor.
Kita lebih memilih `abstract class` dibandingkan `trait` dengan alasan
agar mendapatkan kompatibilitas biner dan juga me-makruh-kan pencampuran "mixin" (lol, mixin)

Ketiga tipe data diatas, secara paket, disebut juga dengan *Tipe Data Aljabar* (TDA).

Sebagai contoh, kita menyusun tipe data dari aljabar Boolean `AND` dan `XOR`:
sebuah produk berisi semua tipe yang terdiri darinya, namun sebuah ko-produk
hanya dapat menjadi satu-satunya. Sebagai contoh

-   produk: `ABC = a AND b AND c`
-   ko-produk: `XYZ = x XOR y XOR z`

yang bila ditulis menggunakan Scala

{lang="text"}
~~~~~~~~
  // values
  case object A
  type B = String
  type C = Int
  
  // product
  final case class ABC(a: A.type, b: B, c: C)
  
  // coproduct
  sealed abstract class XYZ
  case object X extends XYZ
  case object Y extends XYZ
  final case class Z(b: B) extends XYZ
~~~~~~~~


### TDA Tergeneralisasi

Ketika kita mengenalkan sebuah parameter tipe ke sebuah ADT, kita
menyebutnya dengan *Tipe Data Aljabar Tergenerealisasi* (Generalised Algebraic Data Type).

`scalaz.IList`, yang merupakan alternatif yang lebih aman dari pustaka
standar `List`, adalah sebuah TDAT:

{lang="text"}
~~~~~~~~
  sealed abstract class IList[A]
  final case class INil[A]() extends IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
~~~~~~~~

Bila sebuah TDA mengacu pada dirinya sendiri, kita menyebutnya sebagai
sebuah *tipe rekursif*. `IList` sendiri merupakan contoh tipe rekursif
karena `ICons` berisi referensi ke sebuah `IList`.


### Fungsi pada TDA

ADTs can contain *pure functions*

TDA bisa berisi fungsi murni

{lang="text"}
~~~~~~~~
  final case class UserConfiguration(accepts: Int => Boolean)
~~~~~~~~

Namun TDA yang berisi fungsi mempunyai beberapa kekurangan karena
ADT tersebut tidak bisa diterjemahkan dengan sempurna ke JVM.
Sebagai contoh, warisan `Serializable`, `hashCode`, `equals`, dan `toString`
tidak berperilaku sebagaimana yang diharapkan.

Dan yang menjadi sebuah kekecewaan adalah, `Serializable` sangat jamak
digunakan oleh *framework* populer walaupun alternatif yang lebih
bagus banyak.
Salah satu jebakan yang umum memakan korban adalah seorang penulis
lupa bahwa `Serializable` bisa jadi berusaha untuk menyerikan fungsi
*closure* secara keseluruhan dan bisa berakibat server rhemuk! (lol, rhemuk)
Kekurangan lain yang mirip juga sama terjadi pada kelas Java peninggalan
jaman dulu seperti `Throwable`, yang bisa saja berisi rujukan pada objek
arbiter.

Kita akan mengeksplorasi alternatif alternatif untuk metoda peninggalan
sejarah saat kita berbincang mengenai pustaka Scalaz pada bab berikutnya.
Tentu dengan mengorbankan interoperabilitas dengan kode kode Scala dan
Java peninggalan sejarah.


### Kelengkapan

Adalah hal yang penting untuk kita menggunakan `sealed abstract class`,
dan tidak hanya `abstract class`, saat kita mendefinisikan sebuah tipe
data.
Dengan menyegel (`seal`) sebuah `class`, kita juga memastikan bahwa
semua sub-tipe-nya harus didefinisikan di berkas yang sama.
Hal ini memberikan kesempatan agar kompilator bisa mengetahui hubungan
mereka, sehingga kompilator bisa memeriksa keluwesan "pattern match"
dan juga pada makro yang menghilangkan plat cetak.  Sebagai contoh,

{lang="text"}
~~~~~~~~
  scala> sealed abstract class Foo
         final case class Bar(flag: Boolean) extends Foo
         final case object Baz extends Foo
  
  scala> def thing(foo: Foo) = foo match {
           case Bar(_) => true
         }
  <console>:14: error: match may not be exhaustive.
  It would fail on the following input: Baz
         def thing(foo: Foo) = foo match {
                               ^
~~~~~~~~

Cuplikan diatas menunjukkan kepada pengembang apa yang telah mereka
rusak ketika menambah sebuah produk baru ke basis kode.
Hal ini terjadi karena kita menggunakan ekstensi `-Xfatal-warnings`
sehingga semua peringatan dari kompilator menjadi galat.

Namun, kompilator juga tidak akan memeriksa kelengkapan bila kelas tidak
tersegel ataupun ada pengaman lain. Misal

{lang="text"}
~~~~~~~~
  scala> def thing(foo: Foo) = foo match {
           case Bar(flag) if flag => true
         }
  
  scala> thing(Baz)
  scala.MatchError: Baz (of class Baz$)
    at .thing(<console>:15)
~~~~~~~~

Agar tetap aman, jangan gunakan pengaman ketika menggunakan tipe tersegel.

Panji [`-Xstrict-patmat-analysis`](https://github.com/scala/scala/pull/5617)
sudah diajukan sebagai peningkatan bahasa untuk menampah pemeriksaan
"pattern match" tambahan (lol, pattern match.)


### Produk dan Koproduk Alternatif

Bentuk lain dari produk adalah tuple yang merupakan sebuah `final case class`
tanpa label.

`(A.type, B, C)` ekuivalen denga `ABC` pada contoh di atas.
Namun, sangat disarankan untuk menggunakan `final case class` ketika
digunakan pada sebuah ADT. Selain karena agak canggung bila tanpa nama,
`case class` juga mempunyai performa yang jauh lebih baik bila dibandingkan
dengan nilai-nilai primitif.

Contoh lain dari ko-produk adalah saat kita melapisi tipe `Either`.

{lang="text"}
~~~~~~~~
  Either[X.type, Either[Y.type, Z]]
~~~~~~~~

equivalent to the `XYZ` sealed abstract class. A cleaner syntax to define
nested `Either` types is to create an alias type ending with a colon,
allowing infix notation with association from the right:

yang ekuivalen dengan kelas abstrak tersegel `XYZ`.
Untuk sintaks yang lebih rapi yang digunakan untuk mendefinisikan
tipe `Either` berlapis, pengguna dapat menggunakan alias tipe yang diakhiri
dengan titik dua. Hal ini memperkenankan penggunaan notasi infiks dengan
asosiasi sebelah kanan:

{lang="text"}
~~~~~~~~
  type |:[L,R] = Either[L, R]
  
  X.type |: Y.type |: Z
~~~~~~~~

Cara ini berguna untuk membuat ko-produk anonim saat kita tidak
dapat meletakkan semua implementasi dalam sebuah berkas kode yang
sama.

{lang="text"}
~~~~~~~~
  type Accepted = String |: Long |: Boolean
~~~~~~~~

Alternatif lain dari ko-produk adalah dengan membuat `sealed abstract class`
khusus dengan definisi `final case class` yang hanya membungkus tipe
yang diinginkan.

{lang="text"}
~~~~~~~~
  sealed abstract class Accepted
  final case class AcceptedString(value: String) extends Accepted
  final case class AcceptedLong(value: Long) extends Accepted
  final case class AcceptedBoolean(value: Boolean) extends Accepted
~~~~~~~~

Pencocokan pola pada bentuk bentuk ko-produk ini bisa jadi sangat boyak.
Hal ini juga yang melatar belakangi eksplorasi [Tipe Gabungan](https://contributors.scala-lang.org/t/733)
pada kompilator baru Scala, Dotty.
Makro seperti [totalitarian](https://github.com/propensive/totalitarian) dan [iotaz](https://github.com/frees-io/iota)
hadir sebagai alternatif untuk menyandikan ko-produk anonim.


### Penyampaian Informasi

Selain digunakan sebagai kontainer untuk informasi bisnis, tipe data
juga bisa digunakan untuk menyandikan batasan. Sebagai contoh,

{lang="text"}
~~~~~~~~
  final case class NonEmptyList[A](head: A, tail: IList[A])
~~~~~~~~

tidak bisa kosong. Hal inilah yang menjadikan `scalaz.NonEmptyList`
sebuah tipe data yang penting walaupun mempunyai informasi yang sama
dengan `IList`.

Tipe produk sering kali berisi tipe yang jauh lebih umum daripada yang diharapkan.
Pada OOP tradisional, hal ini diatasi dengan menggunakan validasi input
dan penegasan.

{lang="text"}
~~~~~~~~
  final case class Person(name: String, age: Int) {
    require(name.nonEmpty && age > 0) // breaks Totality, don't do this!
  }
~~~~~~~~

Sebagai gantinya, kita dapat menggunakan tipe data `Either` untuk menyediakan
`Right[Person]` untuk instans valid. Tidak hanya itu, penggunaan `Either` juga
bisa mengurangi kemungkinan instansiasi yang seharusnya tidak mungkin terjadi.
Pada contoh selanjutnya, harap diperhatikan bahwa konstruktor kelas `Person`
dibuat sebagai final.


{lang="text"}
~~~~~~~~
  final case class Person private(name: String, age: Int)
  object Person {
    def apply(name: String, age: Int): Either[String, Person] = {
      if (name.nonEmpty && age > 0) Right(new Person(name, age))
      else Left(s"bad input: $name, $age")
    }
  }
  
  def welcome(person: Person): String =
    s"${person.name} you look wonderful at ${person.age}!"
  
  for {
    person <- Person("", -1)
  } yield welcome(person)
~~~~~~~~


#### Tipe Data Terrefinasi

Selain dengan menggunakan `Either` sebagaimana yang telah dicontohkan pada
bagian sebelumnya, ada juga cara yang lebih mudah dan rapi yaitu dengan
menggunakan pustaka `refined`.
Pustaka tersebut memberikan batasan batasan untuk tipe data yang bisa
digunakan pada sebuah kelas.
Untuk memasang pustaka tersebut, silakan tambahkan baris berikut pada
`build.sbt`.

{lang="text"}
~~~~~~~~
  libraryDependencies += "eu.timepit" %% "refined-scalaz" % "0.9.2"
~~~~~~~~

dan baris-baris berikut pada kode sumber.

{lang="text"}
~~~~~~~~
  import eu.timepit.refined
  import refined.api.Refined
~~~~~~~~

`Refined` memberikan batasan batasan yang jauh lebih jelas dengan menuliskan
`A Refined B`.

A> Pada Scala, semua tipe dengan dua parameter dapat ditulis sebagai sisipan.
A> Sebagai contoh, `Either[String, Int]` sama dan sebangun dengan `String Either Int`.
A> Menggunakan fakta ini, `Refined` secara konvensi akan digunakan sebagai operator
A> sisipan. Sehingga, `A Refined B` akan terbaca sebagai "Sebuah `A` yang memenuhi
A> kriteria yang didefinisikan oleh `B`."

{lang="text"}
~~~~~~~~
  import refined.numeric.Positive
  import refined.collection.NonEmpty
  
  final case class Person(
    name: String Refined NonEmpty,
    age: Int Refined Positive
  )
~~~~~~~~

Nilai pokok bisa didapatkan dengan memanggil metoda `.value`. Sedangkan
untuk membuat nilai `refined` pada saat waktu jalan, kita bisa menggunakan
`.refineV` yang mengembalikan `Either`.

{lang="text"}
~~~~~~~~
  scala> import refined.refineV
  scala> refineV[NonEmpty]("")
  Left(Predicate isEmpty() did not fail.)
  
  scala> refineV[NonEmpty]("Sam")
  Right(Sam)
~~~~~~~~

Bila kita menambah impor berikut,

{lang="text"}
~~~~~~~~
  import refined.auto._
~~~~~~~~

kita dapat menyusun nilai nilai valid saat waktu kompile dan akan mendapatkan
pesan galat ketika nilai yang disediakan tidak memenuhi kriteria yang diminta.

{lang="text"}
~~~~~~~~
  scala> val sam: String Refined NonEmpty = "Sam"
  Sam
  
  scala> val empty: String Refined NonEmpty = ""
  <console>:21: error: Predicate isEmpty() did not fail.
~~~~~~~~

Untuk kriteria yang lebih kompleks, kita dapat menggunakan aturan `MaxSize` pada
contoh berikut.

{lang="text"}
~~~~~~~~
  import refined.W
  import refined.boolean.And
  import refined.collection.MaxSize
~~~~~~~~

Untuk memenuhi persyaratan bahwa `String` harus tidak kosong dan mempunyai
panjang maksimal 10 karakter, kita bisa menulis sebagai berikut:

{lang="text"}
~~~~~~~~
  type Name = NonEmpty And MaxSize[W.`10`.T]
  
  final case class Person(
    name: String Refined Name,
    age: Int Refined Positive
  )
~~~~~~~~

A> Notasi `W` merupakan kependekan dari *witness*. Sintaksis ini akan jauh lebih
A> sederhana pada Scala 2.13 yang mendukung *tipe literal*.
A>
A> {lang="text"}
A> ~~~~~~~~
A>   type Name = NonEmpty And MaxSize[10]
A> ~~~~~~~~

Bila kita menemui persyaratan-persyaratan yang tidak didukung oleh pustaka
`refined`, kita dapat dengan mudah menyusunnya sendiri.
Sebagai contoh, pada `drone-dynamic-agents`, kita harus memastikan bahwa
sebuah `String` harus mengandung `application/x-www-form-urlencoded`.
Untuk menyusunnya, kita bisa menggunakan pustaka standar *regex* Java.

{lang="text"}
~~~~~~~~
  sealed abstract class UrlEncoded
  object UrlEncoded {
    private[this] val valid: Pattern =
      Pattern.compile("\\A(\\p{Alnum}++|[-.*_+=&]++|%\\p{XDigit}{2})*\\z")
  
    implicit def urlValidate: Validate.Plain[String, UrlEncoded] =
      Validate.fromPredicate(
        s => valid.matcher(s).find(),
        identity,
        new UrlEncoded {}
      )
  }
~~~~~~~~


### Sederhana untuk Dibagi

Dengan tidak berisi fungsionalitas apapun, sangat mungkin sebuah ADT
memiliki ketergantungan yang kecil. Hal ini-lah yang memudahkan kita
untuk berbagi dengan pengembang lain.
Dengan menggunakan bahasa pemodelan data sederhana, interaksi antar
tim inter-disipliner akan lebih mudah dan ketergantungan atas dokumen
tertulis berkurang.

Terlebih lagi, peralatan yang digunakan bisa dibuat dengan mudah agar
dapat menghasilkan atau menggunakan skema dari bahasa pemrograman lain
dan protokol komunikasi.


### Menghitung Kompleksitas

Kompleksitas dari sebuah tipe data diambil dari jumlah nilai yang bisa
ada pada tipe data tersebut. Sebuah tipe data yang bagus mempunyai tingkat
kompleksitas yang rendah bila dibandingkan dengan informasi yang disampaikan.

Nilai-nilai berikut punya kompleksitas yang tetap.

-   `Unit` punya satu nilai.
-   `Boolean` punya dua nilai.
-   `Int` punya 4,294,967,295 nilai.
-   `String` bisa dibilang punya nilai tak hingga.

Untuk mencari kompleksitas dari sebuah produk, kita tinggal mengalikan
kompleksitas dari tiap bagian.

-   `(Boolean, Boolean)` punya 4 nilai (`2*2`)
-   `(Boolean, Boolean, Boolean)` punya 8 nilai (`2*2*2`)

Sedangkan untuk mencari kompleksitas dari sebuah ko-produk, kita tinggal
menambah kompleksitas dari tiap bagian.

-   `(Boolean |: Boolean)` punya 4 nilai (`2+2`)
-   `(Boolean |: Boolean |: Boolean)` punya 6 nilai (`2+2+2`)

Sedangkan untuk mencari kompleksitas dari sebuah GADT, kalikan tiap bagian
dengan kompleksitas dari setiap parameter.

-   `Option[Boolean]` punya 3 nilai, `Some[Boolean]` dan `None` (`2+1`)

Pada pemrograman fungsional, selain fungsi harus *total*, juga harus mempunyai
nilai kembalian untuk semua input, tak pengecualian..
Praktik utama yang digunakan untuk mencapai *totalitas* adalah dengan
mengurangi jumlah input dan output. Sebagai patokan, tanda tanda fungsi
yang tidak didesain dengan seksama adalah ketika kompleksitas dari output
sebuah fungsi lebih besar daripada jumlah perkalian inputnya.


Kompleksitas dari sebuah fungsi total adalah jumlah fungsi yang bisa memenuhi
*signature* dari fungsi tersebut yang dihitung dengan menggunakan output pangkat
input.

-   `Unit => Boolean` punya kompleksitas 2.
-   `Boolean => Boolean` punya kompleksitas 4.
-   `Option[Boolean] => Option[Boolean]` punya kompleksitas 27.
-   `Boolean => Int` dari quintillion jadi sextillion.
-   `Int => Boolean` sedemikian besar bila semua implementasi ditetapkan pada
    sebuah angka unik, tiap implementasi membutuhkan ruang 4 gigabita agar dapat
    direpresentasikan.

Kenyataannya, `Int => Boolean` bisa jadi hanya sebuah fungsi sederhana seperti
`isOdd`, `isEven`, atau `BitSet`. Fungsi ini, ketika digunakan pada sebuah ADT,
bisa diganti dengan menggunakan ko-produk untuk menandai fungsi yang relevan.

Ketika kompleksitas fungsi kita adalah "semua boleh masuk dan semua bisa keluar",
kita harus memberikan tipe data yang terbatas dan proses validasi. etc (lol, help)

Keuntungan lain yang bisa didapat saat kita bisa menghitung kompleksitas penanda
tipe adalah kita bisa mencari penanda tipe yang lebih sederhana dengan
aljabar tingkat SMP.  Untuk menghitung kompleksitasnya, tinggal mengganti

-   `Either[A, B]` dengan `a + b`
-   `(A, B)` dengan `a * b`
-   `A => B` dengan `b ^ a`

dilanjutkan dengan mengurutkan, lalu tinggal konversi balik. Sebagai contoh,
misalkan kita mendesain sebuah kerangka kerja berdasarkan *callbacks* dan
pada akhirnya kita membuat penanda tipe sebagai berikut:

{lang="text"}
~~~~~~~~
  (A => C) => ((B => C) => C)
~~~~~~~~

Yang bisa kita konversi dan atur ulang sebagai

{lang="text"}
~~~~~~~~
  (c ^ (c ^ b)) ^ (c ^ a)
  = c ^ ((c ^ b) * (c ^ a))
  = c ^ (c ^ (a + b))
~~~~~~~~

dan pada akhirnya, kita bisa konversi ulang ke tipe dan mendapat:

{lang="text"}
~~~~~~~~
  (Either[A, B] => C) => C
~~~~~~~~

yang jauh lebih sederhana. Kita cuma perlu untuk menyuruh pengguna untuk
menyediakan `Either[A, B] => C`.

Dengan penalaran yang sama, kita bisa membuktikan bahwa

{lang="text"}
~~~~~~~~
  A => B => C
~~~~~~~~

ekuivalen dengan

{lang="text"}
~~~~~~~~
  (A, B) => C
~~~~~~~~

yang dikenal dengan *Currying* (lol, bikin kare. tapi tidak sopan, nama orang.)


### Pilih Koproduk, bukan Produk

Sebuah masalah pemodelan dasar yang sering kali muncul adalah ketika
ada beberapa parameter konfigurasi yang saling ekslusif yang sebut saja
`a`, `b`, dan `c`.
Produk `(a: Boolean, b: Boolean, c: Boolean)` punya kompleksitas 8
sedangkan ko-produk

{lang="text"}
~~~~~~~~
  sealed abstract class Config
  object Config {
    case object A extends Config
    case object B extends Config
    case object C extends Config
  }
~~~~~~~~

punya kompleksitas 3. Sebagaimana yang telah ditunjukkan di atas,
adalah lebih disukai untuk memodelkan parameter konfigurasi ini
sebagai ko-produk bila dibandingkan dengan memberikan kemungkinan
5 kondisi invalid terjadi.

Kompleksitas dari sebuah tipe data juga mempunyai implikasi pada testing.
Di lapangan, adalah hal yang mustahil untuk memeriksa semua input yang
mungkin terjadi untuk sebuah fungsi. Sebaliknya, dengan mengecek sedikit
sampel dari sebuah tipe data dengan [Scalacheck](https://www.scalacheck.org)
jauh lebih mudah.
Bila sebuah sampel dari sebuah tipe data punya probabilitas valid rendah,
hal tersebut merupakan pertanda bahwa pemodelan data dilakukan secara
kurang tepat.


### Pengoptimalan

Keuntungan yang sangat terasa saat menggunakan subset sederhana
Scala untuk merepresentasikan tipe data adalah *tooling* dapat
melakukan optimisasi atas representasi *bytecode* JVM.

Sebagai contoh, kita dapat mengemas bidang `Boolean` dan `Option` ke dalam
sebuah `Array[Byte]`, menyimpan nilai di tembolok, memoisasi `hashCode`,
optimisasi `equals`, menggunakan statemen `@switch` pada saat pattern match,
dan banyak lagi.

Pengoptimalan semacam ini tidak bisa diterapkan pada hierarki `class` di
OOP yang juga mengatur "state", melempar eksepsi, ataupun menyediakan
implementasi metoda adhoc.

## Fungsionalitas

Fungsi murni biasanya didefinisikan sebagai metoda pada sebuah objek.

{lang="text"}
~~~~~~~~
  package object math {
    def sin(x: Double): Double = java.lang.Math.sin(x)
    ...
  }
  
  math.sin(1.0)
~~~~~~~~

Sebagaimana yang kita lihat pada cuplikan di atas, penggunaan metoda
pada object bisa jadi terlihat kikuk. Selain karena terbaca dari dalam
ke luar (bukan kiri ke kanan), juga karena objek tersebut menggunakan
"namespace".
Bila kita mendefinisikan `sin(t: T)` di tempat lain, kita akan mendapat
galat *referensi ambigu*. Bila pembaca pernah mengalami masalah saat
menggunakan metoda statik dan metoda kelas pada Java, hal yang sama
juga terjadi bila menggunakan metoda objek pada Scala.

W> Sesungguhnya, pengembang yang meletakkan metoda pada sebuah `trait`
W> dan menyebabkan pengguna menggunakan *cake pattern* akan menerima
W> azab pedih! Penggunaan semacam ini selain tidak rapi, juga menyebabkan
W> kebocoran detail implementasi ke API publik, bytecode gemuk, kompatibilitas
W> binari menjadi tidak mungkin dilakukan, dan membingungkan pengkomplet
W> automatis dari IDE.

Dengan menggunakan fitur `implicit class` dan sedikit plat cetak, kita dapat
menggunakan gaya penulisan yang familiar:

{lang="text"}
~~~~~~~~
  scala> implicit class DoubleOps(x: Double) {
           def sin: Double = math.sin(x)
         }
  
  scala> (1.0).sin
  res: Double = 0.8414709848078965
~~~~~~~~

Sering kali, lebih disukai bila kita melewatkan pendefinisian `object`
dan langsung mendefinisikan `implicit class` untuk mengurangi plat cetak:

{lang="text"}
~~~~~~~~
  implicit class DoubleOps(x: Double) {
    def sin: Double = java.lang.Math.sin(x)
  }
~~~~~~~~

A> `implicit class` merupakan pemanis sintaksis untuk konversi implisit:
A>
A> {lang="text"}
A> ~~~~~~~~
A>   implicit def DoubleOps(x: Double): DoubleOps = new DoubleOps(x)
A>   class DoubleOps(x: Double) {
A>     def sin: Double = java.lang.Math.sin(x)
A>   }
A> ~~~~~~~~
A> 
A> Walaupun mempunyai kekurangan yaitu konstruksi `DoubleOps` yang langsung
A> dibuang bila selesai dipanggil untuk tiap kali pemanggulan.
A> Hal ini bisa memberikan beban tambahan untuk GC.
A>
A> Untuk metode yang lebih disukai, walaupun sedikit lebih lantung, tanpa
A> alokasi memori adalah sebagai berikut.
A>
A> {lang="text"}
A> ~~~~~~~~
A>   implicit final class DoubleOps(private val x: Double) extends AnyVal {
A>     def sin: Double = java.lang.Math.sin(x)
A>   }
A> ~~~~~~~~


### Fungsi Polimorfis

Jenis fungsi yang lebih umum adalah fungsi polimorfis yang biasa
ada pada sebuah *kelas tipe*. Sebuah tipe kelas merupakan ciri
yang:

-   tidak berisi keadaan.
-   mempunyai parameter tipe.
-   mempunyai, setidaknya, satu metoda abstrak (*kombinator primitif*).
-   mungkin berisi metoda yang terumumkan (*kombinator turunan*).
-   mungkin berupa perpanjangan dari *kelas tipe* lain.

Untuk semua tipe parameter, hanya boleh ada satu implementasi kelas tipe.
Properti ini dikenal sebagai *koherensi kelas tipe*. Kelas tipe secara
sekilas, terlihat seperti antarmuka aljabaris di bab sebelumnya. Namun,
aljabar tidak harus koheren.

A> Koherensi kelas tipe pada utamanya adalah mengenai konsistensi. Dan, konsistensi-lah
A> yang memberikan kita kepercayaan diri untuk menggunakan parameter `implicit`.
A> Penalaran mengenai kode akan sulit bila kode tersebut berperilaku berbeda
A> saat impor implisit pada cakupan sumber kode tersebut juga berbeda.
A> Koherensi kelas tipe juga-lah yang menentukan bahwa impor tidak boleh
A> mempengaruhi perilaku kode.
A>
A> Sebagai tambahan, koherensi kelas tipe memberikan kita jalan untuk menyimpan
A> implisit ke tembolok secara global pada saat waktu jalan. Selain itu, alokasi
A> memori yang lebih rendah dan peningkatan performa yang disebabkan oleh GC
A> yang santai juga merupakan bonus menarik karena penggunaan koherensi kelas tipe.

Pustaka standar Scala juga berisi kelas tipe. Kita akan mengeksplorasi
`scala.math.Numeric` yang disederhanakan untuk menunjukkan prinsip prinsip
dari kelas tipe:

{lang="text"}
~~~~~~~~
  trait Ordering[T] {
    def compare(x: T, y: T): Int
  
    def lt(x: T, y: T): Boolean = compare(x, y) < 0
    def gt(x: T, y: T): Boolean = compare(x, y) > 0
  }
  
  trait Numeric[T] extends Ordering[T] {
    def plus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
    def zero: T
  
    def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  }
~~~~~~~~

Kita dapat melihat semua fitur utama dari sebuah kelas tipe pada
cuplikan kode di atas:

-   Tidak ada state (lol, state).
-   `Ordering` dan `Numeric` mempunyai parameter tipe `T`.
-   `Ordering` mempunyai metoda abstrak `compare` dan `Numeric` mempunya metoda
    abstrak `plus`, `times`, `negate`, dan `zero`.
-   `Ordering` mendefinisikan metoda `lt` dan `gt` yang sudah digeneralisasi
    yang didasarkan pada `compare`. `Numeric` mendefinisikan `abs` dengan
    menggunakan `lt`, `negate`, dan `zero`.
-   `Numeric` merupakan perpanjangan dari `Ordering`.

Sekarang kita dapat membuat fungsi untuk tipe yang memiliki kelas tipe `Numeric`:

{lang="text"}
~~~~~~~~
  def signOfTheTimes[T](t: T)(implicit N: Numeric[T]): T = {
    import N._
    times(negate(abs(t)), t)
  }
~~~~~~~~

Kita tidak lagi bergantung kepada hierarki OOP untuk tipe input kita.
Dengan kata lain, kita tidak meminta input kita "merupakan sebuah"
`Numeric`. Hal ini sangat penting bila kita ingin mendukung kelas dari
pihak ketiga yang tidak mungkin kita definisikan ulang.

Keuntungan lain dari kelas tipe adalah pengasosiasian fungsionalitas
ke data dilakukan saat kompilasi. Hal yang berbeda terjadi pada OOP
dimana dilakukan "dynamic dispatch" pada wakut jalan.

Sebagai contoh, dimana kelas `List` hanya bisa mempunya satu implementasi
sebuah metoda, sebuah metoda kelas tipe bisa memberikan kita beberapa
implementasi yang begantung pada konten `List`.
Sehingga, terjadi pemindahan beban kerja dari waktu jalan ke waktu kompilasi.


### Sintaks

Ada beberapa hal yang bisa dirapikan pada sintaks `signOfTheTimes`
yang terlihat kikuk.

Pengguna hilir akan lebih senang bila mereka dapat melihat metoda kita
menggunakan *konteks terikat* karena penanda dapat terbaca
dengan jelas bahwa metoda tersebut menerima parameter `T` yang,
misal, merupakan `Numeric`

{lang="text"}
~~~~~~~~
  def signOfTheTimes[T: Numeric](t: T): T = ...
~~~~~~~~

walaupun hal itu berarti kita harus selalu menggunakan `implicitly[Numeric[T]]`.
Dengan mendefinisikan boilerplate (lol) pada kelas tipe,

{lang="text"}
~~~~~~~~
  object Numeric {
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric
  }
~~~~~~~~

kita bisa mengurangi derau untuk `implicit`.

{lang="text"}
~~~~~~~~
  def signOfTheTimes[T: Numeric](t: T): T = {
    val N = Numeric[T]
    import N._
    times(negate(abs(t)), t)
  }
~~~~~~~~

Namun hal semacam ini tetap saja buruk bagi kita sebagai pengimplementasi.
Kita mempunyai masalah sintaksis dari metoda statik dalam-ke-luar atau
metoda kelas. Kita menangani hal ini dengan memperkenalkan `ops` pada
objek pendamping kelas tipe:

{lang="text"}
~~~~~~~~
  object Numeric {
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric
  
    object ops {
      implicit class NumericOps[T](t: T)(implicit N: Numeric[T]) {
        def +(o: T): T = N.plus(t, o)
        def *(o: T): T = N.times(t, o)
        def unary_-: T = N.negate(t)
        def abs: T = N.abs(t)
  
        // duplicated from Ordering.ops
        def <(o: T): T = N.lt(t, o)
        def >(o: T): T = N.gt(t, o)
      }
    }
  }
~~~~~~~~

Harap diperhatikan bahwa `-x` akan dijabarkan menjadi `x.unary_-` oleh
pemanis sintaksis kompilator. Oleh karena itu, kita mendefinisikan `unary_-`
sebagai sebuah metode perpanjangan. Sekarang, kita dapat menulis `signOfTheTimes`
dengan lebih rapi:

{lang="text"}
~~~~~~~~
  import Numeric.ops._
  def signOfTheTimes[T: Numeric](t: T): T = -(t.abs) * t
~~~~~~~~

Langkah langkah diatas mungkin tidak perlu dilakukan bila menggunakan [Simulacrum](https://github.com/mpilquist/simulacrum)
yang menyediakan anotasi makro `@typeclass` yang secara otomatis menghasilkan
`apply` dan `ops`. Pustaka ini juga menyediakan cara agar kita dapat mendefinisikan
nama alternatif (yang biasanya berupa simbol) untuk metoda-metoda umum.
Untuk lebih lengkapnya, bisa dilihat potongan kode berikut:

{lang="text"}
~~~~~~~~
  import simulacrum._
  
  @typeclass trait Ordering[T] {
    def compare(x: T, y: T): Int
    @op("<") def lt(x: T, y: T): Boolean = compare(x, y) < 0
    @op(">") def gt(x: T, y: T): Boolean = compare(x, y) > 0
  }
  
  @typeclass trait Numeric[T] extends Ordering[T] {
    @op("+") def plus(x: T, y: T): T
    @op("*") def times(x: T, y: T): T
    @op("unary_-") def negate(x: T): T
    def zero: T
    def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  }
  
  import Numeric.ops._
  def signOfTheTimes[T: Numeric](t: T): T = -(t.abs) * t
~~~~~~~~

Saat ada simbol buatan `@op`, simbol ini diucapkan seperti nama metoda-nya.
Misalkan simbol `<` disebut sebagai "kurang dari", bukan "kurung " (lol, apa ya?)

### Instans

*Instans* dari `Numeric` (yang juga merupakan instans dari `Ordering`)
didefinisikan sebagai sebuah `implicit val` (nilai implicit) yang merupakan
perpanjangan dari kelas tipe dan dapat menyediakan implementasi teroptimisasi
dari metoda tergeneralisasi:

{lang="text"}
~~~~~~~~
  implicit val NumericDouble: Numeric[Double] = new Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def zero: Double = 0.0
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  
    // optimised
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def abs(x: Double): Double = java.lang.Math.abs(x)
  }
~~~~~~~~

Walaupun kita menggunakan operator `+`, `*`, `unary_-`, `<`, dan `>`,
metoda-metoda tersebut sebenarnya sudah ada pada `Double`. Metoda kelas
biasanya lebih disukai daripada metoda perpanjangan. Dan faktanya,
kompilator Scala melakukan penanganan khusus untuk primitif dan mengubah
metoda ini menjadi instruksi bytecode asli seperti `dadd`, `dmul`, `dcmpl`,
dan `dcmpg`.

Kita juga bisa mengimplementasikan `Numeric` untuk kelas `BigDecimal` milik
Java (bukan `scala.BigDecimal` yang [rhemuk](https://github.com/scala/bug/issues/9670))

{lang="text"}
~~~~~~~~
  import java.math.{ BigDecimal => BD }
  
  implicit val NumericBD: Numeric[BD] = new Numeric[BD] {
    def plus(x: BD, y: BD): BD = x.add(y)
    def times(x: BD, y: BD): BD = x.multiply(y)
    def negate(x: BD): BD = x.negate
    def zero: BD = BD.ZERO
    def compare(x: BD, y: BD): Int = x.compareTo(y)
  }
~~~~~~~~

Kita bisa membuat struktur data kita sendiri untuk bilangan kompleks:

{lang="text"}
~~~~~~~~
  final case class Complex[T](r: T, i: T)
~~~~~~~~

Dan menurunkan `Numeric[Complex[T]]` bila `Numeric[T]` sudah ada.
Karena instans ini bergantung pada parameter tipe, penurunan ini menggunakan
`def`, bukan `val`.

{lang="text"}
~~~~~~~~
  implicit def numericComplex[T: Numeric]: Numeric[Complex[T]] =
    new Numeric[Complex[T]] {
      type CT = Complex[T]
      def plus(x: CT, y: CT): CT = Complex(x.r + y.r, x.i + y.i)
      def times(x: CT, y: CT): CT =
        Complex(x.r * y.r + (-x.i * y.i), x.r * y.i + x.i * y.r)
      def negate(x: CT): CT = Complex(-x.r, -x.i)
      def zero: CT = Complex(Numeric[T].zero, Numeric[T].zero)
      def compare(x: CT, y: CT): Int = {
        val real = (Numeric[T].compare(x.r, y.r))
        if (real != 0) real
        else Numeric[T].compare(x.i, y.i)
      }
    }
~~~~~~~~

Pembaca yang jeli mungkin memperhatikan bahwa `abs` tidak sesuai
dengan apa yang matematikawan harapkan. Nilai kembalian untuk `abs`
seharusnya berupa `T`, bukan `Complex[T]`.

`scala.math.Numeric` mencoba untuk melakukan terlalu banyak hal dan
tidak tergeneralisasi diluar bilangan nyata. Hal ini bisa jadi pelajaran
yang bagus bahwa kelas tipe yang kecil dan terdefinisi dengan baik sering
kali lebih baik daripada koleksi monolitik yang terdiri dari fitur
fitur yang terlalu spesifik.

### Resolusi Implisit

Kita sudah mendiskusikan mengenai implisit secara panjang lebar.
Bagian ini akan berbicara mengenai apakah implisit itu dan bagaimana
cara mereka bekerja.

*Parameter implisit* adalah saat sebuah metoda meminta instan khusus
dari sebuah tipe tertentu yang ada pada *cakupan implisit* dari pemanggil
dengan sintaks khusus untuk instans kelas tipe. Parameter implisit
merupakan cara yang lebih rapi dalam menggalur konfigurasi pada sebuah
aplikasi.

Pada contoh ini, `foo` meminta instans dari `Numeric` dan `Typeable` yang
tersedia untuk `A` dan juga sebuah objek `Handler` implisit yang meminta
dua parameter.

{lang="text"}
~~~~~~~~
  def foo[A: Numeric: Typeable](implicit A: Handler[String, A]) = ...
~~~~~~~~

*Konversi implisit* adalah ketika sebuah `implicit def` ada. Salah
satu penggunaan konversi implisit adalah untuk pembuatan perpanjangan
metodologi. Ketika kompilator menyelesaikan pemanggilan sebuah metoda,
kompilator pertama tama akan memeriksa apakah metoda tersebut ada pada
tipe, yang dilanjutkan kepada bapaknya (aturan yang mirip dengan Java).
Bila gagal menemukan yang cocok, kompilator akan mencari *cakupan implisit*
untuk konversi ke tipe lain. Baru dilanjutkan dengan pencarian
untuk tipe-tipe tersebut.

Penggunaan lain untuk konversi implisit adalah dengan *derivasi kelas tipe*.
Pada bagian sebelumnya, kita menulis sebuah `implicit def` yang diturunkan dari `Numeric[Complex[T]]`
bila sebuah `Numeric[T]` ada pada cakupan implisit. Adalah sebuah hal
yang mungkin untuk merangkai banyak `implicit def` (juga secara rekursif).
Hal ini juga merupakan basis dari *pemrograman dengan tipe* yang
memindahkan komputasi untuk dilakukan pada saat kompilasi daripada
saat waktu jalan.

Perekat yang menggabungkan parameter implisit dengan konversi implisit
adalah resolusi implisit.

Pertama, cakupan variabel normal dicari dengan urutan:

-   cakupan lokal, termasuk impor tercakup. (mis, blok atau metoda)
-   cakupan luar, termasuk impor tercakup. (mis, anggota pada kelas)
-   kelas orangtua
-   objek dari paket saat ini.
-   objek dari kelas orang tua.
-   impor pada berkas.

Bila semua gagal mencari yang cocok, maka pencarian pada cakupan khusus
akan dilakukan. Pencarian ini dikhususkan untuk instans implisit yang ada
pada objek pasangan, objek paket, objek luar (bila berlapis), dan diulang
untuk ancestor (lol). Pencarian ini dilakukan dengan urutan sebagai berikut:

-   tipe parameter yang ada.
-   tipe parameter yang diminta.
-   parameter tipe (bila ada).

Bila ada dua implisit yang sesuai diketemukan pada resolusi implisit
yang sama, galat *implisit ambigu* akan dilempar.

Implisit seringkali didefinisikan pada sebuah `trait`, yang biasanya
akan diperpanjang oleh sebuah objek. Hal ini dilakukan untuk mengotrol
prioritas dari sebuah implisit, relatif terhadap implisit lain yang lebih
spesifik, untuk mencegah implisit yang ambigu.

Spesifikasi Bahasa Scala cenderung kabur untuk kasus kasus yang kurang umum
dan implementasi kompilator-lah yang menjadi standar de-fakto. Ada beberapa
patokan yang akan kita gunakan sepanjang buku ini. Misalkan, kita akan
memilih `implicit val` dibandingkan `implicit object` meskipun akan ada
godaan untuk menulis lebih pendek. ini adalah [perilaku unik atas resolusi implisit](https://github.com/scala/bug/issues/10411)
yang memperlakukan `imlicit object` tidak sama saat memperlakukan `implicit val`.

Resolusi implisit gagal saat ada hierarki kelas tipe seperti `Ordering` dan `Numeric`.
Bila kita menulis fungsi yang mengambil sebuah `Ordering` implicit, dan kita memanggilnya
untuk sebuah tipe primitif yang punya instans `Numeric` yang terdefinisi pada pasangan `Numeric`,
kompilator akan gagal mencarinya.

Resolusi implisit seringkali untung-untungan [bila kelas tipe digunakan](https://github.com/scala/bug/issues/10582)
saat *bentuk* dari parameter imlisit berubah. Sebagai contoh, sebuah parameter
implisit menggunakan sebuah alias seperti `type Values[A] = List[Option[A]]` mungkin
akan gagal untuk mencari implisit yang definisikan sebagai `List[Option[A]]`.
Hal ini disebabkan karena bentuknya berubah dari *thing of things* dari `A`
menjadi *thing* dari `A`.


## Memodelkan OAuth2

Kita akan menutup bab ini dengan contoh praktikal dari pemodelan data dan
derivasi kelas tipe dan aljabar / desain modul dari bab sebelumnya.

Pada aplikasi `drone-dynamic-agents` kita, untuk berkomunikasi dengan Drone
dan Google Cloud, kita harus menggunakan JSON dengan REST. Kedua layanan tersebut
menggunakan [OAuth2](https://tools.ietf.org/html/rfc6749) untuk otentikasi.
Ada banyak dalam interpretasi OAuth2, namun kita akan fokus pada versi yang
cocok untuk Google Cloud. Bahkan, versi untuk Drone jauh lebih sederhana.


### Deskripsi

Setiap aplikasi Google Cloud mengharuskan kita untuk mengatur *OAuth 2.0 Client Key*
pada

{lang="text"}
~~~~~~~~
  https://console.developers.google.com/apis/credentials?project={PROJECT_ID}
~~~~~~~~

Mendapatkan *Client ID* dan *Client secret*.

Lalu, aplikasi bisa mendapatkan satu *kode* setelah pengguna melakukan
*Permintaan Otorisasi* pada peramban mereka. Kita harus membuka laman
berikut pada peramban:

{lang="text"}
~~~~~~~~
  https://accounts.google.com/o/oauth2/v2/auth?\
    redirect_uri={CALLBACK_URI}&\
    prompt=consent&\
    response_type=code&\
    scope={SCOPE}&\
    access_type=offline&\
    client_id={CLIENT_ID}
~~~~~~~~

*Kode* yang dikirimkan ke `{CALLBACK_URI}` dalam sebuah permintaan `GET`.
Untuk menangkap informasi ini di aplikasi kita, kita harus mempunya sebuah
pelayan web yang mendengar ke `localhost`.

Setelah kita punya *kode* tersebut, kita dapat melakukan *Access Token Request*:


{lang="text"}
~~~~~~~~
  POST /oauth2/v4/token HTTP/1.1
  Host: www.googleapis.com
  Content-length: {CONTENT_LENGTH}
  content-type: application/x-www-form-urlencoded
  user-agent: google-oauth-playground
  code={CODE}&\
    redirect_uri={CALLBACK_URI}&\
    client_id={CLIENT_ID}&\
    client_secret={CLIENT_SECRET}&\
    scope={SCOPE}&\
    grant_type=authorization_code
~~~~~~~~

yang akan memberikan jawaban berupa JSON.

{lang="text"}
~~~~~~~~
  {
    "access_token": "BEARER_TOKEN",
    "token_type": "Bearer",
    "expires_in": 3600,
    "refresh_token": "REFRESH_TOKEN"
  }
~~~~~~~~

*Bearer token* biasanya kadaluarsa setelah satu jam dan dapat disegarkan
dengan mengirimkan sebuah permintaan HTTP dengan *refresh token* yang valid.

{lang="text"}
~~~~~~~~
  POST /oauth2/v4/token HTTP/1.1
  Host: www.googleapis.com
  Content-length: {CONTENT_LENGTH}
  content-type: application/x-www-form-urlencoded
  user-agent: google-oauth-playground
  client_secret={CLIENT_SECRET}&
    grant_type=refresh_token&
    refresh_token={REFRESH_TOKEN}&
    client_id={CLIENT_ID}
~~~~~~~~

yang akan direspon dengan

{lang="text"}
~~~~~~~~
  {
    "access_token": "BEARER_TOKEN",
    "token_type": "Bearer",
    "expires_in": 3600
  }
~~~~~~~~

Semua permintaan dari pengguna ke server harus mengikutsertakan tajuk

{lang="text"}
~~~~~~~~
  Authorization: Bearer BEARER_TOKEN
~~~~~~~~

setelah mengganti dengan `BEARER_TOKEN` yang sebenarnya.

Google hanya akan menerima 50 *bearer token* terakhir. Jadi, waktu kadaluarsa
hanya merupakan panduan saja. *Refresh token* bertahan antar sesi dan dapat
dibuat kadaluarsa secara manual oleh pengguna. Sehingga, kita memiliki
aplikasi yang harus diatur sekali untuk mendapatkan "refresh token" (lol)
dan mengikutsertakan "refresh token" sebagai konfigurasi untuk pemasangan
server "headless".

Drone tidak perlu mengimplementasikan "endpoint" `/auth` atau refresh
karena sebuah `BEARER_TOKEN` sudah cukup untuk antarmuka.

### Data

Langkah pertama adalah memodelkan data yang dibutuhkan untuk OAuth2. Kita membuat
sebuah ADT dengan bidang yang sama persis dengan yang dibutuhkan oleh server OAuth2.
Kita akan menggunakan `String` dan `Long` dengan alasan keringkasan. Namun,
kita juga bisa menggunakan tipe "refined" bila bidang yang menggunakan `String` dan
`Long` tembus ke model bisnis kita.

{lang="text"}
~~~~~~~~
  import refined.api.Refined
  import refined.string.Url
  
  final case class AuthRequest(
    redirect_uri: String Refined Url,
    scope: String,
    client_id: String,
    prompt: String = "consent",
    response_type: String = "code",
    access_type: String = "offline"
  )
  final case class AccessRequest(
    code: String,
    redirect_uri: String Refined Url,
    client_id: String,
    client_secret: String,
    scope: String = "",
    grant_type: String = "authorization_code"
  )
  final case class AccessResponse(
    access_token: String,
    token_type: String,
    expires_in: Long,
    refresh_token: String
  )
  final case class RefreshRequest(
    client_secret: String,
    refresh_token: String,
    client_id: String,
    grant_type: String = "refresh_token"
  )
  final case class RefreshResponse(
    access_token: String,
    token_type: String,
    expires_in: Long
  )
~~~~~~~~

W> Hindari penggunaan `java.net.URL` karena kelas ini menggunakan DNS
W> untuk mengecek bagian hostname saat melakukan `toString`, `equals`, atau `hashCode`.
W> 
W> Selain tidak umum, dan **sangat pelan**, metoda metoda tersebut dapat melempar
W> eksepsi I/O, dan dapat berubah bergantung dengan konfigurasi jaringan.
W> Dengan kata lain, tidak murni dan tidak deterministik.
W> 
W> Tipe terrefinasi (lol) `String Refined Url` menyediakan jalan untuk melakukan
W> pemeriksaan kesamaan berdasarkan `String` dan kita dapat membuat sebuah `URL`
W> dengan aman jika ada kebutuhan dari API lama.
W> 
W> Dengan kata lain, pada kode dengan performa tinggi, kita lebih memilih untuk
W> meninggalkan `java.net.URL` dan menggunakan parser URL dari pihak ketiga seperti [jurl](https://github.com/anthonynsimon/jurl).
W> Selain karena banyak yang tidak "aman", juga karena lelet.


### Fungsionalitas

Kita juga harus menyusun kelas data yang telah kita definisikan pada bagian
sebelumnya ke JSON, URL, dan borang yang dikodekan dalam POST. Kebutuhan seperti
ini sangat bisa dipenuhi dengan menggunakan kelas tipe.

[`jsonformat`](https://github.com/scalaz/scalaz-deriving/tree/master/examples/jsonformat/src)
adalah pustaka JSON sederhana yang akan kita pelajari lebih seksama di bab yang akan datang.
Selain karena pustaka ini ditulis dengan pemrograman fungsional, juga karena didesain
dengan sedemikian rupa agar mudah dibaca. Pustaka ini terdiri dari sebuah AST JSON dan
kelas tipe penyandi dan pembaca sandi:

{lang="text"}
~~~~~~~~
  package jsonformat
  
  sealed abstract class JsValue
  final case object JsNull                                    extends JsValue
  final case class JsObject(fields: IList[(String, JsValue)]) extends JsValue
  final case class JsArray(elements: IList[JsValue])          extends JsValue
  final case class JsBoolean(value: Boolean)                  extends JsValue
  final case class JsString(value: String)                    extends JsValue
  final case class JsDouble(value: Double)                    extends JsValue
  final case class JsInteger(value: Long)                     extends JsValue
  
  @typeclass trait JsEncoder[A] {
    def toJson(obj: A): JsValue
  }
  
  @typeclass trait JsDecoder[A] {
    def fromJson(json: JsValue): String \/ A
  }
~~~~~~~~

A> `\/` merupakan implementasi `Either` dari Scalaz. Operator ini punya `.flatMap`
A> sehingga bisa digunakan pada komprehensi `for`. Hal yang berbeda
A> dengan pustaka standar Scala yang tidak mempunyai `.flatMap` sebelum Scala 2.12.
A> Untuk namanya, biasa disebut dengan (lol, apa ini?)
A> 
A> `scala.Either` [dimasukkan pada pustaka standar Scala](https://issues.scala-lang.org/browse/SI-250) oleh penulis Scalaz, Tony Morris pada 2007.
A> `\/` dibuat ketika metoda yang tidak aman ditambahkan ke `Either`.

Kita butuh instans `JsDecoder[AccessResponse]` dan `JsDecoder[RefreshResponse]`
dan kita dapat membuatnya dengan menggunakan fungsi bantuan:

{lang="text"}
~~~~~~~~
  implicit class JsValueOps(j: JsValue) {
    def getAs[A: JsDecoder](key: String): String \/ A = ...
  }
~~~~~~~~

Kita meletakkan instans tersebut pada pasangan dari tipe data kita. Sehingga,
mereka akan selalu ada pada cakupan implisit:

{lang="text"}
~~~~~~~~
  import jsonformat._, JsDecoder.ops._
  
  object AccessResponse {
    implicit val json: JsDecoder[AccessResponse] = j =>
      for {
        acc <- j.getAs[String]("access_token")
        tpe <- j.getAs[String]("token_type")
        exp <- j.getAs[Long]("expires_in")
        ref <- j.getAs[String]("refresh_token")
      } yield AccessResponse(acc, tpe, exp, ref)
  }
  
  object RefreshResponse {
    implicit val json: JsDecoder[RefreshResponse] = j =>
      for {
        acc <- j.getAs[String]("access_token")
        tpe <- j.getAs[String]("token_type")
        exp <- j.getAs[Long]("expires_in")
      } yield RefreshResponse(acc, tpe, exp)
  }
~~~~~~~~

Lalu, kita dapat menguraikan sebuah string ke `AccessResponse` atau `RefreshResponse`

{lang="text"}
~~~~~~~~
  scala> import jsonformat._, JsDecoder.ops._
  scala> val json = JsParser("""
                       {
                         "access_token": "BEARER_TOKEN",
                         "token_type": "Bearer",
                         "expires_in": 3600,
                         "refresh_token": "REFRESH_TOKEN"
                       }
                       """)
  
  scala> json.map(_.as[AccessResponse])
  AccessResponse(BEARER_TOKEN,Bearer,3600,REFRESH_TOKEN)
~~~~~~~~

Kita dapat menulis tipe kelas kita sendiri untuk URL dan pengkodean POST.
Berikut adalah desain yang masuk akal:

{lang="text"}
~~~~~~~~
  // URL query key=value pairs, in un-encoded form.
  final case class UrlQuery(params: List[(String, String)])
  
  @typeclass trait UrlQueryWriter[A] {
    def toUrlQuery(a: A): UrlQuery
  }
  
  @typeclass trait UrlEncodedWriter[A] {
    def toUrlEncoded(a: A): String Refined UrlEncoded
  }
~~~~~~~~

Kita harus menyediakan instans kelas tipe untuk tipe dasar:

{lang="text"}
~~~~~~~~
  import java.net.URLEncoder
  
  object UrlEncodedWriter {
    implicit val encoded: UrlEncodedWriter[String Refined UrlEncoded] = identity
  
    implicit val string: UrlEncodedWriter[String] =
      (s => Refined.unsafeApply(URLEncoder.encode(s, "UTF-8")))
  
    implicit val long: UrlEncodedWriter[Long] =
      (s => Refined.unsafeApply(s.toString))
  
    implicit def ilist[K: UrlEncodedWriter, V: UrlEncodedWriter]
      : UrlEncodedWriter[IList[(K, V)]] = { m =>
      val raw = m.map {
        case (k, v) => k.toUrlEncoded.value + "=" + v.toUrlEncoded.value
      }.intercalate("&")
      Refined.unsafeApply(raw) // by deduction
    }
  
  }
~~~~~~~~

Disini, kita menggunakan `Refined.unsafeApply` ketika kita dapat menebak isi
dari string sudah berupa url terkode.

`ilist` merupakan sebuah contoh dari penurunan sederhana dari kelas tipe,
yang kurang lebih satu tingkat dengan penurunan `Numeric[Complex]` dari
representasi numerik. Metoda `.intercalate` kurang lebih sama dengan `.mkString`
namun lebih umum.

A> `UrlEncodedWriter` menggunakan *Single Abstract Method* (tipe SAM)
A> yang merupakan fitur dari Scala. Secara lengkapnya bisa dilihat
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   implicit val string: UrlEncodedWriter[String] =
A>     new UrlEncodedWriter[String] {
A>       override def toUrlEncoded(s: String): String = ...
A>     }
A> ~~~~~~~~
A> 
A> Ketika kompilator Scala berharap sebuah kelas (yang memiliki sebuah metoda abstrak)
A> namun menerima sebuah lambda, kompilator akan mengisi plat cetak secara otomatis
A> 
A> Sebelum tipe SAM, pola yang jamak dijumpai adalah mendefinisikan metoda
A> dengan nama `instance` pada kelas tipe pasangan
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   def instance[T](f: T => String): UrlEncodedWriter[T] =
A>     new UrlEncodedWriter[T] {
A>       override def toUrlEncoded(t: T): String = f(t)
A>     }
A> ~~~~~~~~
A>
A> yang memungkinkan
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   implicit val string: UrlEncodedWriter[String] = instance { s => ... }
A> ~~~~~~~~
A> 
A> Pola ini masih digunakan pada kode yang harus mendukung penggunaan pada
A> Scala versi lama, atau untuk instans kelas yang harus menyediakan lebih
A> dari satu metoda.
A> 
A> Harap dicatat, ada banyak kutu yang berhubungan dengan tipe SAM, karena mereka
A> tidak berhubungan langsung dengan fitur bahasa. Silakan gunakan varian non-SAM
A> bila terjadi kerhemukan pada kompilator.

Pada bab khusus pada *Penurunan Kelas Tipe*, kita akan mengkalkulasi instans dari
`UrlQueryWriter` secara otomatis. Selain itu, kita akan merapikan apa yang
telah kita tulis. Untuk saat ini, kita akan menulis plat cetak untuk
tipe yang akan kita konversi:

{lang="text"}
~~~~~~~~
  import UrlEncodedWriter.ops._
  object AuthRequest {
    implicit val query: UrlQueryWriter[AuthRequest] = { a =>
      UriQuery(List(
        ("redirect_uri"  -> a.redirect_uri.value),
        ("scope"         -> a.scope),
        ("client_id"     -> a.client_id),
        ("prompt"        -> a.prompt),
        ("response_type" -> a.response_type),
        ("access_type"   -> a.access_type))
    }
  }
  object AccessRequest {
    implicit val encoded: UrlEncodedWriter[AccessRequest] = { a =>
      List(
        "code"          -> a.code.toUrlEncoded,
        "redirect_uri"  -> a.redirect_uri.toUrlEncoded,
        "client_id"     -> a.client_id.toUrlEncoded,
        "client_secret" -> a.client_secret.toUrlEncoded,
        "scope"         -> a.scope.toUrlEncoded,
        "grant_type"    -> a.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }
  object RefreshRequest {
    implicit val encoded: UrlEncodedWriter[RefreshRequest] = { r =>
      List(
        "client_secret" -> r.client_secret.toUrlEncoded,
        "refresh_token" -> r.refresh_token.toUrlEncoded,
        "client_id"     -> r.client_id.toUrlEncoded,
        "grant_type"    -> r.grant_type.toUrlEncoded
      ).toUrlEncoded
    }
  }
~~~~~~~~


### Modul

Bagian sebelumnya melengkapi semua pemodelan data dan fungsionalitas yang dibutuhkan
untuk mengimplementasikan OAuth2. Sebagaimana yang sudah dibahas pada bab sebelumnya,
kita mendefinisikan komponen yang akan berinteraksi dengan dunia luar sebagai
aljabar. Selain itu, kita akan mendefinisikan logika bisnis pada sebuah modul
sehingga bisa dites dengan seksama.

Kita akan mendefinisikan ketergantungan aljabar dan menggunakan batasan konteks
untuk agar respon kita mempunyai `JsDecoder` dan muatan `POST` kita mempunyai
`UrlEncodedWriter`:

{lang="text"}
~~~~~~~~
  trait JsonClient[F[_]] {
    def get[A: JsDecoder](
      uri: String Refined Url,
      headers: IList[(String, String)]
    ): F[A]
  
    def post[P: UrlEncodedWriter, A: JsDecoder](
      uri: String Refined Url,
      payload: P,
      headers: IList[(String, String]
    ): F[A]
  }
~~~~~~~~

Harap dicatat bahwa kita hanya mendefinisikan alur dengan asumsi terbaik
pada APA `JsonClient`.
Untuk kejadian kejadian yang tidak diinginkan dan penangannya, kita akan
membicarakannya pada bab selanjutnya.


Untuk mendapatkan `CodeToken` dari peladen `OAuth2` Google, ada beberapa langkah
yang harus dilakukan.

1.  Memulai sebuah peladen HTTP pada mesin lokal dan mendapatkan nomor portnya.
2.  Memaksa pengguna untuk membuka sebuah laman web pada peramban mereka yang
    dimaksudkan agar mereka dapat masuk dengan menggunakan kredensial mereka
    mengizinkan aplikasi untuk menggunakan akun mereka, dan dilanjutkan dengan
    sebuah pengalihan balik ke mesin lokal.
3.  Mengambil kode token, menginformasikan kepada pengguna tentang langkah selanjutnya,
    lalu menutup peladen HTTP.

Kita dapat memodelkan langkah berikut dengan tiga metoda pada aljabar di `UserInteraction`.

{lang="text"}
~~~~~~~~
  final case class CodeToken(token: String, redirect_uri: String Refined Url)
  
  trait UserInteraction[F[_]] {
    def start: F[String Refined Url]
    def open(uri: String Refined Url): F[Unit]
    def stop: F[CodeToken]
  }
~~~~~~~~

Mungkin pembaca budiman tidak percaya dengan cuplikan diatas. Akan tetapi,
memang kenyataannya semudah itu.

Lalu, kita akan melanjutkan dengan abstraksi atas waktu pada sistem lokal.

{lang="text"}
~~~~~~~~
  trait LocalClock[F[_]] {
    def now: F[Epoch]
  }
~~~~~~~~

Dan membuat tipe data yang akan kita pakai pada logika untuk memuat ulang

{lang="text"}
~~~~~~~~
  final case class ServerConfig(
    auth: String Refined Url,
    access: String Refined Url,
    refresh: String Refined Url,
    scope: String,
    clientId: String,
    clientSecret: String
  )
  final case class RefreshToken(token: String)
  final case class BearerToken(token: String, expires: Epoch)
~~~~~~~~

Sekarang, kita akan menulis modul klien OAuth2:

{lang="text"}
~~~~~~~~
  import http.encoding.UrlQueryWriter.ops._
  
  class OAuth2Client[F[_]: Monad](
    config: ServerConfig
  )(
    user: UserInteraction[F],
    client: JsonClient[F],
    clock: LocalClock[F]
  ) {
    def authenticate: F[CodeToken] =
      for {
        callback <- user.start
        params   = AuthRequest(callback, config.scope, config.clientId)
        _        <- user.open(params.toUrlQuery.forUrl(config.auth))
        code     <- user.stop
      } yield code
  
    def access(code: CodeToken): F[(RefreshToken, BearerToken)] =
      for {
        request <- AccessRequest(code.token,
                                 code.redirect_uri,
                                 config.clientId,
                                 config.clientSecret).pure[F]
        msg     <- client.post[AccessRequest, AccessResponse](
                     config.access, request)
        time    <- clock.now
        expires = time + msg.expires_in.seconds
        refresh = RefreshToken(msg.refresh_token)
        bearer  = BearerToken(msg.access_token, expires)
      } yield (refresh, bearer)
  
    def bearer(refresh: RefreshToken): F[BearerToken] =
      for {
        request <- RefreshRequest(config.clientSecret,
                                  refresh.token,
                                  config.clientId).pure[F]
        msg     <- client.post[RefreshRequest, RefreshResponse](
                     config.refresh, request)
        time    <- clock.now
        expires = time + msg.expires_in.seconds
        bearer  = BearerToken(msg.access_token, expires)
      } yield bearer
  }
~~~~~~~~


## Kesimpulan

-   *Tipe data aljabar* (TDA) didefinisikan sebagai *produk* (`final case class`) dan
    ko-produk (`sealed abstract class`).
-   Tipe `Refined` memperketat batasan pada nilai.
-   Fungsi konkret dapat didefinisikan pada sebuah `implicit class` agar alur
    pembacaan kode tetap dari kiri ke kanan.
-   Fungsi polimorfis didefinisikan pada *kelas tipe*. Fungsionalitas disediakan
    melalui *batasan konteks* "mempunyai", bukan pada hierarki kelas "merupakan".
-   *Instans* kelas tipe merupakan implementasi dari kelas.
-   Kelas tipe `@simulacrum.typeclass` menghasilkan `.ops` pada pasangan dan menyediakan
    sintaks yang mudah pada fungsi di kelas tipe.
-   *Derivasi kelas tipe* merupakan komposisi yang dijalankan pada saat kompilasi atas
    instans kelas tipe.

# Kelas Tipe Scalaz

Pada bab ini, kita akan melihat-lihat tipe kelas yang ada pada `scalaz-core`.
Tentu kita tidak akan menggunakan semuanya pada `drone-dynamic-agents`.
Maka dari itu, kita akan menggunakan contoh sederhana bila dibutuhkan.

Sebenarnya, banyak sekali kritik tentang penamaan pada Scala dan pemrograman
fungsional secara umum. Kebanyakan nama yang digunakan, menggunakan konvensi
yang dikenalkan oleh bahasa pemrograman Haskell berdasarkan *Teori Kategori*.
Silakan menggunakan tipe alias bila kata kerja yang melandasi fungsionalitas
utama lebih mudah diingat saat belajar. (Mis, `Mappable` untuk yang bisa dipetakan,
`Pureable` untuk yang bisa "diangkat", dll).

Sebelum kita berbincang mengenai hierarki kelas tipe, kita akan melihat
4 metoda yang paling penting, bila dilihat dari sudut pandang kontrol alur

| Kelas Tipe | Metoda | Dari  | Diberikan | Untuk   |
|------------- |---------- |--------- |----------- |--------- |
| `Functor`     | `map`      | `F[A]`    | `A => B`    | `F[B]`    |
| `Applicative` | `pure`     | `A`       |             | `F[A]`    |
| `Monad`       | `flatMap`  | `F[A]`    | `A => F[B]` | `F[B]`    |
| `Traverse`    | `sequence` | `F[G[A]]` |             | `G[F[A]]` |

Sebagaimana yang kita tahu bahwa operasi-operasi yang mengembalikan sebuah `F[_]`
dapat dijalankan secara berurutan pada `for` comprehension (lol) dengan memanggil
`.flatMap` yang didefinisikan pada `Monad[F]` terkait.
Konteks `F[_]` bisa dianggap sebagai kontainer untuk *efek* intensional
dengan `A` sebagai output: `flatMap` memberikan kita jalan untuk menghasilkan
efek `F[B]` pada saat waktu jalan berdasarkan hasil dari evaluasi efek sebelumnya.

Dan sudah barang tentu tidak semua konstruktor `F[_]` mempunyai efek.
Bahkan, bila konstruktor tersebut mempunyai instans `Monad[F]`, juga
belum tentu konstruktor tadi mempunyai efek.
Seringkali, konstruktor tersebut hanya merupakan struktur data yang
digunakan untuk pengabstraksian. Misalkan, kita bisa menggunakan `List`,
`Either`, `Future`, dan lain lain untuk membuat struktur data.

Bila kita hanya perlu mengubah output dari sebuah `F[_]`, maka kita
bisa menggunakan `map` yang diperkenalkan oleh `Functor`.
Pada bab 3, kita menjalankan banyak efek secara paralel dengan membuat
sebuah produk dan melakukan pemetaan ("mapping") kepada produk tersebut.
Pada pemrograman fungsional, komputasi yang bisa diparalelkan seringkali
dianggap kurang manjur bila dibandingkan dengan komputasi sekuensial.

Di antara `Monad` dan `Functor` ada `Applicative` yang mendefinisikan
`pure`. `pure` sendiri berfungsi untuk mengumpil sebuah nilai menjadi
sebuah efek ataupun membuat sebuah struktur data dari sebuah nilai tunggal.

Untuk `.sequence`, metoda ini paling manjur bila digunakan untuk menyusun-ulang
konstruktor tipe. Bilamana kita mempunyai sebuah `F[G[_]]` namun kita butuh `G[F[_]]`, (tfw no gf)
adalah sebuah tindakan yang bijak bila kita menggunakan `.sequence`.
Sebagai contoh, `List[Future[Int]]` bisa diubah menjadi `Future[List[Int]]` dengan
memanggil metoda tadi.


## Agenda

Bab ini jauh lebih panjang dan padat informasi bila dibandingkan dengan
bab lain. Kami sangat menyarankan pembaca nan budiman untuk membaca
bab ini dalam beberapa kesempatan. Selain itu, juga disarankan untuk
menganggap bab ini sebagai lumbung pencarian informasi lebih lanjut,
tidak untuk mengingat-ingat.

Dan sebuah hal yang tidak mengejutkan bahwa kelas tipe yang memperpanjang
`Monad` tidak dibahas pada bab ini karena kelas tipe tersebut akan dibahas pada
bab tersendiri.

Sebagai pengingat, Scalaz menggunakan pembuatan kode, bukan tiruan. Namun,
jangan kuatir, kita hanya akan menempelkan potongan kode dengan `@typeclass`
dengan alasan keringkasan.  Sintaks yang ekuivalen juga tersedia ketika
kita meng-`import scalaz._, Scalaz._`.  Lebih tepatnya, ada pada paket
`scalaz.syntax` pada sumber kode scalaz.

{width=100%}
![](images/scalaz-core-tree.png)

{width=60%}
![](images/scalaz-core-cliques.png)

{width=60%}
![](images/scalaz-core-loners.png)


## Yang dapat Dibubuhkan

{width=25%}
![](images/scalaz-semigroup.png)

{lang="text"}
~~~~~~~~
  @typeclass trait Semigroup[A] {
    @op("|+|") def append(x: A, y: =>A): A
  
    def multiply1(value: F, n: Int): F = ...
  }
  
  @typeclass trait Monoid[A] extends Semigroup[A] {
    def zero: A
  
    def multiply(value: F, n: Int): F =
      if (n <= 0) zero else multiply1(value, n - 1)
  }
  
  @typeclass trait Band[A] extends Semigroup[A]
~~~~~~~~

A> `|+|` secara umum dikenal sebagai operator dasi. Ada dasi lanjutan
A> pada bagian selanjutnya.

Sebuah `Semigroup` bisa didefinisikan sebagai sebuah tipe bila dua buah nilai
bisa digabungkan. Operasi penggabungan tersebut harus *asosiatif* yang berarti
urutan dari operasi berlapis tidak boleh berpengaruh.

{lang="text"}
~~~~~~~~
  (a |+| b) |+| c == a |+| (b |+| c)
  
  (1 |+| 2) |+| 3 == 1 |+| (2 |+| 3)
~~~~~~~~

Sebuah `Monoid` merupakan sebuah `Semigroup` dengan elemen "*zero*" / *nol*
(juga dikenal dengan elemen *kosong* atau *identitas*). Penggabungan `zero`
dengan sebuah nilai `a` harus menghasilkan `a`.

{lang="text"}
~~~~~~~~
  a |+| zero == a
  
  a |+| 0 == a
~~~~~~~~

Pembicaraan ini membuat kita teringat tentang kennagan atas `Numeric` pada bab 4.
Semua angka primitif mempunyai implementasi `Monoid`. Namun, konsep "*appendable*" /
*bisa dibubuhkan* berguna tidak hanya untuk angka saja.

{lang="text"}
~~~~~~~~
  scala> "hello" |+| " " |+| "world!"
  res: String = "hello world!"
  
  scala> List(1, 2) |+| List(3, 4)
  res: List[Int] = List(1, 2, 3, 4)
~~~~~~~~

Ada hukum yang membatasi perilaku dari operasi `append` pada kelas tipe
`Band`, salah satunya adalah penambahan dari dua elemen harus idempoten.
Idempoten yang dimaksud disini adalah selalu memberikan nilai yang sama.
Contoh yang jamak digunakan misalkan `Unit`, yang hanya mempunyai satu
nilai saja. `Set` juga bisa digunakan. Walaupun `Band` tidak mempunyai
metoda lain, pengguna dapat memanfaatkan properti ini untuk optimisasi
performa.

A> Orang kondang dari Lightbend, Viktor Klang, lah yang memperkenalkan
A> istilah [eksekusi sekali saja](https://twitter.com/viktorklang/status/789036133434978304)
A> untuk pemrosesan pesan idempoten.

Sebuah contoh yang cukup realistis untuk `Monoid` adalah mengenai sebuah sistem
*trading* yang mempunyai basis data templat jual beli yang sangat besar.
Untuk mengisi nilai bawaan dari sebuah *trade*, diperlukan pemilahan dan
penggabungan dari banyak templat dengan aturan "aturan terbaru yang dipakai"
bila ada dua templat sama sama menyediakan sebuah nilai untuk bidang yang sama.
Proses pemilahan sendiri sudah dilakukan oleh sistem lain. Tugas kitalah
yang menggabungkan templat templat tersebut.

Kita akan membuat skema templat sederhana untuk menunjukkan prinsip
penggunaan monad. Harap diingat bahwa sebuah sistem yang realistis tentu
mempunyai tipe data aljabar yang jauh lebih kompleks.

{lang="text"}
~~~~~~~~
  sealed abstract class Currency
  case object EUR extends Currency
  case object USD extends Currency
  
  final case class TradeTemplate(
    payments: List[java.time.LocalDate],
    ccy: Option[Currency],
    otc: Option[Boolean]
  )
~~~~~~~~

Bila kita menulis sebuah metoda yang menerima `templates: List[TradeTemplate]`,
kita hanya perlu memanggil

{lang="text"}
~~~~~~~~
  val zero = Monoid[TradeTemplate].zero
  templates.foldLeft(zero)(_ |+| _)
~~~~~~~~

dan selesai.

Tetapi, untuk bisa menggunakan `zero` atau memanggil `|+|`, kita harus
mempunyai instans `Monoid[TradeTemplate]`. Walaupun kita bisa menurunkan
instans ini secara otomatis, seperti yang akan ditunjukkan pada bab
selanjutnya, demi contoh yang komprehensif, kita akan membuat instans
secara manual pada objek pasangan:

{lang="text"}
~~~~~~~~
  object TradeTemplate {
    implicit val monoid: Monoid[TradeTemplate] = Monoid.instance(
      (a, b) => TradeTemplate(a.payments |+| b.payments,
                              a.ccy |+| b.ccy,
                              a.otc |+| b.otc),
      TradeTemplate(Nil, None, None)
    )
  }
~~~~~~~~

Yang disayangkan dari contoh di atas adalah `Monoid[Option[A]]` akan menambah
konten dari `A`. Bisa dilihat dari hasil REPL berikut:

{lang="text"}
~~~~~~~~
  scala> Option(2) |+| None
  res: Option[Int] = Some(2)
  scala> Option(2) |+| Option(1)
  res: Option[Int] = Some(3)
~~~~~~~~

sedangkan, yang kita inginkan adalah "yang digunakan adalah aturan terakhir".
Kita dapat mengesampingkan nilai bawaan `Monoid[Option[A]]` dengan mengganti
dengan kode berikut:

{lang="text"}
~~~~~~~~
  implicit def lastWins[A]: Monoid[Option[A]] = Monoid.instance(
    {
      case (None, None)   => None
      case (only, None)   => only
      case (None, only)   => only
      case (_   , winner) => winner
    },
    None
  )
~~~~~~~~

Dan semua terkompil dengan baik.

{lang="text"}
~~~~~~~~
  scala> import java.time.{LocalDate => LD}
  scala> val templates = List(
           TradeTemplate(Nil,                     None,      None),
           TradeTemplate(Nil,                     Some(EUR), None),
           TradeTemplate(List(LD.of(2017, 8, 5)), Some(USD), None),
           TradeTemplate(List(LD.of(2017, 9, 5)), None,      Some(true)),
           TradeTemplate(Nil,                     None,      Some(false))
         )
  
  scala> templates.foldLeft(zero)(_ |+| _)
  res: TradeTemplate = TradeTemplate(
                         List(2017-08-05,2017-09-05),
                         Some(USD),
                         Some(false))
~~~~~~~~

Yang kita butuhkan hanyalah pengimplementasian sebuah logika bisnis
dan `Monoid` menyelesaikan semuanya.

Harap diperhatikan bahwa daftar `payments` digabungkan. Kenapa demikian?
Karena perilaku bawaan dari `Monoid[List]` adalah menggabungkan elemen-elemen
dan kebetulan perilaku tersebut juga kita harapkan. Bila persyaratan bisnis
yang ditemui berbeda, maka kita hanya perlu menyediakan `Monoid[List[LocalDate]]`
yang kita tulis sendiri. Dan jangan lupa bahwa dengan menggunakan polimorfisme
saat kompilasi, kita bisa mendapatkan implementasi `append` yang berbeda
sesuai dengan `E` pada `List[E]`.


A> Saat kita berkenalan dengan kelas tipe pada bab 4, saat itu kita
A> berbicara bahwa implementasi kelas tipe untuk sebuah parameter tipe
A> hanya boleh ada satu saja. Misalkan, hanya boleh ada satu `Monoid[Option[Boolean]]`
A> pada aplikasi. *Orphan instance* semacam `lastWins` merupakan cara yang
A> paling mudah untuk merusak koherensi kelas tipe kita.
A>
A> Bisa saja kita membuat pembenaran atas perusakan koherensi kelas tipe
A> dengan menjadikan `lastWins` metoda privat. Namun, saat kita sampai
A> pada pembahasan kelas tipe `Plus`, kita akan tahu bahwa ada cara lain
A> yang lebih disarankan untuk mengimplementasikan `Monoid`.
A> Ketika kita membahas mengenai tipe bertanda (lol, tagged type), kita akan
A> tahu cara terbaik adalah dengan menggunakan `LastOption`, bukan `Option`,
A> pada model data kita.


## Semacam Objek

Pada bab mengenai Data dan Fungsionalitas, kita secara sekilas menyimpulkan
mengenai gagasan JVM atas persamaan menghancurkan banyak hal yang bisa
kita masukkan ke dalam sebuah ADT. Permasalahan mendasar mengenai hal ini
adalah JVM didesain untuk Java. Dan Java sendiri, `equals` didefinisikan
pada `java.lang.Object`. Hal ini diperparah dengan kenyataan bahwa metoda
tersebut tidak bisa dihapus dan tidak ada jaminan bahwa pasti sudah diimplementasikan.

Untungnya, pada pemprograman fungsional, kita lebih memilih untuk menggunakan
kelas tipe untuk memenuhi kebutuhun atas fungsionalitas polimorfis.
Ditambah dengan konsep persamaan diperiksa pada saat waktu kompilasi.

{width=20%}
![](images/scalaz-comparable.png)

{lang="text"}
~~~~~~~~
  @typeclass trait Equal[F]  {
    @op("===") def equal(a1: F, a2: F): Boolean
    @op("/==") def notEqual(a1: F, a2: F): Boolean = !equal(a1, a2)
  }
~~~~~~~~

Dan faktanya, `===` adalah lebih aman dibandingkan `==` yang hanya bisa
dikompilasi bila tipe dari kedua belah sisi operator ini mempunyai tipe
yang sama. Kita bisa mengatakan bahwa operator ini salah satu jaring
pengaman yang bagus.

`equal` mempunyai persyaratan implementasi yang sama dengan `Object.equals`

-   *komutatif* `f1 === f2` yang juga sama dengan `f2 === f1`
-   *refleksif* `f === f`
-   *tarnsitif* `f1 === f2 && f2 === f3` yang juga sama dengan `f1 === f3`

Dengan membuang konsep umum `Object.equals`, kita tidak akan menyia-nyiakan
persamaan saat kita menyusun sebuah ADT. Selain itu, kita tak akan mendapatkan
harapan palsu akan persamaan dimana sebenarnya tidak pernah ada.

Melanjutkan tren pengubahan konsep Java, data tidak lagi *merupakan*
`java.lang.Comparable` namun *memiliki* `Order`, berdasarkan:

{lang="text"}
~~~~~~~~
  @typeclass trait Order[F] extends Equal[F] {
    @op("?|?") def order(x: F, y: F): Ordering
  
    override  def equal(x: F, y: F): Boolean = order(x, y) == Ordering.EQ
    @op("<" ) def lt(x: F, y: F): Boolean = ...
    @op("<=") def lte(x: F, y: F): Boolean = ...
    @op(">" ) def gt(x: F, y: F): Boolean = ...
    @op(">=") def gte(x: F, y: F): Boolean = ...
  
    def max(x: F, y: F): F = ...
    def min(x: F, y: F): F = ...
    def sort(x: F, y: F): (F, F) = ...
  }
  
  sealed abstract class Ordering
  object Ordering {
    case object LT extends Ordering
    case object EQ extends Ordering
    case object GT extends Ordering
  }
~~~~~~~~

`Order` mengimplementasikan `.equal` dalam primitif baru `.order`. Ketika
sebuah kelas tipe mengimplementasikan *kombinator primitif* bapaknya dengan
sebuah *kombinator turunan*, maka **hukum substitusi** untuk kelas
tipe tersebut akan ditambahkan secara tidak langsung. Bila sebuah instans
dari `Order` digunakan untuk mengesampingkan `.equal` dengan alasan performa,
maka instans tersebut harus punya perilaku yang identik dengan implementasi
yang asli.

Things that have an order may also be discrete, allowing us to walk
successors and predecessors:

Objek-objek yang mempunyai order bisa jadi juga merupakan objek diskrit,
hal ini memberikan kita ruang untuk memeriksa objek sebelum dan sesudahnya:

{lang="text"}
~~~~~~~~
  @typeclass trait Enum[F] extends Order[F] {
    def succ(a: F): F
    def pred(a: F): F
    def min: Option[F]
    def max: Option[F]
  
    @op("-|-") def succn(n: Int, a: F): F = ...
    @op("---") def predn(n: Int, a: F): F = ...
  
    @op("|->" ) def fromToL(from: F, to: F): List[F] = ...
    @op("|-->") def fromStepToL(from: F, step: Int, to: F): List[F] = ...
    @op("|=>" ) def fromTo(from: F, to: F): EphemeralStream[F] = ...
    @op("|==>") def fromStepTo(from: F, step: Int, to: F): EphemeralStream[F] = ...
  }
~~~~~~~~

{lang="text"}
~~~~~~~~
  scala> 10 |--> (2, 20)
  res: List[Int] = List(10, 12, 14, 16, 18, 20)
  
  scala> 'm' |-> 'u'
  res: List[Char] = List(m, n, o, p, q, r, s, t, u)
~~~~~~~~

A> `|-->` merupakan *Lightsaber* dari Scalaz. Pengguna pemrograman fungsional
A> biasa menggunakan sintaks seperti ini.

Kita akan berdiskusi mengenai `EphemeralStream` pada bab berikutnya. Untuk
saat ini, kita hanya perlu tahu bahwa tipe data ini bisa digunakan untuk menyusun
struktur data tak hingga tanpa harus kuatir mengenai masalah memori sebagaimana
struktur data `Stream` dari pustaka standar.

Sama halnya dengan `Object.equals`, konsep `.toString` yang ada pada tiap kelas
sangat tidak masuk akal di Java. Idealnya, kita harus memastikan bahwa sebuah
objek memang bisa diubah menjadi string pada saat waktu kompilasi.
Untuk mendapatkan hasil tersebut, kita dapat menggunakan `Show`:

{lang="text"}
~~~~~~~~
  trait Show[F] {
    def show(f: F): Cord = ...
    def shows(f: F): String = ...
  }
~~~~~~~~

Kita akan membahas `Cord` lebih mendetail pada bab selanjutnya mengenai tipe data.
Untuk saat ini, kita hanya perlu tahu bahwa `Cord` merupakan struktur data yang
efisien yang dipergunakan untuk menyimpan dan memanipulasi `String`.

## Yang Dapat Dipetakan

Kita akan fokus pada benda benda yang bisa dipetakan atau dilalui:

{width=100%}
![](images/scalaz-mappable.png)


### Fungtor

{lang="text"}
~~~~~~~~
  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  
    def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())
    def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))
  
    def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))
    def strengthL[A, B](a: A, f: F[B]): F[(A, B)] = map(f)(b => (a, b))
    def strengthR[A, B](f: F[A], b: B): F[(A, B)] = map(f)(a => (a, b))
  
    def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
    def mapply[A, B](a: A)(f: F[A => B]): F[B] = map(f)((ff: A => B) => ff(a))
  }
~~~~~~~~

Satu-satunya metoda abstrak adalah `map` yang harus bisa *menggabungkan*
dua fungsi. Sebagai contoh, memetakan `f` dan dilanjutkan dengan `g`
sama dengan memetakan dengan hasil komposisi dari `f` dan `g`:

{lang="text"}
~~~~~~~~
  fa.map(f).map(g) == fa.map(f.andThen(g))
~~~~~~~~

`map` juga harus melakukan no-op bila fungsi yang disediakan berupa
fungsi `identity` (`x => x`).

{lang="text"}
~~~~~~~~
  fa.map(identity) == fa
  
  fa.map(x => x) == fa
~~~~~~~~

`Functor` mendefinisikan beberapa metoda pembantu untuk `map` yang bisa dioptimalkan
dengan instans khusus. Dokumentasi memang sengaja dihilangkan pada definisi diatas
agar pembaca budiman memnebak apa yang sebuah metoda lakukan sebelum melihat
ke implementasi dari metoda tersebut. Sangat disarankan untuk memperhatikan dengan
penanda tipe dengan seksama berikut sebelum melanjutkan seksi ini:

{lang="text"}
~~~~~~~~
  def void[A](fa: F[A]): F[Unit]
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)]
  
  def fpair[A](fa: F[A]): F[(A, A)]
  def strengthL[A, B](a: A, f: F[B]): F[(A, B)]
  def strengthR[A, B](f: F[A], b: B): F[(A, B)]
  
  // harder
  def lift[A, B](f: A => B): F[A] => F[B]
  def mapply[A, B](a: A)(f: F[A => B]): F[B]
~~~~~~~~

1.  `void` menerima sebuah instans dari `F[A]` dan selalu mengembalikan
    `F[Unit]`. Metoda ini selalu menghapus semua nilai sembari menjaga
    struktur.
2.  `fproduct` menerima input yang sama dengan `map` namun mengembalikan `F[(A, B)]`.
    Sebagai contoh, fungsi ini akan memasangkan konten dengan hasil dari
    fungsi tersebut. Fungsi ini berguna bila kita ingin tetap menggunakan
    input yang diterima oleh fungsi ini.
3.  `fpair` menggandakan semua elemen dari `A` menjadi *tuple* `F[(A, A)]`.
4.  `strengthL` memasangkan konten dari sebuah `F[A]` dengan konstan `B`
    pada bagian kiri.
5.  `strengthR` memasangkan konten dari sebuah `F[A]` dengan konstan `B`
    pada bagian kanan.
6.  `lift` menerima sebuah fungsi `A => B` dan mengembalikan `F[A] => F[B]`.
    Dengan kata lain, fungsi ini menerima sebuah fungsi berdasarkan konten
    dari `F[A]` dan mengembalikan sebuah fungsi yang beroperasi secara
    langsung pada `F[A]`.
7.  `mapply` sendiri merupakan fungsi yang agak janggal. Misalkan kita
    mempunyai sebuah `F[_]` pada fungsi `A => B` dan nilai `A`. Kita bisa
    mendapatkan hasil berupa `F[B]`. Fungsi ini mempunyai *signature* yang
    mirip dengan `pure`, namun mengharuskan pemanggil fungsi ini untuk
    mempunyai `F[A => B]`.

Secara sekilas, `fpair`, `strengthL`, dan `strengthR` terlihat tidak berguna.
Namun, kita bisa menggunakannya saat kita ingin tetap menggunakan informasi
yang bisa jadi hilang saat keluar dari cakupan fungsi. Misal, indeks dari
sebuah `List` atau `Set` saat melakukan `traverse`.

`Functor` punya beberapa sintaks khusus, antara lain:

{lang="text"}
~~~~~~~~
  implicit class FunctorOps[F[_]: Functor, A](self: F[A]) {
    def as[B](b: =>B): F[B] = Functor[F].map(self)(_ => b)
    def >|[B](b: =>B): F[B] = as(b)
  }
~~~~~~~~

`.as` dan `>|` digunakan untuk mengganti keluaran fungsi dengan sebuah
konstanta.

A> Ketika Scalaz menyediakan fungsionalitas tambahan dalam bentuk sintaks, hal
A> ini dilakukan dengan alasan kompatibilitas biner.
A>
A> Saat versi `X.Y.0` Scalaz rilis, adalah tidak mungkin bila kita harus menambah
A> metoda ke kelas tipe untuk versi tersebut. Apalagi dengan kewajiban atas dukungan
A> terhadap Scala versi 2.10 dan 2.11. Maka dari itu, sangat dianjurkan untuk
A> mempelajari kode sumber dan sintaks dari kelas tipe.

Pada contoh aplikasi kita, terdapat sebuah tambalan yang tidak kita
ungkap sampai sekarang. Tambalan tersebut adalah pendefinisian `start` dan `stop`
untuk mengembalikan input:

{lang="text"}
~~~~~~~~
  def start(node: MachineNode): F[MachineNode]
  def stop (node: MachineNode): F[MachineNode]
~~~~~~~~

Pendefinisian diatas memperkenankan kita untuk menulis logika
bisnis yang ringkas seperti

{lang="text"}
~~~~~~~~
  for {
    _      <- m.start(node)
    update = world.copy(pending = Map(node -> world.time))
  } yield update
~~~~~~~~

dan

{lang="text"}
~~~~~~~~
  for {
    stopped <- nodes.traverse(m.stop)
    updates = stopped.map(_ -> world.time).toList.toMap
    update  = world.copy(pending = world.pending ++ updates)
  } yield update
~~~~~~~~

Namun, tambalan ini melimpahkan kompleksitas ke bagian implementasi secara mubazir.
Sungguh, jauh lebih disukai bila kita mendesain aljabar kita untuk
mengembalikan `F[Unit]` dan menggunakan `as`:

{lang="text"}
~~~~~~~~
  m.start(node) as world.copy(pending = Map(node -> world.time))
~~~~~~~~

dan

{lang="text"}
~~~~~~~~
  for {
    stopped <- nodes.traverse(a => m.stop(a) as a)
    updates = stopped.map(_ -> world.time).toList.toMap
    update  = world.copy(pending = world.pending ++ updates)
  } yield update
~~~~~~~~


### *Foldable*

Secara teknis, `Foldable` merupakan struktur data yang bisa langkahi satu-per-satu
untuk menghasilkan sebuah nilai ijmal. Namun, terlalu meremehkan bila kita hanya
berhenti sampai disitu. Kenyataannya, `Foldable` merupakan kelas tipe yang
bisa menjawab hampir semua apa yang diharapkan dari sebuah Koleksi APA.

Berhubung kelas tipe ini mempunyai begitu banyak metoda, ada baiknya
kita pecah pecah. Dimulai dengan metoda abstrak:

{lang="text"}
~~~~~~~~
  @typeclass trait Foldable[F[_]] {
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B
    def foldRight[A, B](fa: F[A], z: =>B)(f: (A, =>B) => B): B
    def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = ...
~~~~~~~~

Secara teori, untuk mendapatkan semua fungsionalitas dari kelas tipe `Foldable`,
sebuah instans hanya perlu mengimplementasikan `foldMap` dan `foldRight`.
Walaupun pada kenyataannya, banyak sekali metoda lain yang diimplementasikan
sesuai dengan struktur data yang dibutuhkan agar waktu jalan lebih optimal.

Di pasaran, santer terdengar istilah keren **MapReduce**. Pada Scala
sendiri, istilah tersebut dikenal dengan `.foldMap`. `.foldMap` sendiri
berupa fungsi yang hanya membutuhkan fungsi yang memetakan `A` ke `B`,
sebuah `F[A]`, dan cara untuk menggabungkan semua hasil pemetaan dari
`A` ke `B` menjadi satu nilai (yang disediakan oleh `Monoid` dan `zero` dari `B`)
untuk menghasilkan nilai ijmal `B`. Selain itu, tidak ada urutan operasi
dari fungsi yang memetakan `A` ke `B`. Sehingga, memungkinkan untuk
dilakukannya komputasi paralel.

Untuk `foldRight`, metoda ini tidak memaksa parameternya untuk mempunyai
instans `Monoid` walau harus menerima sebuah nilai awal `z` dan cara
penggabungan tiap elemen dari struktur data. Selain itu, urutan pelangkahan
dari elemen elemen input adalah dari kiri ke kanan. Hal ini juga berarti
bahwa metoda ini tidak bisa dijalankan secara paralel.

A> Secara konsep, `foldRight` dari Scalaz mempunyai perilaku yang sama
A> dengan `foldRight` yang disediakan oleh pustaka standar. Salah satu
A> yang menjadikan metoda dari Scalaz lebih unggul dibandingkan dengan
A> metoda dari pustaka standar adalah metoda dari Scalaz mampu menangani
A> sturktur data yang besar. Penyebab dari hal ini adalah penerapan `List.foldRight`
A> dari pustaka standar dengan menggunakan `foldLeft` yang dibalik
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   override def foldRight[B](z: B)(op: (A, B) => B): B =
A>     reverse.foldLeft(z)((right, left) => op(left, right))
A> ~~~~~~~~
A> 
A> namun, konsep pembalikkan seperti ini tidaklah universal. Banyak struktur data
A> tidak bisa ditangani dengan menggunakan pembalikan seperti ini. Misal, kita
A> ingin mencari beberapa nilai kecil di sebuah `Stream` dengan penyelesaian dini:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> def isSmall(i: Int): Boolean = i < 10
A>   scala> (1 until 100000).toStream.foldRight(false) {
A>            (el, acc) => isSmall(el) || acc
A>          }
A>   java.lang.StackOverflowError
A>     at scala.collection.Iterator.toStream(Iterator.scala:1403)
A>     ...
A> ~~~~~~~~
A> 
A> Scalaz menyelesaikan masalah ini dengan mengambil sebuah parameter
A> panggilan untuk nilai agregat
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> (1 |=> 100000).foldRight(false)(el => acc => isSmall(el) || acc )
A>   res: Boolean = true
A> ~~~~~~~~
A> 
A> yang berarti `acc` tidak akan dievaluasi sebelum nilai tersebut benar
A> benar dibutuhkan.
A>
A> Juga harap dipahami bahwa tidak semua operasi tidak berbahaya bagi
A> *stack memory* saat menggunakan `foldRight`. Bila kita benar-benar
A> membutuhkan evaluasi dari semua elemen, bisa saja kita mendapat galat
A> `StackOverflowError` saat menggunakan `EphemeralStream` milik Scalaz
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   scala> (1L |=> 100000L).foldRight(0L)(el => acc => el |+| acc )
A>   java.lang.StackOverflowError
A>     at scalaz.Foldable.$anonfun$foldr$1(Foldable.scala:100)
A>     ...
A> ~~~~~~~~

`foldLeft` melangkahi semua elemen dari kiri ke kanan. Untuk mengimplementasikan
`foldLeft`, kita bisa menggunakan `foldMap` walau kebanyakan instans
juga mengimplementasikan sendiri `foldLeft` khusus untuk instans tersebut.
Hal lain yang patut diperhatikan adalah implementasi metoda ini berupa
rekursi akhir, `foldLeft` tidak menggunakan parameter panggilan.

Hukum yang mengatur `Foldable` hanya ada satu, yaitu `foldLeft` dan
`foldRight` harus konsisten dengan `foldMap` untuk operasi monoidal.
Misalnya, menambahkan sebuah elemen di bagian awal dari sebuah list
untuk implmentasi `foldLeft` dan menambahkan sebuah elemen di bagian
akhir dari sebuah list untuk `foldRight`.
Di sisi lain, `foldLeft` dan `foldRight` tidak harus selalu konsisten
satu sama lain. Bahkan, seringkali mereka mempunyai hasil yang berlawanan.

Hal yang paling sederhana untuk dilakukan pada `foldMap` adalah menggunakan
fungsi `identity` yang menghasilkan `fold` (ijmal natural dari elemen monoidal)
dengan varian kiri/kanan agar dapat memperkenankan pemilihan berdasarkan
kriteria performa:

{lang="text"}
~~~~~~~~
  def fold[A: Monoid](t: F[A]): A = ...
  def sumr[A: Monoid](fa: F[A]): A = ...
  def suml[A: Monoid](fa: F[A]): A = ...
~~~~~~~~

Mengulang apa yang kita pelajari tentang `Monoid`, kita menulis:

{lang="text"}
~~~~~~~~
  scala> templates.foldLeft(Monoid[TradeTemplate].zero)(_ |+| _)
~~~~~~~~

Namun, kode di atas bisa ditulis ulang menjadi

{lang="text"}
~~~~~~~~
  scala> templates.toIList.fold
  res: TradeTemplate = TradeTemplate(
                         List(2017-08-05,2017-09-05),
                         Some(USD),
                         Some(false))
~~~~~~~~

Sayangnya, `.fold` tidak bisa dipanggil dari `List` milik pustaka standar.
Hal ini dikarenakan `List` sudah memiliki metoda dengan nama `fold` yang
berbeda dengan metoda `fold` dari kelas tipe `Foldable`.

Untuk metoda `intercalate`, metoda ini menyisipkan sebuah `A` spesifik
diantara elemen sebelum melakukan operasi `fold`

{lang="text"}
~~~~~~~~
  def intercalate[A: Monoid](fa: F[A], a: A): A = ...
~~~~~~~~

metoda ini bisa dianggap sebagai metoda `mkString` dari pustaka standar
yang diumumkan:

{lang="text"}
~~~~~~~~
  scala> List("foo", "bar").intercalate(",")
  res: String = "foo,bar"
~~~~~~~~

`foldLeft` menyediakan kita cara untuk mendapatkan elemen manapun dengan
melangkahi semua elemen satu per satu, termasuk dengan beberapa metoda
lainnya:

{lang="text"}
~~~~~~~~
  def index[A](fa: F[A], i: Int): Option[A] = ...
  def indexOr[A](fa: F[A], default: =>A, i: Int): A = ...
  def length[A](fa: F[A]): Int = ...
  def count[A](fa: F[A]): Int = length(fa)
  def empty[A](fa: F[A]): Boolean = ...
  def element[A: Equal](fa: F[A], a: A): Boolean = ...
~~~~~~~~

Berbeda dengan `List(0)` yang sangat mungkin melempar eksepsi, `Foldable.index`
lebih memilih untuk mengembalikan sebuah `Option[A]` atau bisa juga dengan
mengembalikan sebuah `A` bila menggunakan `.indexOr` (dengan sebuah nilai
bawaan `A`). Sama halnya dengan `.contains` punya pustaka standar yang menggunakan
persamaan standar dari JVM, Scalaz menyediakan `.element` yang menggunakan `Equal`
yang jauh lebih unggul.

Metoda ini *sangat* terlihat mirip dengan APA koleksi. Dan yang paling utama,
`Foldable` bisa diubah menjadi `List`.

{lang="text"}
~~~~~~~~
  def toList[A](fa: F[A]): List[A] = ...
~~~~~~~~

Selain itu, konversi ke tipe data lain juga ada. Sebagai contoh, `.toStream`,
`.toSet`, `.toVector`, `.to[T <: TraversableLike]`, dan lain sebagainya.

Untuk pengecekan predikat, Foldable menyediakan

{lang="text"}
~~~~~~~~
  def filterLength[A](fa: F[A])(f: A => Boolean): Int = ...
  def all[A](fa: F[A])(p: A => Boolean): Boolean = ...
  def any[A](fa: F[A])(p: A => Boolean): Boolean = ...
~~~~~~~~

Untuk memeriksa jumlah elemen yang bernilai `true` terhadap sebuah predikat,
kita bisa menggunakan `filterLength`. Sedangkan untuk memeriksa apakah
semua elemen bernilai `true`, `all` adalah fungsi yang tepat guna.
`any` sendiri hanya memastikan bahwa setidaknya ada satu elemen dari `Foldable`
bernilai `true` terhadap predikat yang disediakan.

A> We've seen the `NonEmptyList` in previous chapters. For the sake of
A> brevity we use a type alias `Nel` in place of `NonEmptyList`.
A>
A> Kita telah melihat `NonEmptyList` pada bab sebelumnya. Demi keringkasan,
A> kita akan menggunakan alias tipe `Nel` sebagai ganti `NonEmptyList`.
A>
A> Kita juga sudah diperkenalkan kepadaa `IList` sebagai alternatif
A> untuk `List` pustaka standar dengan menghilangkan metoda tidak murni (lol, pure)
A> seperti `apply`.

Untuk memecah sebuah `F[A]` menjadi beberapa bagian, kita bisa menggunakan
`splitBy`

{lang="text"}
~~~~~~~~
  def splitBy[A, B: Equal](fa: F[A])(f: A => B): IList[(B, Nel[A])] = ...
  def splitByRelation[A](fa: F[A])(r: (A, A) => Boolean): IList[Nel[A]] = ...
  def splitWith[A](fa: F[A])(p: A => Boolean): List[Nel[A]] = ...
  def selectSplit[A](fa: F[A])(p: A => Boolean): List[Nel[A]] = ...
  
  def findLeft[A](fa: F[A])(f: A => Boolean): Option[A] = ...
  def findRight[A](fa: F[A])(f: A => Boolean): Option[A] = ...
~~~~~~~~

sebagai contoh

{lang="text"}
~~~~~~~~
  scala> IList("foo", "bar", "bar", "faz", "gaz", "baz").splitBy(_.charAt(0))
  res = [(f, [foo]), (b, [bar, bar]), (f, [faz]), (g, [gaz]), (b, [baz])]
~~~~~~~~

patut diperhatikan bahwa ada dua nilai dengan indeks `'b'`.

Bilamana sebuah list objek `A` yang  tidak memiliki kelas tipe `Equal`
namun kita ingin memecahnya menjadi beberapa bagian, kita bisa menggunakan
`splitByRelation` yang meminta operator pembanding sebagai gantinya.

Bisa juga kita memecah sebuah `Foldable` menjadi dua bagian, satu bagian
memenuhi sebuah predikat, dan sebaliknya, dengan menggunakan `splitWith`.
Sedangkan untuk memilih himpunan yang memenuhi predikat sembari membuang
yang lain, kita menggunakan `selectSplit`.

Untuk `findLeft` dan `findRight` sendiri, metoda ini mengambil elemen
pertama dari kiri atau kanan yang sesuai dengan predikat.

Dengan menggunakan `Equal` dan `Order`, kita juga mendapat metoda lain
yang mengembalikan himpunan.

{lang="text"}
~~~~~~~~
  def distinct[A: Order](fa: F[A]): IList[A] = ...
  def distinctE[A: Equal](fa: F[A]): IList[A] = ...
  def distinctBy[A, B: Equal](fa: F[A])(f: A => B): IList[A] =
~~~~~~~~

`distinct`, secara pengimplementasian, lebih efisien bila dibandingkan
dengan `distinctE`. Hal ini disebabkan karena `distinct` menggunakan
pengurutan (menggunakan `Order`) sehingga menggunakan algoritma yang
mirip dengan quicksort yang relatif lebih cepat bila dibandingkan dengan
menggunakan `List.distinct` dari pustaka standar. Keuntungan lain
adalah struktur data semacam set secara otomatis mempunyai `distinct`.

Untuk mengelompokkan berdasarkan dari hasi sebuah fungsi atas tiap elemen,
kita bisa menggunakan `distinctBy`. Sebagai contoh, kita bisa mengelompokkan
nama berdasarkan huruf pertama.

Kita dapat menggunakan `Order` lebih lanjut untuk mengekstrak elemen dengan
nilai terkecil maupun terbesar dari sebuah `Foldable`. Lebih lanjut lagi,
kita juga akan menggunakan pola varian `Of` dan `By` untuk memetakan
elemen-elemen tadi ke tipe lain ataupun menggunakan tipe lain sebagai
pembanding urutan.

{lang="text"}
~~~~~~~~
  def maximum[A: Order](fa: F[A]): Option[A] = ...
  def maximumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] = ...
  def maximumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] = ...
  
  def minimum[A: Order](fa: F[A]): Option[A] = ...
  def minimumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] = ...
  def minimumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] = ...
  
  def extrema[A: Order](fa: F[A]): Option[(A, A)] = ...
  def extremaOf[A, B: Order](fa: F[A])(f: A => B): Option[(B, B)] = ...
  def extremaBy[A, B: Order](fa: F[A])(f: A => B): Option[(A, A)] =
~~~~~~~~

Sebagai contoh, kita bisa memeriksa `String` manakah mempunyai nilai
paling besar berdasarkan panjang dengan menggunakan varian `By`.
Bisa juga kita mencari nilai paling besar dari elemen-elemen yang ada
dengan menggunakan varian `Of`.

{lang="text"}
~~~~~~~~
  scala> List("foo", "fazz").maximumBy(_.length)
  res: Option[String] = Some(fazz)
  
  scala> List("foo", "fazz").maximumOf(_.length)
  res: Option[Int] = Some(4)
~~~~~~~~

Dengan ini, fitur utama dari `Foldable` sudah digambarkan secara sekilas.
Dan hikmah yang bisa kita ambil dari sub-bab ini adalah apapun yang bisa
kita gunakan pada pustaka *collection*, kita juga bisa mendapatkannya
pada `Foldable`.

Kita akan menutup sub-bab ini dengan beberapa variasi metoda yang sudah
kita lihat sebelumnya. Pertama, berikut adalah metoda yang menerima
subah `Semigroup`, bukan `Monoid`:

{lang="text"}
~~~~~~~~
  def fold1Opt[A: Semigroup](fa: F[A]): Option[A] = ...
  def foldMap1Opt[A, B: Semigroup](fa: F[A])(f: A => B): Option[B] = ...
  def sumr1Opt[A: Semigroup](fa: F[A]): Option[A] = ...
  def suml1Opt[A: Semigroup](fa: F[A]): Option[A] = ...
  ...
~~~~~~~~

yang mengembalikan `Option` dengan pertimbangan struktur data yang kosong. 
(Harap ingat, `Semigroup` tidak mempunyai `zero`)

A> Metoda tersebut menggunakan "satu-Option" bukan `10pt`

Kelas tipe `Foldable1` berisi jauh lebih banyak varian `Semigroup` dari
metoda `Monoid` bila dibandingkan dengan yang ditampilkan di sini.
Dan hal itu dirasa masuk akal untuk struktur data yang tidak bisa kosong,
tanpa harus memaksa elemen elemennya mempunyai kelas `Monoid`.

Tidak kalah penting, ada beberapa varian yang menerima nilai nilai kembalian monadik.
Kita juga telah menggunakan `foldLeftM` saat kita menulis logika bisnis dari
aplikasi kita.
Sekarang, kita tahu dari mana asal fungsi tersebut.

{lang="text"}
~~~~~~~~
  def foldLeftM[G[_]: Monad, A, B](fa: F[A], z: B)(f: (B, A) => G[B]): G[B] = ...
  def foldRightM[G[_]: Monad, A, B](fa: F[A], z: =>B)(f: (A, =>B) => G[B]): G[B] = ...
  def foldMapM[G[_]: Monad, A, B: Monoid](fa: F[A])(f: A => G[B]): G[B] = ...
  def findMapM[M[_]: Monad, A, B](fa: F[A])(f: A => M[Option[B]]): M[Option[B]] = ...
  def allM[G[_]: Monad, A](fa: F[A])(p: A => G[Boolean]): G[Boolean] = ...
  def anyM[G[_]: Monad, A](fa: F[A])(p: A => G[Boolean]): G[Boolean] = ...
  ...
~~~~~~~~


### *Traverse*

`Traverse` bisa dikatakan sebagai penggabungan antara `Functor` dan `Foldable`

{lang="text"}
~~~~~~~~
  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = ...
  
    def reverse[A](fa: F[A]): F[A] = ...
  
    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = ...
    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = ...
    def indexed[A](fa: F[A]): F[(Int, A)] = ...
    def zipWithL[A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): F[C] = ...
    def zipWithR[A, B, C](fa: F[A], fb: F[B])(f: (Option[A], B) => C): F[C] = ...
  
    def mapAccumL[S, A, B](fa: F[A], z: S)(f: (S, A) => (S, B)): (S, F[B]) = ...
    def mapAccumR[S, A, B](fa: F[A], z: S)(f: (S, A) => (S, B)): (S, F[B]) = ...
  }
~~~~~~~~

Pada awal bab, kita telah menunjukkan mengenai pentingnya `traverse` dan
`sequence` untuk membolak-balik konstruktor tipe agar sesuai dengan *requirement*.
Sebagai contoh, `List[Future[_]]` menjadi `Future[List[_]]`.

Tidak seperti `Foldable` dimana kita tidak dapat serta merta mengasumsikan
bahwa `reverse` adalah sebuah hak asasi, dengan `Traverse` kita dapat
dengan santai membolak-balik sesuatu.


Selain itu, kita juga bisa merekatkan dua buah objek yang mempunyai `Traverse`
menjadi `F[(A, B)]`. Namun, harap diingat bahwa ada kemungkinan bahwa panjang
kedua benda tadi tidak sama, sehingga kita harus menggunakan `Option[A]` atau
`Option[B]` bilamana panjang salah satu benda lebih pendek bila dibandingkan
dengan panjang benda lainnya. Untuk menangani hal tersebut, kita bisa menggunakan
`zipL` maupun `zipR` untuk menentukan sisi mana yang "dipotong" bila panjang
berbeda. `zip` merupakan fungsi khusus untuk menambahkan sebuah indeks untuk
setiap entri yang terindeks.

`zipWithL` dan `zipWithR` memberikan kita kesempatan untuk membuat sebuah `F[C]`
dengan menggabungkan kedua sisi dari panggabungan tadi.

`mapAccumR` dan `mapAccumL` sebenarnya hanya `map` yang dikombinasi dengan
sebuah akumulator. Bilamana kita terbiasa dengan Java yang membuat kita
ingin menggunakan sebuah `var` dan menggunakan `var` tersebut dalam sebuah
`map`, maka kita harus menggunakan `mapAccumL`.

Sebagai contoh, anggap saja kita mempunyai sebuah senarai kata dan kita ingin
mengabaikan kata-kata yang sudah ada. Algoritma penyaringan tidak diperkenankan
untuk mengolah senarai kata dua kali. Keuntungan yang didapat adalah algoritma
ini bisa mencapai urutan tak hingga:

{lang="text"}
~~~~~~~~
  scala> val freedom =
  """We campaign for these freedoms because everyone deserves them.
     With these freedoms, the users (both individually and collectively)
     control the program and what it does for them."""
     .split("\\s+")
     .toList
  
  scala> def clean(s: String): String = s.toLowerCase.replaceAll("[,.()]+", "")
  
  scala> freedom
         .mapAccumL(Set.empty[String]) { (seen, word) =>
           val cleaned = clean(word)
           (seen + cleaned, if (seen(cleaned)) "_" else word)
         }
         ._2
         .intercalate(" ")
  
  res: String =
  """We campaign for these freedoms because everyone deserves them.
     With _ _ the users (both individually and collectively)
     control _ program _ what it does _ _"""
~~~~~~~~

Pada akhirnya, `Traverse1`, sebagaimana `Foldable1`, menyediakan varian metoda
metoda untuk struktur data yang tidak bisa kosong. Selain itu, `Traverse1` juga
menerima `Semigroup`, bukan `Monoid`, dan sebuah `Apply`, bukan `Applicative`.
Harap diingat bahwa `Semigroup` tidak mempunyai `.empty` dan `Apply` tidak harus
mempunyai `.point`.

### *Align*

Untuk bab ini, kita berbicara tentang `Align`. `Align` sendiri juga berbicara
mengenai penggabungan pelapisan `Functor`. Ada baiknya sebelum kita menyelami
`Align`, kita memandang sekilas mengenai tipe data `\&/` yang akan kita
panggil dengan *hore!*

{lang="text"}
~~~~~~~~
  sealed abstract class \&/[+A, +B]
  final case class This[A](aa: A) extends (A \&/ Nothing)
  final case class That[B](bb: B) extends (Nothing \&/ B)
  final case class Both[A, B](aa: A, bb: B) extends (A \&/ B)
~~~~~~~~

bisa dibilang, *hore!* merupakan penyandian data logika inklusif `OR`.
`A`, `B`, ataupun keduanya. Jadi, bilamana ada token `F[A \&/ B]`,
kita bisa menafsirkannya sebagai "sebuah fungtor yang bisa berisi `A`
ataupun `B`."

{lang="text"}
~~~~~~~~
  @typeclass trait Align[F[_]] extends Functor[F] {
    def alignWith[A, B, C](f: A \&/ B => C): (F[A], F[B]) => F[C]
    def align[A, B](a: F[A], b: F[B]): F[A \&/ B] = ...
  
    def merge[A: Semigroup](a1: F[A], a2: F[A]): F[A] = ...
  
    def pad[A, B]: (F[A], F[B]) => F[(Option[A], Option[B])] = ...
    def padWith[A, B, C](f: (Option[A], Option[B]) => C): (F[A], F[B]) => F[C] = ...
~~~~~~~~

`alignWith` menerima sebuah fungsi yang menghasilkan `C`, bisa dari `A`,
`B`, ataupun keduanya, dan mengembalikan sebuah fungsi yang terangkat
dari tuple `F[A]` dan `F[B]` menjadi `F[C]`. Sedangkan bila kita ingin
membuat sebuah fungtor *hore!*, kita bisa menggunakan `align` yang
membuat sebuah `\&/` dari dua `F[_]`.

`merge` memberikan kita jalan untuk menggabungkan dua `F[A]` bila `A`
mempunyai `Semigroup`. Sebagai contoh, implementasi dari `Semigroup[Map[K, V]]`
mengikuti implementasi dari `Semigroup[V]` dan menggabungkan dua entri.
Selain mengembalikan nilai penggabungkan, implmentasi ini juga berperilaku
sebagaimana sebuah multimap:

{lang="text"}
~~~~~~~~
  scala> Map("foo" -> List(1)) merge Map("foo" -> List(1), "bar" -> List(2))
  res = Map(foo -> List(1, 1), bar -> List(2))
~~~~~~~~

dan ketika penggabungan terjadi, `Map[K, Int]` hanya perlu menambahkan
isi dari map tersebut:

{lang="text"}
~~~~~~~~
  scala> Map("foo" -> 1) merge Map("foo" -> 1, "bar" -> 2)
  res = Map(foo -> 2, bar -> 2)
~~~~~~~~

`.pad` dan `.padWith` biasa digunakan untuk menggabungkan dua struktur data,
yang munggkin saja tidak lengkap pada salah satunya, secara parsial.
Sebagai contoh, kita menggunakan fungsi ini saat kita ingin mengagregasi
penghitungan suara independen dan tetap menyimpan asal dari suara tersebut.

{lang="text"}
~~~~~~~~
  scala> Map("foo" -> 1) pad Map("foo" -> 1, "bar" -> 2)
  res = Map(foo -> (Some(1),Some(1)), bar -> (None,Some(2)))
  
  scala> Map("foo" -> 1, "bar" -> 2) pad Map("foo" -> 1)
  res = Map(foo -> (Some(1),Some(1)), bar -> (Some(2),None))
~~~~~~~~

Ada beberapa varian dari `align` yang memudahkan kita untuk menggunakan
struktur dari `\&/`

{lang="text"}
~~~~~~~~
  ...
    def alignSwap[A, B](a: F[A], b: F[B]): F[B \&/ A] = ...
    def alignA[A, B](a: F[A], b: F[B]): F[Option[A]] = ...
    def alignB[A, B](a: F[A], b: F[B]): F[Option[B]] = ...
    def alignThis[A, B](a: F[A], b: F[B]): F[Option[A]] = ...
    def alignThat[A, B](a: F[A], b: F[B]): F[Option[B]] = ...
    def alignBoth[A, B](a: F[A], b: F[B]): F[Option[(A, B)]] = ...
  }
~~~~~~~~

yang seharusnya bisa terlihat dari *type signature* (lol, help) mereka.
Contoh:

{lang="text"}
~~~~~~~~
  scala> List(1,2,3) alignSwap List(4,5)
  res = List(Both(4,1), Both(5,2), That(3))
  
  scala> List(1,2,3) alignA List(4,5)
  res = List(Some(1), Some(2), Some(3))
  
  scala> List(1,2,3) alignB List(4,5)
  res = List(Some(4), Some(5), None)
  
  scala> List(1,2,3) alignThis List(4,5)
  res = List(None, None, Some(3))
  
  scala> List(1,2,3) alignThat List(4,5)
  res = List(None, None, None)
  
  scala> List(1,2,3) alignBoth List(4,5)
  res = List(Some((1,4)), Some((2,5)), None)
~~~~~~~~

Harap dicatat bahwa varian `A` dan `B` menggunakan inklusif `OR` sedangkan
varian `This` dan `That` menggunakan ekslusif `OR` yang mengembalikan `None`
bila nilai pada salah satu sisi.


## *Variance*

Mungkin adalah sebuah keputusan yang tepat bila kita kembali membahas
`Functor` sesaat dan mendiskusikan hierarki yang sebelumnya kita abaikan:

{width=100%}
![](images/scalaz-variance.png)

`InvariantFunctor`, yang juga dikenal sebagai *fungtor eksponensial*,
mempunyai metoda `xmap` yang menyatakan bahwa bila kita mempunyai fungsi
yang memetakan `A` ke `B` dan sebuah fungsi yang memetakan `B` ke `A`,
maka kita dapat mengkonversi `F[A]` ke `F[B]`.

`Functor` merupakan kependekan dari yang seharusnya disebut *fungtor kovarian*.
Dikarenakan `Functor` sudah lebih dulu dikenal, maka penyebutan ini
diteruskan. Begitu halnya dengan `Contravariant`, fungtor ini seharusnya
disebut sebagai *fungtor kontravarian*.

`Functor` mengimplementasikan `xmap` dengan `map` dan mengabaikan fungsi
dari `B` ke `A`. Sedangkan `Contravariant` mengimplementasikan `xmap`
dengan `contramap` dan mengabaikan fungsi dari `A` ke `B`:

{lang="text"}
~~~~~~~~
  @typeclass trait InvariantFunctor[F[_]] {
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B]
    ...
  }
  
  @typeclass trait Functor[F[_]] extends InvariantFunctor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = map(fa)(f)
    ...
  }
  
  @typeclass trait Contravariant[F[_]] extends InvariantFunctor[F] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = contramap(fa)(g)
    ...
  }
~~~~~~~~

Adalah hal yang penting untuk diperhatikan, walaupun secara teori berkaitan,
kata *kovarian*, *kontravarian*, dan *invarian* tidak berhubungan secara
langsung dengan varian tipe punya Scala (mis, `+` dan `-` yang biasa ditulis
pada penanda tipe). *Invarian* yang dimaksudkan disini
adalah bisa dilakukannya pemetaan konten dari struktur `F[A]` ke `F[B]`.
Menggunakan `identity`, kita dapat menentukan bahwa `A` bisa dengan aman
di-downcast atau upcast menjadi `B` dengan melihat varian dari
fungtor.

`.map` bisa dipahami dengan "bila kamu punya sebuah `F` atas `A` dan cara untuk
mengubah `A` ke `B`, maka saya bisa memberi kamu sebuah `F` atas `B`."

Sebaliknya, `.contramap` dapat dibaca sebagai "bila kamu mempunyai `F` atas `A`
dan cara untuk mengubah `B` menjadi `A`, maka saya dapat memberi kamu sebuah `F`
atas `B`."

Anggap contoh berikut: pada aplikasi kita, kita memperkenalkan tipe spesifik domain
`Alpha`, `Beta`, `Gamma`, dan lain lain untuk memastikan bahwa kita tidak akan
mencampur aduk angka angka pada kalkulasi finansial:

{lang="text"}
~~~~~~~~
  final case class Alpha(value: Double)
~~~~~~~~

namun, masalah diatas tergantikan dengan masalah baru mengenai tidak adanya
kelas tipe untuk tipe baru ini. Bilamana kita menggunakan nilai pada dokumen
JSON, kita harus menulis instans dari `JsEncoder` dan `JsDecoder` untuk tipe
baru tadi.

Untungnya, `JsEncoder` mempunyai sebuah `Contravariant` dan `JsDecoder` mempunyai
sebuah `Functor` sehingga kita dapat menurunkan instans tersebut dengan mengisi
kontrak:

-  "bila kamu memberi saya sebuah `JsDecoder` untuk `Double` dan cara untuk mengubah
   `Double` menjadi `Alpha`, maka saya akan memberikan sebuah `JsDecoder` untuk `Alpha`."
-  "bila kamu memberi saya sebuah `JsEncoder` untuk `Double` dan cara untuk mengubah
   `Alpha` menjadi `Double`, maka saya akan memberikan sebuah `JsEncoder` untuk `Alpha`."

{lang="text"}
~~~~~~~~
  object Alpha {
    implicit val decoder: JsDecoder[Alpha] = JsEncoder[Double].map(_.value)
    implicit val encoder: JsEncoder[Alpha] = JsEncoder[Double].contramap(_.value)
  }
~~~~~~~~

Metoda pada kelas tipe bisa saja mempunyai tipe parameter dengan posisi kontravarian
(parameter metoda) atau posisi kovarian (tipe kembalian). Bila sebuah kelas tipe
mempunyai sebuah kombinasi atas posisi kovarian dan kontravarian, bisa jadi
kelas tipe tersebut mempunyai *fungtor invarian*. Sebagai contoh, `Semigroup`
dan `Monoid` mempunyai `InvariantFunctor` namun tidak memiliki `Functor` maupun
`Contravariant`.


## *Apply* dan *Bind*

Sub-bab ini bisa dianggap sebagai pemanasan untuk `Applicative` dan `Monad`

{width=100%}
![](images/scalaz-apply.png)


### Apply

`Apply` memperpanjang `Functor` dengan menambahkan sebuah metoda dengan
nama `ap` yang mirip dengan `map`, dalam batasan `ap` juga menerapkan
sebuah fungsi ke nilai. Bedanya, fungsi yang diterima `ap` masih dalam
konteks yang sama dengan nilai yang diterapi.

{lang="text"}
~~~~~~~~
  @typeclass trait Apply[F[_]] extends Functor[F] {
    @op("<*>") def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B]
    ...
~~~~~~~~

A> Mari kita anggap `<*>` sebagai pesawat TIE dari Star Wars.

Implikasi dari hal ini adalah struktur data sederhana seperti `Option[A]`
juga mempunyai implementasi `.ap`

{lang="text"}
~~~~~~~~
  implicit def option[A]: Apply[Option[A]] = new Apply[Option[A]] {
    override def ap[A, B](fa: =>Option[A])(f: =>Option[A => B]) = f match {
      case Some(ff) => fa.map(ff)
      case None    => None
    }
    ...
  }
~~~~~~~~

Untuk mengimplementasikan `.ap`, pertama-tama kita harus mengekstrak
fungsi `ff: A => B` dari `f: Option[A => B]`, dan dilanjutkan dengan memetakan
`ff` atas `fa`. Ekstraksi fungsi dari konteks adalah fitur penting dari `Apply`
yang memberikan ruang untuk menggabungkan isi dari konteks yang melingkupi
operasi pemanggilan `ap`.

Kembali ke `Apply`, kita menemukan plat cetak `.applyX` yang
menyediakan jalan untuk menggabungkan fungsi-fungsi paralel dan pada akhirnya
memetakan nilai keluaran mereka:

{lang="text"}
~~~~~~~~
  @typeclass trait Apply[F[_]] extends Functor[F] {
    ...
    def apply2[A,B,C](fa: =>F[A], fb: =>F[B])(f: (A, B) => C): F[C] = ...
    def apply3[A,B,C,D](fa: =>F[A],fb: =>F[B],fc: =>F[C])(f: (A,B,C) =>D): F[D] = ...
    ...
    def apply12[...]
~~~~~~~~

`.apply2` bisa dibaca sebagai: "bila kamu memberi saya sebuah `F` atas `A` dan
`F` atas `B` dan sebuah cara untuk menggabungkan `A` dan `B` menjadi `C`, maka
saya akan memberi kamu `F` atas `C`." Ada beberapa penggunaan atas metoda ini.
Dan, dua yang paling penting adalah:

-   membuat kelas tipe untuk tipe produk `C` dari `A` dan `B`
-   melakukan *efek* secara paralel, sebagaimana drone dan aljabar google
    yang kita buat pada Bab 3, dan menggabungkan hasilnya.

Sudah barang tentu `Apply` mempunyai beberapa sintaks khusus yang
berguna:

{lang="text"}
~~~~~~~~
  implicit class ApplyOps[F[_]: Apply, A](self: F[A]) {
    def *>[B](fb: F[B]): F[B] = Apply[F].apply2(self,fb)((_,b) => b)
    def <*[B](fb: F[B]): F[A] = Apply[F].apply2(self,fb)((a,_) => a)
    def |@|[B](fb: F[B]): ApplicativeBuilder[F, A, B] = ...
  }
  
  class ApplicativeBuilder[F[_]: Apply, A, B](a: F[A], b: F[B]) {
    def tupled: F[(A, B)] = Apply[F].apply2(a, b)(Tuple2(_))
    def |@|[C](cc: F[C]): ApplicativeBuilder3[C] = ...
  
    sealed abstract class ApplicativeBuilder3[C](c: F[C]) {
      ..ApplicativeBuilder4
        ...
          ..ApplicativeBuilder12
  }
~~~~~~~~

yang sudah kita gunakan pada Bab 3:

{lang="text"}
~~~~~~~~
  (d.getBacklog |@| d.getAgents |@| m.getManaged |@| m.getAlive |@| m.getTime)
~~~~~~~~

Sintaks `<*` dan `*>` (paruh buruh kiri dan kanan) menawarkan cara mudah untuk
mengabaikan keluaran dari salah satu dari dua efek paralel .

Walaupun sintaks `|@|` cukup jelas, ada masalah yang ada pada `ApplicativeBuilder`.
Yaitu, pengalokasian objek dengan instans `ApplicativeBuilder` baru tiap kali
penambahan efek. Bila tugas yang diberikan sangat bergantung pada I/O, maka
alokasi memori tidak signifikan. Namun, bila tugas sangat bergantung pada
CPU, maka sangat disarankan untuk menggunakan sintaks alternatif *pengangkatan dengan arity*
yang tidak membuat objek penengah.


{lang="text"}
~~~~~~~~
  def ^[F[_]: Apply,A,B,C](fa: =>F[A],fb: =>F[B])(f: (A,B) =>C): F[C] = ...
  def ^^[F[_]: Apply,A,B,C,D](fa: =>F[A],fb: =>F[B],fc: =>F[C])(f: (A,B,C) =>D): F[D] = ...
  ...
  def ^^^^^^[F[_]: Apply, ...]
~~~~~~~~

digunakan seperti

{lang="text"}
~~~~~~~~
  ^^^^(d.getBacklog, d.getAgents, m.getManaged, m.getAlive, m.getTime)
~~~~~~~~

atau memanggil `applyX` secara langsung

{lang="text"}
~~~~~~~~
  Apply[F].apply5(d.getBacklog, d.getAgents, m.getManaged, m.getAlive, m.getTime)
~~~~~~~~

Walaupun lebih sering digunakan bersama dengan efek, `Apply` juga bisa digunakan
dengan struktur data. Misalkan, kita bisa menulis ulang

{lang="text"}
~~~~~~~~
  for {
    foo <- data.foo: Option[String]
    bar <- data.bar: Option[Int]
  } yield foo + bar.shows
~~~~~~~~

sebagai

{lang="text"}
~~~~~~~~
  (data.foo |@| data.bar)(_ + _.shows)
~~~~~~~~

Bila kita hanya ingin menggabungkan keluaran sebagai sebuah tuple, ada
metoda yang bisa memenuhi hal tersebut:

{lang="text"}
~~~~~~~~
  @op("tuple") def tuple2[A,B](fa: =>F[A],fb: =>F[B]): F[(A,B)] = ...
  def tuple3[A,B,C](fa: =>F[A],fb: =>F[B],fc: =>F[C]): F[(A,B,C)] = ...
  ...
  def tuple12[...]
~~~~~~~~

{lang="text"}
~~~~~~~~
  (data.foo tuple data.bar) : Option[(String, Int)]
~~~~~~~~

Juga ada versi umum dari `ap` untuk lebih dari dua parameter:

{lang="text"}
~~~~~~~~
  def ap2[A,B,C](fa: =>F[A],fb: =>F[B])(f: F[(A,B) => C]): F[C] = ...
  def ap3[A,B,C,D](fa: =>F[A],fb: =>F[B],fc: =>F[C])(f: F[(A,B,C) => D]): F[D] = ...
  ...
  def ap12[...]
~~~~~~~~

yang bersamaan dengan metoda `.lift` yang menerima fungsi normal dan mengangkat
mereka pada konteks `F[_]`

{lang="text"}
~~~~~~~~
  def lift2[A,B,C](f: (A,B) => C): (F[A],F[B]) => F[C] = ...
  def lift3[A,B,C,D](f: (A,B,C) => D): (F[A],F[B],F[C]) => F[D] = ...
  ...
  def lift12[...]
~~~~~~~~

juga ada pula aplikasi sintaks parsial untuk `ap`

{lang="text"}
~~~~~~~~
  def apF[A,B](f: =>F[A => B]): F[A] => F[B] = ...
~~~~~~~~

Dan terakhir, `.forever`

{lang="text"}
~~~~~~~~
  def forever[A, B](fa: F[A]): F[B] = ...
~~~~~~~~

yang mengulang operasi dengan efek tanpa henti. Instans dari `Apply` harus aman
secara alokasi stack atau kita bisa mendapatkan galat `StackOverflowError`.


### *Bind*

Fungsi utama yang dibawa oleh `Bind` tentu adalah `.bind` yang sama dan sebangun
dengan `.flatMap`. Dan sebagaimana yang telah kita pelajari pada bab sebelumnya,
fungsi ini, `.bind`, memperkenankan sebuah fungsi untuk menerima nilai input dari
keluaran dari fungsi dengan efek, dan pada akhirnya fungsi `.bind` mengembalikan
sebuah nilai dengan efek yang sama dari fungsi pemberi nilai input.
Fungsi `.bind` ini juga bisa menggabungkan dua buah struktur data.

{lang="text"}
~~~~~~~~
  @typeclass trait Bind[F[_]] extends Apply[F] {
  
    @op(">>=") def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = bind(fa)(f)
  
    override def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B] =
      bind(f)(x => map(fa)(x))
    override def apply2[A, B, C](fa: =>F[A], fb: =>F[B])(f: (A, B) => C): F[C] =
      bind(fa)(a => map(fb)(b => f(a, b)))
  
    def join[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)
  
    def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] = ...
    def ifM[B](value: F[Boolean], t: =>F[B], f: =>F[B]): F[B] = ...
  
  }
~~~~~~~~

Pembaca budiman yang biasa menggunakan fungsi `.flatten` dari pustaka standar
mungkin akan merasa familiar dengan fungsi `.join`. `.join` menerima sebuah
konteks yang berlapis dan meratakan lapisan lapisan tadi menjadi satu.

Untuk kombinator turunan yang ada pada kelas tipe ini, pembaca budiman mendapatkan
`.ap` dan `.apply2` yang mempunyai batasan untuk selalu berkesesuaian dengan `.bind`.
Pada nantinya, kita akan menyaksikan bahwa hukum ini berpengaruh besar dalam
strategi paralelisasi.

Sebagaimana halnya dengan `Functor.fproduct`, `mproduct` juga memasangkan
masukan dan keluaran dari fungsi tersebut di dalam `F`.

Bilamana `if` merupakan konstruk kondisional, `ifM` merupakan konstruk
kondisional yang menerima struktur data atau operasi dengan efek:

{lang="text"}
~~~~~~~~
  scala> List(true, false, true).ifM(List(0), List(1, 1))
  res: List[Int] = List(0, 1, 1, 0)
~~~~~~~~

Untuk masalah performa, pembaca budiman tidak perlu kuatir bila menggunakan
`ifM` dan `ap` karena kedua fungsi ini sudah dioptimalkan untuk menyimpan
hasil eksekusi cabang kode di tembolok dan menggunakannya kembali saat
kondisi terpenuhi. Sebagai contoh, silakan perhatikan contoh berikut

{lang="text"}
~~~~~~~~
  scala> List(true, false, true).flatMap { b => if (b) List(0) else List(1, 1) }
~~~~~~~~

yang menciptakan objek `List(0)` atau `List(1, 1)` baru tiap kali percabangan
dieksekusi.

A> Optimisasi semacam ini mungkin dilakukan pada pemrograman fungsional
A> yang disebabkan karena semua metoda adalah fungsi deterministik.
A> Fungsi semacam ini juga dikenal dengan *rujukan transparan*. 
A>
A> Bilamana sebuah metoda mengembalikan nilai yang berbeda tiap kali metoda
A> tersebut dipanggil, maka metoda ini dianggap tidak murni (lol, murni apa sih?)
A> dan mengaburkan penalaran dan optimisasi yang seharusnya bisa dilakukan.
A>
A> Bila `F` merupakan sebuah efek, anggap saja salah satu dari drone kita
A> atau aljabar Google, bukan berarti keluaran atas pemanggilan aljabar
A> tersebut disimpan di tembolok. Melainkan, rujukan pada operasi pemanggilan
A> tersebutlah yang disimpan. Sedangkan optimisasi performa dari `ifM` hanya
A> terlihat bila kita melakukan perbandingan atas struktur data. Selain itu
A> perbedaan semakin jelas terlihat bila tugas yang dikerjakan pada tiap
A> cabang semakin berat.
A>
A> Kita akan mengeksplorasi konsep atas determinisme dan penyimpanan nilai di
A> tembolok lebih lanjut pada bab selanjutnya.

Untuk pembaca yang menyukai operator sintaks, `Bind` juga menyediakan
sintaks khusus.

{lang="text"}
~~~~~~~~
  implicit class BindOps[F[_]: Bind, A] (self: F[A]) {
    def >>[B](b: =>F[B]): F[B] = Bind[F].bind(self)(_ => b)
    def >>![B](f: A => F[B]): F[A] = Bind[F].bind(self)(a => f(a).map(_ => a))
  }
~~~~~~~~

Operator `>>` biasa digunakan bila kita ingin membuang masukan `bind`.
Sebaliknya, `>>!` digunakan ketika kita ingin menjalankan sebuah efek
dan membuang keluarannya.


## *Applicative* dan *Monad*

Bila dipandang dari sudut pandang fungsionalitas, `Applicative` merupakan
`Apply` dengan metoda `pure`. Kurang lebih hal yang sama dengan `Monad`,
merupakan `Applicative` yang diperluas dengan menggabungkan `Bind`.

{width=100%}
![](images/scalaz-applicative.png)

{lang="text"}
~~~~~~~~
  @typeclass trait Applicative[F[_]] extends Apply[F] {
    def point[A](a: =>A): F[A]
    def pure[A](a: =>A): F[A] = point(a)
  }
  
  @typeclass trait Monad[F[_]] extends Applicative[F] with Bind[F]
~~~~~~~~

Setelah mempertimbangkan banyak hal, `Applicative` dan `Monad` bisa dianggap sebagai
puncak atas semua yang telah kita pelajari dari bab ini.
Sebagai contoh, `.pure`, atau `.point` bagi struktur data, acap kali digunakan
untuk menghasilkan efek ataupun struktur data dari nilai.

Untuk membuat instans `Applicative`, pembaca budiman harus menerapkan sifat-sifat
sebagai berikut:

-   **Identity**: `fa <*> pure(identity) === fa`, (where `fa` is an `F[A]`) i.e.
    applying `pure(identity)` does nothing.
-   **Homomorphism**: `pure(a) <*> pure(ab) === pure(ab(a))` (where `ab` is an `A =>
      B`), i.e. applying a `pure` function to a `pure` value is the same as applying
    the function to the value and then using `pure` on the result.
-   **Interchange**: `pure(a) <*> fab === fab <*> pure(f => f(a))`, (where `fab` is
    an `F[A => B]`), i.e. `pure` is a left and right identity
-   **Mappy**: `map(fa)(f) === fa <*> pure(f)`

-   **Identitas**: `fa <*> pure(identity) === fa` dimana `fa` merupakan sebuah `F[A]`.
    Sebagai contoh, pengaplikasian `pure(identity)` harus tidak mempunyai efek apapun
    tanpa mengubah apapun.
-   **Homomorfisme**: `pure(a) <*> pure(ab) === pure(ab(a))` dimana `ab` merupakan
    pemeteaan dari `A` ke `B` (`A => B`). Misalkan, penerapan sebuah fungsi `pure`
    ke sebuah nilai `pure` adalah sama dengan penerapan fungsi tersebut ke sebuah
    nilai yang sama dan dilanjutkan dengan menerapkan fungsi `pure` pada hasilnya.
-   **Komutatif**: `pure(a) <*> fab == fab <*> pure (f => f(a))` dimana `fab` merupakan
    sebuah `F[A => B]`. Contoh yang paling sederhana dari sifat ini adalah `pure`
    yang merupakan sebuah fungsi identitas baik untuk sisi kiri maupun sisi kanan.
-   **Mappy**: `map(fa)(f) === fa <*> pure(f)`.

Dan sifat tambahan untuk `Monad` diatas adalah:

-   **Identitas Kiri**: `pure(a).bind(f) === f(a)`.
-   **Identitas Kanan**: `a.bind(pure(_)) === a`.
-   **Asosiatif**: `fa.bind(f).bind(g) === fa.bind(a => f(a).bind(g))` dimana `fa`
    merupkana sebuah `F[A]`, `f` merupakan sebuah `A => F[B]` dan `g` merupakan
    `B => F[C]`.

Sifat asosiatif menentukan bahwa pemanggilan fungsi `bind` yang disambung harus
sesuai dengan fungsi `bind` lainnya. Walaupun bukan berarti kita bisa dengan
seenaknya mengubah urutan pemanggilan fungsi fungsi tersebut karena hal tersebut
adalah sifat komutatif. Sebagai contoh, `flatMap` yang merupakan alias dari `bind`
tidak dapat diubah dari

{lang="text"}
~~~~~~~~
  for {
    _ <- machine.start(node1)
    _ <- machine.stop(node1)
  } yield true
~~~~~~~~

menjadi

{lang="text"}
~~~~~~~~
  for {
    _ <- machine.stop(node1)
    _ <- machine.start(node1)
  } yield true
~~~~~~~~

karena `start` dan `stop` tidak bersifat komutatif. Tentu karena efek dari
kedua fungsi tersebut berbeda (bahkan berkebalikan!).

Berbeda halnya dengan penerapan sifat komutatif untuk pemanggilan beberapa `start`
maupun `stop`. Sebagai contoh, kita dapat menulis ulang fungsi berikut

{lang="text"}
~~~~~~~~
  for {
    _ <- machine.start(node1)
    _ <- machine.start(node2)
  } yield true
~~~~~~~~

menjadi

{lang="text"}
~~~~~~~~
  for {
    _ <- machine.start(node2)
    _ <- machine.start(node1)
  } yield true
~~~~~~~~

yang, bila menggunakan dipandang menggunakan kacamata aljabar kita, setara.
Tentu hal ini tidak bisa secara buta diterapkan ke semua aljabar.
Lalu, kenapa kita melakukan hal ini? Karena kita mengasumsikan banyak hal dari
Antarmuka Pemrograman Aplikasi dari Google Container yang kurang lebih cukup masuk
akal dilakukan.

Konsekuensi praktis dari hal ini adalah sebuah `Monad` harus bersifat komutatif
bila moteda `applyX` dapat dijalankan secara paralel. Dan pada Bab 3, kita mengambil
jalan pintas saat kita menjalankan efek efek ini secara paralel

{lang="text"}
~~~~~~~~
  (d.getBacklog |@| d.getAgents |@| m.getManaged |@| m.getAlive |@| m.getTime)
~~~~~~~~

karena kita tahu bahwa fungsi-fungsi diatas bersifat komutatif bila dijalankan
secara bebarengan. Bila nanti sudah waktunya untuk kita menerjemahkan aplikasi kita,
kita harus membuktikan bahwa efek-efek yang dihasilkan oleh fungsi-fungsi diatas
harus bersifat komutatif dan bila implementasi bersifat asinkronus, kita bisa saja
mengubah operasi menjadi bersifat berurutan, untuk menghindari kejadian yang
tidak diinginkan.

Mengenai seluk beluk tentang cara yang dianjurkan saat kita berurusan dengan
pengurutan efek dan apa saja efek efek yang ada, akan dibahas pada bab khusus
mengenai Monad Lanjutan.


## *Divide* dan *Conquer*

{width=100%}
![](images/scalaz-divide.png)

Sebagaimana yang terlihat pada gambar di atas, `Divide` berkorelasi
dengan `Contravariant` sebagaimana `Apply` berkorelasi dengan `Functor`.

{lang="text"}
~~~~~~~~
  @typeclass trait Divide[F[_]] extends Contravariant[F] {
    def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] = divide2(fa, fb)(f)
  
    def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = ...
    def divide2[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C] = ...
    ...
    def divide22[...] = ...
~~~~~~~~

`divide` menyatakan bahwa bila kita bisa memecah sebuah `C` menjadi sebuah
`A` dan `B`, dan kita mendapat sebuah `F[A]` dan `F[B]`, maka kita bisa
mendapatkan seubah `F[C]`.

Hal semacam ini sangat memudahkan kita untuk membuat instans kelas tipe
kontravarian untuk tipe produk dengan memecah produk-produk menjadi
bagian-bagian yang menyusunnya. Sebagai contoh, mari kita membuat sebuah
`Equal` untuk tipe produk baru, `Foo`, dari instans `Divide[Equal]` milik
Scalaz

{lang="text"}
~~~~~~~~
  scala> case class Foo(s: String, i: Int)
  scala> implicit val fooEqual: Equal[Foo] =
           Divide[Equal].divide2(Equal[String], Equal[Int]) {
             (foo: Foo) => (foo.s, foo.i)
           }
  scala> Foo("foo", 1) === Foo("bar", 1)
  res: Boolean = false
~~~~~~~~

Sebagaimana dengan `Apply`, `Divide` juga mempunyai sintaks untuk tuple.
Berikut merupakan contoh memecah belah lalu menguasai dalah menyelesaikan
permasalah pada perangkat lunak:

{lang="text"}
~~~~~~~~
  ...
    def tuple2[A1, A2](a1: F[A1], a2: F[A2]): F[(A1, A2)] = ...
    ...
    def tuple22[...] = ...
  }
~~~~~~~~

Secara umum, bila kelas tipe penyandi mampu menyediakan sebuah instans
dari `Divide`, dan tidak berhenti hanya pada `Contravariant`, adalah
sebuah hal yang tidak mustahil untuk menurunkan instans untuk semua
`case class`. Sama halnya dengan kelas tipe dekoder juga mampu menyediakan
instans `Apply`. Pembahasan mengenai penurunan kelas tipe akan dibahas
lebih lanjut pada bab berikutnya.

Seperti yang sudah dibahas pada beberapa paragraf di atas, `Divisible` ke
`Contravariant` adalah sama halnya dengan `Applicative` ke `Functor`.
Selain itu, kelas tipe ini menyediakan metoda `.conquer` yang sama dengan
`.pure`

{lang="text"}
~~~~~~~~
  @typeclass trait Divisible[F[_]] extends Divide[F] {
    def conquer[A]: F[A]
  }
~~~~~~~~

`.conquer` memperkenankan kita untuk membuat penerapan sederhana yang
mengabaikan parameter tipe. Nilai nilai tersebut biasa disebut sebagai
*terkuantifikasi secara umum* (lol, help me senpai. ;-; ). Sebagai contoh,
`Divisible[Equal].conquer[INil[String]]` akan mengembalikan sebuah
implementasi `Equal` untuk list `String` kosong yang akan selalu `true`.


## Plus

{width=100%}
![](images/scalaz-plus.png)

`Plus` merupakan `Semigroup` yang dikhususkan untuk konstruktor tipe.
Sedangkan `PlusEmpty` adalah padanan untuk `Monoid`. Untuk `IsEmpty`
lebih dikhususkan untuk menentukan apakah sebuah `F[A]` kosong atau tidak.

{lang="text"}
~~~~~~~~
  @typeclass trait Plus[F[_]] {
    @op("<+>") def plus[A](a: F[A], b: =>F[A]): F[A]
  }
  @typeclass trait PlusEmpty[F[_]] extends Plus[F] {
    def empty[A]: F[A]
  }
  @typeclass trait IsEmpty[F[_]] extends PlusEmpty[F] {
    def isEmpty[A](fa: F[A]): Boolean
  }
~~~~~~~~

Walaupun secara kasat mata `<+>` berperilaku seperti `|+|`

{lang="text"}
~~~~~~~~
  scala> List(2,3) |+| List(7)
  res = List(2, 3, 7)
  
  scala> List(2,3) <+> List(7)
  res = List(2, 3, 7)
~~~~~~~~

alangkah baiknya untuk menganggap operator ini hanya beroperasi pada `F[_]` dan
tidak melihat isi dari fungtor tersebut. `Plus` juga mempunyai konvensi untuk
selalu menghiraukan galat dan mengambil hasil operasi pertama. Maka dari itu,
operator `<+>` dapat digunakan sebagai mekanisme arus pendek dan penanganan galat
melalui gerakan mundur teratur:

{lang="text"}
~~~~~~~~
  scala> Option(1) |+| Option(2)
  res = Some(3)
  
  scala> Option(1) <+> Option(2)
  res = Some(1)
  
  scala> Option.empty[Int] <+> Option(1)
  res = Some(1)
~~~~~~~~

Sebagai contoh, bila kita mempunyai `NonEmptyList[Option[Int]]` dan kita ingin
menghiraukan nilai `None` beserta mengambil hasil yang pertama kali munncul,
kita akan memanggil `<+>` dari `Foldable1.foldRight1`:

{lang="text"}
~~~~~~~~
  scala> NonEmptyList(None, None, Some(1), Some(2), None)
         .foldRight1(_ <+> _)
  res: Option[Int] = Some(1)
~~~~~~~~

In fact, now that we know about `Plus`, we realise that we didn't need to break
typeclass coherence (when we defined a locally scoped `Monoid[Option[A]]`) in
the section on Appendable Things. Our objective was to "pick the last winner",
which is the same as "pick the winner" if the arguments are swapped. Note the
use of the TIE Interceptor for `ccy` and `otc` with arguments swapped.

Bahkan nyatanya, setelah kita tahu mengenai `Plus`, kita akan menyadari bahwa
kita tidak perlu merusak koherensi kelas tipe (saat mendefinisikan
sebuah `Monoid[Option[A]]` dengan cakupan lokal) pada seksi Appendable Things.
Tujuan kita adalah "mengambil hasil pertama yang ditemui" yang sama saja dengan
"mengambil hasil terakhir" bila argumen dibalik. Mohon diperhatikan, argumen
`<+>` untuk `ccy` dan `otc` ditukar termpatnya.

{lang="text"}
~~~~~~~~
  implicit val monoid: Monoid[TradeTemplate] = Monoid.instance(
    (a, b) => TradeTemplate(a.payments |+| b.payments,
                            b.ccy <+> a.ccy,
                            b.otc <+> a.otc),
    TradeTemplate(Nil, None, None)
  )
~~~~~~~~

`Applicative` dan `Monad` juga mempunyai versi khusus dari `PlusEmpty`

{lang="text"}
~~~~~~~~
  @typeclass trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F]
  
  @typeclass trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] {
    def unite[T[_]: Foldable, A](ts: F[T[A]]): F[A] = ...
  
    def withFilter[A](fa: F[A])(f: A => Boolean): F[A] = ...
  }
~~~~~~~~

`.unite` memperkenankan kita untuk menekuk struktur data menggunakan kontainer
`PlusEmpty[F].monoid` paling luar, bukan kontainer bagian dalam. Sebagai contoh,
untuk `List[Either[String, Int]]`, `Left[String]` lah yang akan dikonversi ke
`.empty`, bukan `List[A]`, yang akan dikonversi menjadi `.empty`. Dan dilanjutkan
dengan menggabungkan semuanya. Untuk pemrogram yang santai, metoda ini memberikan
kita kenyamanan untuk membuang galat galat yang mungkin terjadi.

{lang="text"}
~~~~~~~~
  scala> List(Right(1), Left("boo"), Right(2)).unite
  res: List[Int] = List(1, 2)
  
  scala> val boo: Either[String, Int] = Left("boo")
         boo.foldMap(a => a.pure[List])
  res: List[String] = List()
  
  scala> val n: Either[String, Int] = Right(1)
         n.foldMap(a => a.pure[List])
  res: List[Int] = List(1)
~~~~~~~~

`withFilter` allows us to make use of `for` comprehension language
support as discussed in Chapter 2. It is fair to say that the Scala
language has built-in language support for `MonadPlus`, not just
`Monad`!

`withFilter` memperkenankan kita untuk menggunakna dukungan komprehensi `for`
yang sudah dibahas pada Bab 2. Hal ini menunjukkan bahwa Scala
sudah mendukung `MonadPlus` dan tidak hanya `Monad` saja.

Kembali ke `Foldable`, kita akan menunjukkan beberapa metoda yang tidak
kita diskusikan sebelumnya

{lang="text"}
~~~~~~~~
  @typeclass trait Foldable[F[_]] {
    ...
    def msuml[G[_]: PlusEmpty, A](fa: F[G[A]]): G[A] = ...
    def collapse[X[_]: ApplicativePlus, A](x: F[A]): X[A] = ...
    ...
  }
~~~~~~~~

`msuml` akan mem-`fold` menggunakan `Monoid` dari `PlusEmpty[G]` dan
`collapse` mem-`foldRight` dengan menggunakan `PlusEmpty` dari tipe target:

{lang="text"}
~~~~~~~~
  scala> IList(Option(1), Option.empty[Int], Option(2)).fold
  res: Option[Int] = Some(3) // uses Monoid[Option[Int]]
  
  scala> IList(Option(1), Option.empty[Int], Option(2)).msuml
  res: Option[Int] = Some(1) // uses PlusEmpty[Option].monoid
  
  scala> IList(1, 2).collapse[Option]
  res: Option[Int] = Some(1)
~~~~~~~~


## Penyendiri

Beberapa kelas tipe pada Scalaz tidak dapat menjadi bagian dari hierarki
seperti `Monad`, `Applicative`, `Functor` dkk.

{width=80%}
![](images/scalaz-loners.png)


### **Zippy**

{lang="text"}
~~~~~~~~
  @typeclass trait Zip[F[_]]  {
    def zip[A, B](a: =>F[A], b: =>F[B]): F[(A, B)]
  
    def zipWith[A, B, C](fa: =>F[A], fb: =>F[B])(f: (A, B) => C)
                        (implicit F: Functor[F]): F[C] = ...
  
    def ap(implicit F: Functor[F]): Apply[F] = ...
  
    @op("<*|*>") def apzip[A, B](f: =>F[A] => F[B], a: =>F[A]): F[(A, B)] = ...
  
  }
~~~~~~~~

Metoda inti dari kelas tipe ini adalah `zip` yang bisa dianggap sebagai
`Divide.tuple2` yang kurang fleksibel. Dan bila terdapat sebuah `Functor[F]`
maka `zipWith` bisa berperilaku sebagaimana `Apply.apply2`.
Dan menariknya, sebuah `Apply[F]` dapat dibuat dengan mamnggil `ap` dan
mengaplikasikannya pada `Zip[F]` dan `Functor[F]`.

`apzip`, mirip dengan `Functor.fproduct` menerima sebuah `F[A]` dan sebuah
fungsi terangkat dari `F[A] => F[B]` dan menghasilkan sebuah `F[(A, B)]`.

A> Anggap operator `<*|*>` sebagai hiu pada film Jaws.

{lang="text"}
~~~~~~~~
  @typeclass trait Unzip[F[_]]  {
    @op("unfzip") def unzip[A, B](a: F[(A, B)]): (F[A], F[B])
  
    def firsts[A, B](a: F[(A, B)]): F[A] = ...
    def seconds[A, B](a: F[(A, B)]): F[B] = ...
  
    def unzip3[A, B, C](x: F[(A, (B, C))]): (F[A], F[B], F[C]) = ...
    ...
    def unzip7[A ... H](x: F[(A, (B, ... H))]): ...
  }
~~~~~~~~

Metoda utama dari kelas tipe `Unzip` adalah `unzip` dengan `firsts` 
dan `seconds` sebagai pemilih elemen pertama ataupun kedua dari pasangan
pada `F`. Dan yang paling penting adalah, `unzip` merupakan kebalikan
dari `zip`.

Metoda `unzip3` sampai `unzip7` merupakan pengaplikasian yang diulang
dari `unzip` untuk menghilangkan basa basi. Sebagai contoh, bila kita
menerima sebuah tuple berlapis, `Unzip[Id]` bisa dengan sigap meratakannya:

{lang="text"}
~~~~~~~~
  scala> Unzip[Id].unzip7((1, (2, (3, (4, (5, (6, 7)))))))
  res = (1,2,3,4,5,6,7)
~~~~~~~~

Pendek kata, `Zip` dan `Unzip` merupakan versi yang lebih kaku dari
`Divide` dan `Apply`. Selain itu, kedua kelas tipe sebelumnya menyediakan
fitur fitur berguna tanpa harus menyaratkan penggunaan `F`.


### *Optional*

Pada dasarnya, `Optional` adalah bentuk umum dari struktur data yang
mungkin mempunyai nilai, seperti `Option` dan `Either`.

Bila pembaca budiman ingat mengenai operator disjungsi (`\/`), operator
tersebut merupakan perbaikan atas `scala.Either`. Selain itu, Scalaz juga
memberikan operator lain sebagai peningkatan untuk `scala.Option`.

{lang="text"}
~~~~~~~~
  sealed abstract class Maybe[A]
  final case class Empty[A]()    extends Maybe[A]
  final case class Just[A](a: A) extends Maybe[A]
~~~~~~~~

{lang="text"}
~~~~~~~~
  @typeclass trait Optional[F[_]] {
    def pextract[B, A](fa: F[A]): F[B] \/ A
  
    def getOrElse[A](fa: F[A])(default: =>A): A = ...
    def orElse[A](fa: F[A])(alt: =>F[A]): F[A] = ...
  
    def isDefined[A](fa: F[A]): Boolean = ...
    def nonEmpty[A](fa: F[A]): Boolean = ...
    def isEmpty[A](fa: F[A]): Boolean = ...
  
    def toOption[A](fa: F[A]): Option[A] = ...
    def toMaybe[A](fa: F[A]): Maybe[A] = ...
  }
~~~~~~~~

Sebagaimana cuplikan diatas, tentu pembaca budiman cukup familiar
dengan metoda-metoda di atas. Satu metoda yang mungkin agak asing adalah
`pextract` yang menerima sebuah `Functor[A]` dan mengembalikan salah satu
dari `F[B]` atau nilai `a`. Sebagai contoh, `Optional[Option].pextract`
akan mengembalikan `Option[Nothing] \/ A`.

Selain itu, Scalaz juga memberikan operator terner untuk apapun yang
mempunyai kelas tipe `Optional`

{lang="text"}
~~~~~~~~
  implicit class OptionalOps[F[_]: Optional, A](fa: F[A]) {
    def ?[X](some: =>X): Conditional[X] = new Conditional[X](some)
    final class Conditional[X](some: =>X) {
      def |(none: =>X): X = if (Optional[F].isDefined(fa)) some else none
    }
  }
~~~~~~~~

sebagai contoh

{lang="text"}
~~~~~~~~
  scala> val knock_knock: Option[String] = ...
         knock_knock ? "who's there?" | "<tumbleweed>"
~~~~~~~~


## Co-

Tipe kelas dengan awalan "ko" pada umumnya, kelas ini merupakan lawan
dari kelas tipe yang diawali "ko" tadi. Walaupun, bukan berarti kelas tipe
ini selalu berupa invers. Untuk menunjukkan hubungan antara, misal, "sesuatu"
dengan "ko-sesuatu", kita akan mengikutsertakan penanda tipe dari "sesuatu"
bilamana memungkinkan.

{width=100%}
![](images/scalaz-cothings.png)

{width=80%}
![](images/scalaz-coloners.png)


### *Cobind*

{lang="text"}
~~~~~~~~
  @typeclass trait Cobind[F[_]] extends Functor[F] {
    def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
  //def   bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  
    def cojoin[A](fa: F[A]): F[F[A]] = ...
  //def   join[A](ffa: F[F[A]]): F[A] = ...
  }
~~~~~~~~

`cobind` (juga dikenal sebagai `coflatmap`) menerima sebuah `F[A] => B`
dan beroperasi atas `F[A]`, bukan `A`. Walaupun hal ini bukan berarti
`F[A]` harus benar benar merupakan fungtor dengan isi `A`. Biasanya,
`F[A]` yang dimaksud di sini merupakan sub-struktur yang didefinisikan
oleh `cojoin` (atau `coflatten`) yang mempunyai fungsi untuk memperluas
sebuah data struktur.

Contoh permasalahan yang cocok untuk diselesaikan oleh `Cobind` sebenarnya
cukup sulit untuk ditemui. Walaupun, sebagaimana yang diperlihatkan pada
tabel permutasi `Functor`, adalah sebuah hal yang sulit untuk menyanggah
mengenai penting atau tidaknya sebuah metoda bila dibandingka dengan metoda
lainnya:

| metoda      | parameter          |
|----------- |------------------ |
| `map`       | `A => B`           |
| `contramap` | `B => A`           |
| `xmap`      | `(A => B, B => A)` |
| `ap`        | `F[A => B]`        |
| `bind`      | `A => F[B]`        |
| `cobind`    | `F[A] => B`        |


### *Comonad*

{lang="text"}
~~~~~~~~
  @typeclass trait Comonad[F[_]] extends Cobind[F] {
    def copoint[A](p: F[A]): A
  //def   point[A](a: =>A): F[A]
  }
~~~~~~~~

`.copoint` (atau `.copure`) mengelupas sebuah elemen dari konteks yang
melingkupinya. Efek biasanya tidak mempunyai instans `Comonad` karena
`Comonad` akan menghapus transparansi rujukan saat penginterpretasian sebuah
`IO[A]` menjadi `A`. Namun, untuk struktur data koleksi, penggunaan `.copoint`
merupakan salah satu cara untuk mengakses semua elemen beserta elemen
yang berdekatan dengannya.

Misalkan, sebuah *daerah sekeliling* (disingkat `Hood` dari *neighbourhood*) yang
terdiri atas sebuah senarai elemen bagian kiri (`lefts`), elemen yang dilihat
(`focus`), dan sebuah senarai elemen pada bagian kanan (`rights`).

{lang="text"}
~~~~~~~~
  final case class Hood[A](lefts: IList[A], focus: A, rights: IList[A])
~~~~~~~~

Struktur `lefts` dan `rights` harus dibuat dengan yang dimulai dari
`focus` lalu semakin menjauh bila ditambahkan, sehingga kita bisa
mendapatkan kembali `IList` awal dengan metoda `.toIList`.

{lang="text"}
~~~~~~~~
  object Hood {
    implicit class Ops[A](hood: Hood[A]) {
      def toIList: IList[A] = hood.lefts.reverse ::: hood.focus :: hood.rights
~~~~~~~~

Kita dapat menulis metoda-metoda untuk memindah fokus ke kiri (`previous`)
ataupun ke kanan (`next`)

{lang="text"}
~~~~~~~~
  ...
      def previous: Maybe[Hood[A]] = hood.lefts match {
        case INil() => Empty()
        case ICons(head, tail) =>
          Just(Hood(tail, head, hood.focus :: hood.rights))
      }
      def next: Maybe[Hood[A]] = hood.rights match {
        case INil() => Empty()
        case ICons(head, tail) =>
          Just(Hood(hood.focus :: hood.lefts, head, tail))
      }
~~~~~~~~

Dengan mengaplikasikan `more` atas sebuah fungsi opsional secara berulang
kepada sebuah `Hood`, kita dapat menghitung *semua* `positions` yang
bisa digunakan pada `Hood` yang bersangkutan

{lang="text"}
~~~~~~~~
  ...
      def more(f: Hood[A] => Maybe[Hood[A]]): IList[Hood[A]] =
        f(hood) match {
          case Empty() => INil()
          case Just(r) => ICons(r, r.more(f))
        }
      def positions: Hood[Hood[A]] = {
        val left  = hood.more(_.previous)
        val right = hood.more(_.next)
        Hood(left, hood, right)
      }
    }
~~~~~~~~

Sekarang, kita dapat mengimplementasikan `Comonad[Hood]`

{lang="text"}
~~~~~~~~
  ...
    implicit val comonad: Comonad[Hood] = new Comonad[Hood] {
      def map[A, B](fa: Hood[A])(f: A => B): Hood[B] =
        Hood(fa.lefts.map(f), f(fa.focus), fa.rights.map(f))
      def cobind[A, B](fa: Hood[A])(f: Hood[A] => B): Hood[B] =
        fa.positions.map(f)
      def copoint[A](fa: Hood[A]): A = fa.focus
    }
  }
~~~~~~~~

`cojoin` memberikan kita sebuah `Hood[Hood[IList]]` yang berisi semua
`Hood` ynag mungkin pada `IList` awal kita

{lang="text"}
~~~~~~~~
  scala> val middle = Hood(IList(4, 3, 2, 1), 5, IList(6, 7, 8, 9))
  scala> middle.cojoin
  res = Hood(
          [Hood([3,2,1],4,[5,6,7,8,9]),
           Hood([2,1],3,[4,5,6,7,8,9]),
           Hood([1],2,[3,4,5,6,7,8,9]),
           Hood([],1,[2,3,4,5,6,7,8,9])],
          Hood([4,3,2,1],5,[6,7,8,9]),
          [Hood([5,4,3,2,1],6,[7,8,9]),
           Hood([6,5,4,3,2,1],7,[8,9]),
           Hood([7,6,5,4,3,2,1],8,[9]),
           Hood([8,7,6,5,4,3,2,1],9,[])])
~~~~~~~~

Dan memang, `cojoin` sebenarnya adalah `positions`! Tentu, kita dapat
meng-`override`-nya dengan implementasi yang lebih lugas dan performan.

{lang="text"}
~~~~~~~~
  override def cojoin[A](fa: Hood[A]): Hood[Hood[A]] = fa.positions
~~~~~~~~

`Comonad` menggeneralisasi konsep `Hood` untuk semua struktur data.
`Hood` merupakan sebuah contoh dari *zipper* (tidak ada hubungannya
dengan `Zip`). Scalaz sendiri juga mempunyai sebuah tipe data `Zipper`
yang berhubungan dengan aliran (mis, struktur 1 dimensi tak hingga),
yang akan kita bahan pada bab selanjutnya.

Salah satu penggunaan dari sebuah zipper adalah *automata seluler*
yang menghitung nilai tiap sel generasi selanjutnya dengan melakukan
penghitungan terhadap sel-sel di sekeliling sel tadi.


### *Cozip*

{lang="text"}
~~~~~~~~
  @typeclass trait Cozip[F[_]] {
    def cozip[A, B](x: F[A \/ B]): F[A] \/ F[B]
  //def   zip[A, B](a: =>F[A], b: =>F[B]): F[(A, B)]
  //def unzip[A, B](a: F[(A, B)]): (F[A], F[B])
  
    def cozip3[A, B, C](x: F[A \/ (B \/ C)]): F[A] \/ (F[B] \/ F[C]) = ...
    ...
    def cozip7[A ... H](x: F[(A \/ (... H))]): F[A] \/ (... F[H]) = ...
  }
~~~~~~~~

Walaupun dinamai sebagai `cozip`, kelas tipe ini mungkin lebih cocok
bila dibicarakan sebagai simetri dari `unzip`. Bilamana `unzip` memisah
`F[_]` dari tuple (produk) menjadi tuple `F[_]`, `cozip` memisah `F[_]`
dari disjungsi (ko-produk) menjadi disjungsi `F[_]`.

## Bi-

Seringkali kita menemui keadaan dimana kita mempunyai sebuah benda yang
mempunyai dua tipe, dan kita ingin memetakan keduanya ke kategori lain.
Sebagai contoh, mungkin kita ingin melacak galat pada bagian kiri sebuah
`Either` dan ingin melakukan sesuatu pada pesan galat tersebut. 

Kelas tipe `Functor` / `Foldable` / `Traverse` mempunyai sepupu yang
janggal yang memperkenankan kita untuk memetakan dari satu kategori
ke kategori lainnya secarabolak balik. 

{width=30%}
![](images/scalaz-bithings.png)

{lang="text"}
~~~~~~~~
  @typeclass trait Bifunctor[F[_, _]] {
    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
  
    @op("<-:") def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = ...
    @op(":->") def rightMap[A, B, D](fab: F[A, B])(g: B => D): F[A, D] = ...
    @op("<:>") def umap[A, B](faa: F[A, A])(f: A => B): F[B, B] = ...
  }
  
  @typeclass trait Bifoldable[F[_, _]] {
    def bifoldMap[A, B, M: Monoid](fa: F[A, B])(f: A => M)(g: B => M): M
  
    def bifoldRight[A,B,C](fa: F[A, B], z: =>C)(f: (A, =>C) => C)(g: (B, =>C) => C): C
    def bifoldLeft[A,B,C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C = ...
  
    def bifoldMap1[A, B, M: Semigroup](fa: F[A,B])(f: A => M)(g: B => M): Option[M] = ...
  }
  
  @typeclass trait Bitraverse[F[_, _]] extends Bifunctor[F] with Bifoldable[F] {
    def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[A, B])
                                                 (f: A => G[C])
                                                 (g: B => G[D]): G[F[C, D]]
  
    def bisequence[G[_]: Applicative, A, B](x: F[G[A], G[B]]): G[F[A, B]] = ...
  }
~~~~~~~~

A> `<-:` dan `:->` merupakan operator om senang!

Walaupun penanda tipe dari metoda-metoda di atas bisa
dikatakan sangat lantung, mereka tidak lain dan tidak bukan hanyalah
metoda inti dari `Functor`, `Foldable`, dan `Bitraverse` yang menerima
dua fungsi, bukan satu. Selain itu, metoda-metoda tadi juga memaksa
kedua fungsi untuk mengembalikan tipe yang sama dengan pertimbangan
keluaran mereka dapat digabungkan menggunakan `Monoid` ataupun `Semigroup`.

{lang="text"}
~~~~~~~~
  scala> val a: Either[String, Int] = Left("fail")
         val b: Either[String, Int] = Right(13)
  
  scala> b.bimap(_.toUpperCase, _ * 2)
  res: Either[String, Int] = Right(26)
  
  scala> a.bimap(_.toUpperCase, _ * 2)
  res: Either[String, Int] = Left(FAIL)
  
  scala> b :-> (_ * 2)
  res: Either[String,Int] = Right(26)
  
  scala> a :-> (_ * 2)
  res: Either[String, Int] = Left(fail)
  
  scala> { s: String => s.length } <-: a
  res: Either[Int, Int] = Left(4)
  
  scala> a.bifoldMap(_.length)(identity)
  res: Int = 4
  
  scala> b.bitraverse(s => Future(s.length), i => Future(i))
  res: Future[Either[Int, Int]] = Future(<not completed>)
~~~~~~~~

Sebagai tamabahan, kita dapat meninjau kembali `MonadPlus` (yang merupakan
`Monad` dengan tambahan `filterWith` dan `unite`) dan mempertimbangkan
apakah kelas tipe ini bisa memisah konten `Bifoldable` dari sebuah `Monad`

{lang="text"}
~~~~~~~~
  @typeclass trait MonadPlus[F[_]] {
    ...
    def separate[G[_, _]: Bifoldable, A, B](value: F[G[A, B]]): (F[A], F[B]) = ...
    ...
  }
~~~~~~~~

Hal ini sangat berguna bila kita mempunyai sebuah koleksi dari bi-things
dan kita ingin me-reorganisasi menjadi sebuah koleksi atas `A` dan
koleksi atas `B`

{lang="text"}
~~~~~~~~
  scala> val list: List[Either[Int, String]] =
           List(Right("hello"), Left(1), Left(2), Right("world"))
  
  scala> list.separate
  res: (List[Int], List[String]) = (List(1, 2), List(hello, world))
~~~~~~~~


## Kesimpulan

Sesungguhnya, materi pada bab ini cukup banyak dan kita sudah mengeksplorasi
pustaka standar untuk fungsionalitas polimorfis. Namun, bila kita harus
membandingkan satu dengan lainnya, pustaka standar Koleksi milik Scala
memiliki *trait* yang jauh lebih banyak bila dibandingkan dengan kelas
tipe yang dimiliki oleh Scalaz.

Adalah hal yang jamak ditemui bila sebuah aplikasi pemrograman fungsional
hanya menyentuh sebagian kecil dari hierarki kelas tipe. Hal itu juga
dengan mempertimbangkan bahwa kebanyakan fungsionalitas berasal dari aljabar
spesifik domain dan kelas tipe. Bahkan bila kelas tipe yang spesifik
pada domain hanya merupakan salinan khusus dari kelas tipe Scalaz,
kita bisa melakukan refaktor di lain waktu.

Sebagai tambahan, kita juga sudah mengikutsertakan contekan untuk kelas
tipe dan metoda utamanya pada Lampiran. Contekan ini mendapatkan inspirasi
dari [Contekan Scalaz](http://arosien.github.io/scalaz-cheatsheets/typeclasses.pdf)
yang ditulis oleh Adam Rosien.

Lebih lanjut, Valentin Kasas menjelaskan mengenai [Penggabungan `N` thing](https://twitter.com/ValentinKasas/status/879414703340081156):

{width=70%}
![](images/shortest-fp-book.png)


# Tipe Data Scalaz

Siapa yang tidak suka dengan struktur data keren? Tentu tidak ada, karena
semua struktur data keren!

Pada bab ini, kita akan mengeksplorasi tipe data *seperti koleksi*  yang ada
pada Scalaz dan juga tipe data yang memperkaya Scala dengan semantik multi guna
dan keamanan tipe data.

Alasan utama kita memberi perhatian lebih terhadap banyaknya jenis koleksi
yang kita miliki adalah performa. Sebuah vektor dan list mampu melakukan hal
yang sama, namun karakteristik performa mereka berbeda: sebuah vektor mempunyai
beban pencarian konstan sendangkan list harus melangkahi elemen satu per satu.

W> Estimasi performa - termasuk klai pada bab ini - tak perlu dianggap secara serius.
W> Desain prosesor medore, penyaluran memori, dan pengumpulan sampah JVM bisa
W> saja mengahpuskan penalaran intuitif berdasarkan penghitungan operasi.
W> 
W> Kenyataan mutlak pada komputer modern pada tes performa empiris, untuk tugas
W> khusus, hasil yang muncul bisa saja mengejutkan: mis., `lookup` pada sebuuah
W> `List` sering kali lebih cepat bila dibandingkan dengan sebuah `Vector`.
W> Sangat disarankan untuk menggunakan alat bantu seperti [JMH](http://openjdk.java.net/projects/code-tools/jmh) pada saat melakukan tes performa.

Semua koleksi yang ditunjukkan di sini bersifat *persisten*: blia kita menambah
ataupun menghapus sebuah elemen, kita masih bisa menggunakan koleksi sebelumnya.
Pembagian struktural merupakan bagian penting bila kita berbicara mengenai performa
dari struktur data persisten. Bila kita tidak memperhatikan pembagian struktural,
koleksi akan dibuat ulang setiap kali operasi atas koleksi tersebut dilakukan.

Tidak seperti koleksi pada pustaka standar Java dan Scala, Scalaz tidak mempunyai
hierarki tipe data: koleksi-koleksi ini lebih sederhana dan mudah dipahami.
Fungsionalitas polimorfis tersedia dengan mengoptimisasi instans dari kelas 
tipe yang telah kita pelajari pada bab sebelumnya. Penggunaan instans kelas tipe
sangat mempermudah kita dalam menukar implementasi dengan alasan performa ataupun
dengan membuat implementasi kita sendiri.

## Varian Tipe

Banyak dari tipe data Scalaz mempunyai parameter tipe yang bersifat *invarian*.
Sebagai contoh, `IList[A]` bukan merupakan sub-tipe dari `IList[B]` walau
`A <: B`.


### Kovarian

Salah satu permasalahan dari parameter tipe *kovarian*, seperti `class
List[+A]`, adalah `List[A]` juga merupakan sub-tipe dari `List[Any]`.
Hal semacam ini sangat mempermudah hilangnya informasi tipe.

{lang="text"}
~~~~~~~~
  scala> List("hello") ++ List(' ') ++ List("world!")
  res: List[Any] = List(hello,  , world!)
~~~~~~~~

Harap perhatikan bahwa list kedua merupakan `List[Char]` dan kompilator
menyimpulkan, walaupun ngaco, bahwa *Batas Atas Terendah* (BAT) sebagai
`Any`. Bila dibandingkan dengan `IList`, yang mengharuskan `.widen[Any]`
secara eksplisit untuk memberikan celah untuk kecerobohan semacam ini:

{lang="text"}
~~~~~~~~
  scala> IList("hello") ++ IList(' ') ++ IList("world!")
  <console>:35: error: type mismatch;
   found   : Char(' ')
   required: String
  
  scala> IList("hello").widen[Any]
           ++ IList(' ').widen[Any]
           ++ IList("world!").widen[Any]
  res: IList[Any] = [hello, ,world!]
~~~~~~~~

Hal yang sama juga terjadi ketika kompilator menyimpulkan bahwa
sebuah tipe `with Product with Serializable` (yang berupa *Product*
dan *Serializable*), hal semacam ini merupakan indikator yang kuat
bahwa pelebaran tanpa sengaja (lol, aku bego) telah terjadi
dikarenakan kovarian.

Sayangnya, kita harus berhati-hati saat menyusun tipe data invarian
dikarenakan kalkulasi BAT dilakukan pada parameter:

{lang="text"}
~~~~~~~~
  scala> IList("hello", ' ', "world")
  res: IList[Any] = [hello, ,world]
~~~~~~~~

Masalah yang mirip dengan hal ini juga terjadi pada tipe `Nothing` milik Scala,
yang merupakan sub-tipe dari semua tipe, termasuk ADT `sealed`, kelas `final`,
primitif, dan `null`.

Dikarenakan tidak ada nilai dari tipe `Nothing`: fungsi yang menerima `Nothing`
sebagai salah satu parameter tidak dapat dijalankan dan fungsi yang mengembalikan
`Nothing` tidak akan mengembalikan kembaliannya.
`Nothing` pada awalnya diperkenalkan sebagai sebuah mekanisme untuk memperkenankan
kovarian pada parameter tipe. Walaupun, sebagai konsekuensinya yang tak disengaja,
kita juga bisa menghasilkan kode yang tak bisa dijalankan. Di sisi lain Scalaz
berpendapat bahwa kita tidak butuh parameter tipe kovarian. Hal ini berarti
bahwa kita membatasi diri kita untuk hanya menulis kode yang bisa dijalankan saja.


### Kontrarivarian

Agak berbeda dengan kovarian, parameter tipe *kontravarian*, seperti
`trait Thing[-A]`, bisa menimbulkan masalah tak terduga sebagaimana
yang ditunjukkan pada [kutu di kompilator](https://issues.scala-lang.org/browse/SI-2509).
Paul Phillips (bekas anggota tim `scalac`) juga telah mendemonstrasikan
apa yang dia sebut sebagai *kontrari-varian*.

{lang="text"}
~~~~~~~~
  scala> :paste
         trait Thing[-A]
         def f(x: Thing[ Seq[Int]]): Byte   = 1
         def f(x: Thing[List[Int]]): Short  = 2
  
  scala> f(new Thing[ Seq[Int]] { })
         f(new Thing[List[Int]] { })
  
  res = 1
  res = 2
~~~~~~~~

Sebagaimana yang telah pembaca yang budiman terka, kompilator berhasil
menentukan argumen paling spesifik untuk setiap pemanggilan `f`.
Namun, resolusi implisit dari kompilator memberikan hasil yang tak terduga:

{lang="text"}
~~~~~~~~
  scala> :paste
         implicit val t1: Thing[ Seq[Int]] =
           new Thing[ Seq[Int]] { override def toString = "1" }
         implicit val t2: Thing[List[Int]] =
           new Thing[List[Int]] { override def toString = "2" }
  
  scala> implicitly[Thing[ Seq[Int]]]
         implicitly[Thing[List[Int]]]
  
  res = 1
  res = 1
~~~~~~~~

Resolusi implisit membalik definisi kompilator atas "argumen paling spesifik"
untuk tipe kontravarian sehingga argumen tersebut menjadi percuma bila digunakan
dengan kelas tipe maupun semua yang menggunakan fungsionalitas polimorfis.
Perilaku semacam ini sudah dibenahi pada Dotty.


### Batasan dari Pembuatan Subtipe

Sebagaimana yang telah pembaca yang budiman ketahui, `scala.Option`
mempunyai metoda `.flatten` yang akan mengubah `Option[Option[B]]` menjadi
`Option[B]`. Namun, sistem tipe Scala akan menggagalkan usaha kita
untuk menuliskan penanda tipe untuk metoda tersebut.
Mohon perhatikan pada contoh berikut yang terlihat benar anmun
mempunyai sebuah kutu yang hampir tak kasat mata:

{lang="text"}
~~~~~~~~
  sealed abstract class Option[+A] {
    def flatten[B, A <: Option[B]]: Option[B] = ...
  }
~~~~~~~~

`A` yang diperkenalkan pada `.flatten` membayangi `A` yang diperkenalkan
pada kelas. Hal seperti ini sama saja dengan menuliskan

{lang="text"}
~~~~~~~~
  sealed abstract class Option[+A] {
    def flatten[B, C <: Option[B]]: Option[B] = ...
  }
~~~~~~~~

yang berbeda dengan batasan yang kita inginkan.

Untuk menyiasati batasan ini, Scala mendefinisikan kelas infiks `<:<` 
dan `=:=` beserta bukti implisit yang selalu meninggalkan sebuah *saksi*

{lang="text"}
~~~~~~~~
  sealed abstract class <:<[-From, +To] extends (From => To)
  implicit def conforms[A]: A <:< A = new <:<[A, A] { def apply(x: A): A = x }
  
  sealed abstract class =:=[ From,  To] extends (From => To)
  implicit def tpEquals[A]: A =:= A = new =:=[A, A] { def apply(x: A): A = x }
~~~~~~~~

`=:=` bisa digunakan untuk memaksa kedua parameter tipe benar benar
sama. Sedangkan `<:<` digunakan untuk mendeskripsikan hubungan sub-tipe.
Kedua kelas tersebut memperkenankan kita untuk mengimplementasikan
`.flatten` sebagai

{lang="text"}
~~~~~~~~
  sealed abstract class Option[+A] {
    def flatten[B](implicit ev: A <:< Option[B]): Option[B] = this match {
      case None        => None
      case Some(value) => ev(value)
    }
  }
  final case class Some[+A](value: A) extends Option[A]
  case object None                    extends Option[Nothing]
~~~~~~~~

Scalaz memperbaiki kedua kelas tadi dengan menggunakan *Liskov* 
(dialiaskan sebagai `<~<`) dan Leibniz (`===`).

{lang="text"}
~~~~~~~~
  sealed abstract class Liskov[-A, +B] {
    def apply(a: A): B = ...
    def subst[F[-_]](p: F[B]): F[A]
  
    def andThen[C](that: Liskov[B, C]): Liskov[A, C] = ...
    def onF[X](fa: X => A): X => B = ...
    ...
  }
  object Liskov {
    type <~<[-A, +B] = Liskov[A, B]
    type >~>[+B, -A] = Liskov[A, B]
  
    implicit def refl[A]: (A <~< A) = ...
    implicit def isa[A, B >: A]: A <~< B = ...
  
    implicit def witness[A, B](lt: A <~< B): A => B = ...
    ...
  }
  
  // type signatures have been simplified
  sealed abstract class Leibniz[A, B] {
    def apply(a: A): B = ...
    def subst[F[_]](p: F[A]): F[B]
  
    def flip: Leibniz[B, A] = ...
    def andThen[C](that: Leibniz[B, C]): Leibniz[A, C] = ...
    def onF[X](fa: X => A): X => B = ...
    ...
  }
  object Leibniz {
    type ===[A, B] = Leibniz[A, B]
  
    implicit def refl[A]: Leibniz[A, A] = ...
  
    implicit def subst[A, B](a: A)(implicit f: A === B): B = ...
    implicit def witness[A, B](f: A === B): A => B = ...
    ...
  }
~~~~~~~~

Selain metoda-metoda umum yang tentu berguna dan konversi implisit,
bukti dari kelas `<~<` dan `===` lebih memegang prinsip bila dibandingkan
dengan kelas `<:<` dan `=:=` milik pustaka standar.


A> Kelas Liskov dinamai berdasarkan Barbara Liskov yang meletakkan
A> batu pondasi Pemrograman Berorientasi Objek dengan mengemukakan
A> prinsip substitusi Liskov.
A>
A> Gottfried Wilhelm Leibniz pada dasarnya menciptakan semua *yang* ada
A> pada abad ke 17. Dia percaya pada [Tuhan yang disebut Monad](https://en.wikipedia.org/wiki/Monad_(philosophy)).
A> Nama tersebut juga digunakan untuk apa yang kita ketahui sebagai `scalaz.Monad`
A> oleh Eugenio Moggi.


## Evaluasi

Pada bahasa pemrograman Java, evaluasi program dijalankan secara tegas:
semua parameter dari sebuah metoda harus dievaluasi menjadi sebuah *nilai*
sebelum metoda tersebut dipanggil. Scala, di sisi lain, memperkenalkan
istilah parameter *by-name* (lol, help) pada metoda dengan sintaks `a: => A`.
Parameter ini dibungkus (lol) sebagai fungsi tanpa argumen yang dipanggil
tiap kali `a` dirujuk. Seperti yang telah kita lihat pada bab-bab sebelumnya,
kelas tipe cenderung menggunakan parameter *by-name*.

Scala juga mempunyai strategi evaluasi nilai berdasarkan pemanggilan *by-need*,
menggunakan kata kunci `lazy`: komputasi dilakukan paling banyak satu kali
ketika nilai parameter akan digunakan. Sayangnya, scala tidak mendukung
evaluasi komputasi dengan pemanggilan *by-need* pada parameter metoda.

A> Bila kalkulasi `lazy val` melempar pengecualian, kalkulasi ini akan
A> diulang tiap kali nilai ini diakses. Karena pengecualian dapat merusak
A> transparansi rujukan, kita akan membatasi diskusi kita hanya sampai pada
A> kalkulasi `lazy val` yang tidak melempar pengecualian.

Scalaz memformalisasi tiga strategi evaluasi yang menggunakan TDA

{lang="text"}
~~~~~~~~
  sealed abstract class Name[A] {
    def value: A
  }
  object Name {
    def apply[A](a: =>A) = new Name[A] { def value = a }
    ...
  }
  
  sealed abstract class Need[A] extends Name[A]
  object Need {
    def apply[A](a: =>A): Need[A] = new Need[A] {
      private lazy val value0: A = a
      def value = value0
    }
    ...
  }
  
  final case class Value[A](value: A) extends Need[A]
~~~~~~~~

Bentuk evaluasi paling lemah adalah `Name` yang tidak memberikan
jaminan komputasi. Selanjutnya adalah `Need`, yang menjamin evaluasi
*paling banyak satu kali*. Dan evaluasi `Value` yang merupakan nilai
hasil dari komputasi yang terjadi sebelum pemanggilan terjadi.
Evaluasi `Value` menjamin satu kali evaluasi.

Bila kita berbengah diri, bisa saja kita munder ke kelas tipe dan membuat
metoda mereka untuk secara spesifik menerima parameter `Name`, `Need`,
atau `Value`. Namun, kita memilih untuk mengasumsikan bahwa parameter
normal akan selalu dibungkus dalam sebuah `Value` dan parameter *by-name*
dapat dibungkus dengan `Name`.

Ketika kita menulis *program murni* (lol), kita bebas untuk mengganti
`Name` dengan `Need` atau `Value`, begitu juga sebaliknya, tanpa
mengubah kebenaran program (lol). Yang menjadi esensi dari *transparansi
rujukan* adalah keluwesan untuk mengganti sebuah komputasi dengan nilai
komputasi tersebut atau mengganti nilai sebuah komputasi dengan komputasi
itu sendiri.

Pada pemrograman fungsional, kita hampir selalu menggunakan `Value` atau
`Need` (dikenal dengan *tegas* dan *lundung*) dikarenakan hampir tidak ada
untungnya menggunakan `Name` secara eksplisit.
Hal ini dikarenakan tidak dukungan pada tingkat bahasa untuk parameter
metoda yang dipanggil secara lundung. Metoda secara umum meminta parameter
*by-name* lalu mengubahnya menjadi `Need` secara internal agar mendapatkan
tambahan performa.

A> `Lazy` seringkali  digunakan pada pustaka inti Scala untuk tipe data
A> dengan semantik *by-name* walaupun hal ini adalah sebuah salah kaprah.
A>
A> Secara umum, kita tidak begitu semangat bila membicarakan evaluasi lundung:
A> mungkin akan lebih singkat bila kita mengklarifikasi tentang evaluasi lundung
A> apa yang sedang didiskusikan. Hal ini dikarenakan

`Name` menyediakan instans dari kelas tipe berikut:

-   `Monad`
-   `Comonad`
-   `Traverse1`
-   `Align`
-   `Zip` / `Unzip` / `Cozip`

A> Pemanggilan *by name* yang lundung dan *lundung* tidak datang cuma cuma.
A> Saat Scala mengkonversi parameter *by-name* dan `lazy val` menjadi
A> bytecode akan selalu ada ongkons tambahan pada saat alokasi objek. 
A>
A> Selalu pastikan agar ongkos tambahan tidak lebih besar daripada penghematan
A> saat mengubah metoda untuk menggunakan pemanggilan parameter *by-name*.
A> Selain itu, pembaca yang budiman hanya bisa merasakan keuntungan dan manfaat
A> dari pemanggilan parameter *by-name* bila ada kemungkinan *tidak* dievaluasi-nya
A> sebuah statemen.
A> Selain itu, kode dengan performa tinggi dalam sebuah ikalan ketat atau
A> selalu mengevaluasi akan kehilangan performa bila menggunakan strategi
A> pemanggilan ini.


## Memoisasi

Scalaz mampu melakukan memoisasi fungsi yang belum pasti akan selalu
dievaluasi dikarenakan bermacamnya implementasi.
Secara formal, memoisasi diwakilkan dengan `Memo`:

{lang="text"}
~~~~~~~~
  sealed abstract class Memo[K, V] {
    def apply(z: K => V): K => V
  }
  object Memo {
    def memo[K, V](f: (K => V) => K => V): Memo[K, V]
  
    def nilMemo[K, V]: Memo[K, V] = memo[K, V](identity)
  
    def arrayMemo[V >: Null : ClassTag](n: Int): Memo[Int, V] = ...
    def doubleArrayMemo(n: Int, sentinel: Double = 0.0): Memo[Int, Double] = ...
  
    def immutableHashMapMemo[K, V]: Memo[K, V] = ...
    def immutableTreeMapMemo[K: scala.Ordering, V]: Memo[K, V] = ...
  }
~~~~~~~~

`memo` memperkenankan kita untuk membuat implementasi khusus atas kelas
tipe `Memo`. Sedangkan untuk `nilMemo`, metoda ini tidak melakukan
memoisasi. Dengan kata lain, `nilMemo` mengevaluasi fungsi secara normal.
Untuk implementasi metoda lainnya, mereka hanya mencegat pemanggilan
fungsi dan nilai yang tersimpan di tembolok dengan menggunakan
implementasi dari pustaka koleksi standar.

Untuk menggunakan `Memo`, kita hanya perlu membungkus sebuah fungsi dengan
implmentasi `Memo` dan  dilanjutkan dengan memanggil fungsi ter-memoisasi
tadi:

{lang="text"}
~~~~~~~~
  scala> def foo(n: Int): String = {
           println("running")
           if (n > 10) "wibble" else "wobble"
         }
  
  scala> val mem = Memo.arrayMemo[String](100)
         val mfoo = mem(foo)
  
  scala> mfoo(1)
  running // evaluated
  res: String = wobble
  
  scala> mfoo(1)
  res: String = wobble // memoised
~~~~~~~~

Bila sebuah fungsi menerima lebih dari sebuah parametr, kita harus mengubah
parameter-parameter tadi menjadi sebuah *tuple* menggunakan metoda `tupled`
sehingga fungsi tadi berubah menjadi fungsi ter-memoisasi yang menerima
sebuah *tuple*.

{lang="text"}
~~~~~~~~
  scala> def bar(n: Int, m: Int): String = "hello"
         val mem = Memo.immutableHashMapMemo[(Int, Int), String]
         val mbar = mem((bar _).tupled)
  
  scala> mbar((1, 2))
  res: String = "hello"
~~~~~~~~

`Memo` pada dasarnya dianggap sebagai konstruk khusus dan penegakan
aturan mengenai *kemurnian* (lol, help) sedikit lebih longgar dengan
alasan memudahkan implmentasi. Agar tetap murni, yang perlu
kita lakukan hanyalah memastikan implementasi `Memo` yang kita buat
untuk selalu secara melakukan transparansi saat merujuk pada saat evaluasi
`K => V`. Kita bisa juga menggunakan data yang bisa bermutasi dan melakukan
`I/O` pada implementasi `Memo`, misal dengan LRU atau tembolok terdistribusi
tannpa harus mendeklarasikan efek pada penanda tipe. Bahasa pemrograman
fungsional lainnya punya mekanisme memoisasi terotomatis yang diatur oleh
lingkungan waktu jalan mereka. `Memo` di sisi lain, merupakan satu-satunya
cara kita untuk menambal JVM agar mempunyai dukungan yang mirip. 


## Pelabelan

Pada bagian dimana kita memperkenalkan kelas tipe `Monoid` , kita membuat sebuah
instans `Monoid` untuk `TradeTemplate` (yang disimbolkan dengan `Monoid[TradeTemplate]`).
Namun, kita juga menemukan bahwa perilaku Scalaz tidak sesuai dengan ekspektasi
kita terhadap `Monoid[Option[A]]`. Perbedaan perilaku semacam ini bukan sebuah
keluputan dari Scalaz: Seringkali kita akan mendapatkan tipe data yang bisa menerapkan
kelas tipe mendasar dengan banyak cara, namun tidak berperilaku sesuai dengan yang
kita inginkan.

Contoh sederhana atas permasalahan seperti ini adalah `Monoid[Boolean]` (konjungsi `&&`
dan disjungsi `||`) dan `Monoid[Int]` (perkalian dan penjumlahan).

Untuk menerapkan `Monoid[TradeTemplate]`, kita terpaksa harus merusak harmonisasi
kelas tipe, atau tinggal menggunakan kelas tipe lain.

Untuk menyelesaikan masalah yang muncul pada penerapan beberapa kelas tipe pada
satu kelas, `scalaz.Tag` bisa digunakan tanpa merusak koherensi dari kelas tipe
yang sudah ada.

Pendefinisian metoda `Tag` memang agak rancu. Namun, sintaks yang  digunakan
sangat jelas. Beginilah cara kita untuk mengelabuhi kompilator agar kita bisa mendefinisikan
tipe infiks `A && T` yang menghapus penanda tipe menjadi `A` pada saat waktu jalan:

{lang="text"}
~~~~~~~~
  type @@[A, T] = Tag.k.@@[A, T]
  
  object Tag {
    @inline val k: TagKind = IdTagKind
    @inline def apply[A, T](a: A): A @@ T = k(a)
    ...
  
    final class TagOf[T] private[Tag]() { ... }
    def of[T]: TagOf[T] = new TagOf[T]
  }
  sealed abstract class TagKind {
    type @@[A, T]
    def apply[A, T](a: A): A @@ T
    ...
  }
  private[scalaz] object IdTagKind extends TagKind {
    type @@[A, T] = A
    @inline override def apply[A, T](a: A): A = a
    ...
  }
~~~~~~~~

Beberapa label yang bermanfaat yang disediakan pada objek `Tags`

{lang="text"}
~~~~~~~~
  object Tags {
    sealed trait First
    val First = Tag.of[First]
  
    sealed trait Last
    val Last = Tag.of[Last]
  
    sealed trait Multiplication
    val Multiplication = Tag.of[Multiplication]
  
    sealed trait Disjunction
    val Disjunction = Tag.of[Disjunction]
  
    sealed trait Conjunction
    val Conjunction = Tag.of[Conjunction]
  
    ...
  }
~~~~~~~~

`First` / `Last` digunakan untuk memilih instans `Monoid` dengan mengambil
oeran bukan-nol pertama / terakhir yang ditemui. `Multiplication`, tentu,
digunakan untuk perkalian numerik, bukan penambahan. `Disjunction` / `Conjunction`
digunakan untuk memilih `&&` atau `||`.

Pada `TradeTemplate`, jauh lebih disukai untuk menggunakan `Option[Currency] @@ Tags.Last`
bila dibandingkan hanya menggunakan `Option[Currency]` saja. Karena hal semacam ini
sangat jamak dijumpai, maka kita bisa menggunakan alias bawaan, `LastOption`

{lang="text"}
~~~~~~~~
  type LastOption[A] = Option[A] @@ Tags.Last
~~~~~~~~

yang memperkenankan kita untuk menulis `Monoid[TradeTemplate]` menjadi lebih jelas

{lang="text"}
~~~~~~~~
  final case class TradeTemplate(
    payments: List[java.time.LocalDate],
    ccy: LastOption[Currency],
    otc: LastOption[Boolean]
  )
  object TradeTemplate {
    implicit val monoid: Monoid[TradeTemplate] = Monoid.instance(
      (a, b) =>
        TradeTemplate(a.payments |+| b.payments,
                      a.ccy |+| b.ccy,
                      a.otc |+| b.otc),
        TradeTemplate(Nil, Tag(None), Tag(None))
    )
  }
~~~~~~~~

Sedangkan bila kita harus membuat sebuah nilai mentah untuk tipe `LastOption`,
kita bisa menggunakan `Tag` pada sebuah `Option`. Kita akan menyebut hal ini
sebagai `Tag(None)`.

Pada bab mengenai derivasi kelas tipe, kita akan melangkah lebih lanjut dengan
melakukan derivasi otomatis atas `monoid`.

Tentu sangat menggiurkan untuk menggunakan `Tag` agar tipe data pada
validasi borang (mis, `String @@ PersonName`), namun hal ini harus dihindari
karena tidak ada pemeriksaan konten pada saat waktu jalan. `Tag` seharusnya
hanya boleh digunakan untuk pemilihan kelas tipe saja. Pembaca budiman
dianjurkan untuk menggunakan pustaka `Refined` yang diperkenalkan pada bab
4 untuk membatasi nilai.

## Transformasi Natural

Pada Scala, penulisan sebuah fungsi yang memetakan sebuah tipe ke tipe lainnya
biasa dituliskan sebagai `A => B`. Penulisan tersebut sendiri merupakan
pemanis sintaksis untuk `Function[A, B]`. Sedangkan untuk memetakan konstruktor
tipe `F[_]` ke `G[_]`. Scalaz menyediakan pemanis sintaks yeang mirip dengan
`A => B` yaitu `F ~> G`.

`F ~> G` disebut sebagai *transformasi natural* dan *secara umum terkuantifikasi*
karena sintaks ini tidak menghiraukan isi dari `F_]`.

{lang="text"}
~~~~~~~~
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  trait NaturalTransformation[-F[_], +G[_]] {
    def apply[A](fa: F[A]): G[A]
  
    def compose[E[_]](f: E ~> F): E ~> G = ...
    def andThen[H[_]](f: G ~> H): F ~> H = ...
  }
~~~~~~~~

Sebagai contoh transformasi natural, mari kita lihat sebuah fungsi yang
mengubah `IList` menjadi `List`

{lang="text"}
~~~~~~~~
  scala> val convert = new (IList ~> List) {
           def apply[A](fa: IList[A]): List[A] = fa.toList
         }
  
  scala> convert(IList(1, 2, 3))
  res: List[Int] = List(1, 2, 3)
~~~~~~~~

atau yang lebih ringkas,  dengan menggunakan pemanis `kind-projector`:

{lang="text"}
~~~~~~~~
  scala> val convert = λ[IList ~> List](_.toList)
  
  scala> val convert = Lambda[IList ~> List](_.toList)
~~~~~~~~

Namun pada tahap pengembangan sehari-hari, sangat mungkin kita menggunakan
transformasi natural untuk memetakan dari aljabar satu ke aljabar lainnya.
Sebagai contoh, pada `drone-dynamic-agents`, kita mungkin lebih memilih
untuk mengimplementasikannya dengan menggunakan aljabar yang sudah ada,
`BigMachines`. Setelah mengetahui adanya transformasi ini, kita mungkin
akan memilih untuk melakukan transformasi dengan menggunakan `Machine ~> BigMachines`
daripada secara manual menulis ulang logika bisnis dan test kita menggunankan
`BigMachine`. Kita akan kembali membahas gagasan ini pada bab mengenai
Monad Lanjutan.


## `Isomorphism`

Seringkali kita mendapati dua tipe yang benar-benar sama dan mengakibatkan
masalah kompatibilitas yang dikarenakan kompilator tidak mengetauhi asumsi-
asumsi yang kita ketahui. Hal ini biasanya terjadi bila kita menggunakan
kode dari pihak ketiga yang sama dengan kode kita yang sudah ada.

This is when `Isomorphism` can help us out. An isomorphism defines a formal "is
equivalent to" relationship between two types. There are three variants, to
account for types of different shapes:

Masalah seperti ini bisa diselesaikan dengan `Isomorphism`. Sebuah isomorfisme
mendefinisikan secara formal hubungan setara antara dua tipe. Isomorphism
mempunyai tiga varian berdasarkan perbedaan bentuk dari tipe:

{lang="text"}
~~~~~~~~
  object Isomorphism {
    trait Iso[Arr[_, _], A, B] {
      def to: Arr[A, B]
      def from: Arr[B, A]
    }
    type IsoSet[A, B] = Iso[Function1, A, B]
    type <=>[A, B] = IsoSet[A, B]
    object IsoSet {
      def apply[A, B](to: A => B, from: B => A): A <=> B = ...
    }
  
    trait Iso2[Arr[_[_], _[_]], F[_], G[_]] {
      def to: Arr[F, G]
      def from: Arr[G, F]
    }
    type IsoFunctor[F[_], G[_]] = Iso2[NaturalTransformation, F, G]
    type <~>[F[_], G[_]] = IsoFunctor[F, G]
    object IsoFunctor {
      def apply[F[_], G[_]](to: F ~> G, from: G ~> F): F <~> G = ...
    }
  
    trait Iso3[Arr[_[_, _], _[_, _]], F[_, _], G[_, _]] {
      def to: Arr[F, G]
      def from: Arr[G, F]
    }
    type IsoBifunctor[F[_, _], G[_, _]] = Iso3[~~>, F, G]
    type <~~>[F[_, _], G[_, _]] = IsoBifunctor[F, G]
  
    ...
  }
~~~~~~~~

Tipe alias `IsoSet`, `IsoFunctor`, dan `IsoBifunctor` mencakup hal-hal
umum: fungsi reguler, transformasi atural, dan  transformasi binatural.
Fungsi fungsi pembantu mempermudah kita dalam membuat instans dari fungsi
fungsi atau transformasi natural yang telah ada sebelumnya. Walaupun
kadangkala, akan lebih mudah dalam pendefinisian isomorfisme dengan
menggunakan abstrak `Template`. Sebagai contoh:

{lang="text"}
~~~~~~~~
  val listIListIso: List <~> IList =
    new IsoFunctorTemplate[List, IList] {
      def to[A](fa: List[A]) = fromList(fa)
      def from[A](fa: IList[A]) = fa.toList
    }
~~~~~~~~

Bila kita memperkenalkan sebuah isomorfisme, kita juga akan membuat
banyak instans kelas tipe standar. Sebagai contoh:

{lang="text"}
~~~~~~~~
  trait IsomorphismSemigroup[F, G] extends Semigroup[F] {
    implicit def G: Semigroup[G]
    def iso: F <=> G
    def append(f1: F, f2: =>F): F = iso.from(G.append(iso.to(f1), iso.to(f2)))
  }
~~~~~~~~

memperkenankan kita untuk menderivasi sebuah `Semigroup[F]` untuk tipe `F` bila
kita mempunyai sebuah `F <=> G` dan `Semigroup[G]`. Hampir semua kelas tipe
pada hierarki menyediakan varian isomorfik. Bila kita berada pada situasi
salin-tempel saat menulis implementasi kelas tipe, mungkin ada baiknya
mempertimbangkan `Isomorphism` sebagai solusi yng lebih baik.


## Kontainer


### Maybe

Sebagaimana yang telah kita saksikan, Scalaz menyediakan peningkatan atas
`scala.Option` dengan konstruk `Maybe`. `Maybe` dianggap sebagai peningkatan
dikarenakann konstruk ini merupakan sebuah invarian dan tidak mempunyai
metoda rawan seperti `Option.get`, yang bisa melempar pengecualian.

Secara umum, konstruk ini digunakan untuk merepresentasikan keadaan
dimana sebuah objek bisa ada maupun tidak, tnapa memberikan konteks
kenapa bisa begitu.

{lang="text"}
~~~~~~~~
  sealed abstract class Maybe[A] { ... }
  object Maybe {
    final case class Empty[A]()    extends Maybe[A]
    final case class Just[A](a: A) extends Maybe[A]
  
    def empty[A]: Maybe[A] = Empty()
    def just[A](a: A): Maybe[A] = Just(a)
  
    def fromOption[A](oa: Option[A]): Maybe[A] = ...
    def fromNullable[A](a: A): Maybe[A] = if (null == a) empty else just(a)
    ...
  }
~~~~~~~~

Metoda pasangan `.empty` dan `just` lebih disukai saat membuat instans
`Empty` dan `Just` mentah karena kedua metoda tersebut mengembalikan
sebuha `Maybe` dan membantu mempermudah pendugaan tipe. Pola ini seringkali
digunakan karena mengembalikan a *sum type* (lol, help). *Sum type* sendiri
merupakan keadaan dimana kita mempunyai beberapa implementasi sebuah
`sealed trait` namun tidak menggunakan sub-tipe khusus pada sebuah penanda tipe.

Kita juga bisa tinggal memanggil `.just` pada semua nilai dan mendapatkan sebuah
`Maybe`. Hal ini dikarenakan kelas pembantu `implicit class`

{lang="text"}
~~~~~~~~
  implicit class MaybeOps[A](self: A) {
    def just: Maybe[A] = Maybe.just(self)
  }
~~~~~~~~

`Maybe` mempunyai instans kelas tipe untuk

-   `Align`
-   `Traverse`
-   `MonadPlus` / `IsEmpty`
-   `Cobind`
-   `Cozip` / `Zip` / `Unzip`
-   `Optional`

dan mendelegasi instans yang bergantung pada `A`

-   `Monoid` / `Band`
-   `Equal` / `Order` / `Show`

Sebagai tambahan untuk kelas tipe di atas, `Maybe` juga mempunyai
beberapa fungsionalitas yang tidak didukung oleh kelas tipe polimorfis.


{lang="text"}
~~~~~~~~
  sealed abstract class Maybe[A] {
    def cata[B](f: A => B, b: =>B): B = this match {
      case Just(a) => f(a)
      case Empty() => b
    }
  
    def |(a: =>A): A = cata(identity, a)
    def toLeft[B](b: =>B): A \/ B = cata(\/.left, \/-(b))
    def toRight[B](b: =>B): B \/ A = cata(\/.right, -\/(b))
    def <\/[B](b: =>B): A \/ B = toLeft(b)
    def \/>[B](b: =>B): B \/ A = toRight(b)
  
    def orZero(implicit A: Monoid[A]): A = getOrElse(A.zero)
    def orEmpty[F[_]: Applicative: PlusEmpty]: F[A] =
      cata(Applicative[F].point(_), PlusEmpty[F].empty)
    ...
  }
~~~~~~~~

`.cata` merupakan bentuk singkat dari `.map(f).getOrElse(b)` dan bahkan
mempunyai bentuk yang lebih sederhana dalam bentuk `|` bila map berupa
sebuah `identity` (mis, hanya `.getOrElse`).

`.toLeft` dan `toRight`, dan alias simbolis mereka, membuat sebuah disjungsi
(yang akan dijelaskan pada bagian selanjutnya) dengan menerima sebuah
penadah untuk kasus `Empty`.

`.orZero` menerima sebuah `Monoid` untuk mendefinisikan nilai bawaan.

`orEmpty` menggunakan `ApplicativePlus` untuk membuat sebuah elemen atau
kontainer kosong, tanpa melupakan bahwa kita sudah mempunyai dukungan
untuk pustaka koleksi standar dari metoda `.to` dari instans `Foldable`.

{lang="text"}
~~~~~~~~
  scala> 1.just.orZero
  res: Int = 1
  
  scala> Maybe.empty[Int].orZero
  res: Int = 0
  
  scala> Maybe.empty[Int].orEmpty[IList]
  res: IList[Int] = []
  
  scala> 1.just.orEmpty[IList]
  res: IList[Int] = [1]
  
  scala> 1.just.to[List] // from Foldable
  res: List[Int] = List(1)
~~~~~~~~

A> Metoda pada `Maybe` didefinisikan dengan menggunakan gaya OOP, walaupun
A> berlawanan dengan apa yang telah dipelajari pada bab 4 untuk menggunakan
A> `object` atau `implicit class`. Hal ini merupakan hal yang biasa pada
A> Scalaz dan mempunyai alasan historis:
A> 
A> -   penyunting teks gagal menemukan metoda ekstensi, walaupun saat ini
A>     IntelliJ, ENSIME, dan ScalaIDE mendukung penuh.
A> -   ada beberapa kasus sudut (lol) dimana kompilator gagal menerka
A>     tipe dan tidak mampu menemunkan metoda ekstensi.
A> -   pustaka standar mendefinisikan beberapa instans dari `implicit class`
A>     yang menambah metoda ke semua nilai, termasuk dengan metoda dengan
A>     nama yang bertabrakan. Contoh paling nyata adalah metoda `+`.
A>     Metoda ini mengubah semua menjadi `String` yang disambung.
A> 
A> Hal yang sama juga berlaku untuk fungsionalitas yang disediakan oleh
A> instans kelas tipe, seperti metoda berikut yang bila tidak ada, akan
A> disediakan oleh `Optional`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   sealed abstract class Maybe[A] {
A>     def getOrElse(a: =>A): A = ...
A>     ...
A>   }
A> ~~~~~~~~
A>
A> Namun, versi Scala baru-baru ini sudah menangani banyak kutu dan kita
A> mungkin semakin jaran menemui masalah semacam ini.


### Either

Untuk perbaikan yang diberikan oleh Scalaz terhadap `scala.Either`,
walaupun hanya dalam bentuk simbol operator, adalah hal yang jamak untuk
menyebut operator tersebut sebagai *antara* (*either*)atau `Disjunction`. 

{lang="text"}
~~~~~~~~
  sealed abstract class \/[+A, +B] { ... }
  final case class -\/[+A](a: A) extends (A \/ Nothing)
  final case class \/-[+B](b: B) extends (Nothing \/ B)
  
  type Disjunction[+A, +B] = \/[A, B]
  
  object \/ {
    def left [A, B]: A => A \/ B = -\/(_)
    def right[A, B]: B => A \/ B = \/-(_)
  
    def fromEither[A, B](e: Either[A, B]): A \/ B = ...
    ...
  }
~~~~~~~~

dengan sintaks

{lang="text"}
~~~~~~~~
  implicit class EitherOps[A](val self: A) {
    final def left [B]: (A \/ B) = -\/(self)
    final def right[B]: (B \/ A) = \/-(self)
  }
~~~~~~~~

Harap diperhatikan, metoda ekstensi di atas menerima tipe untuk
*sisi yang berseberangan*. Jadi, bila kita ingin membuat sebuah
`String \/ Int` dan kita mempunyai sebuah `Int`, kita harus menyerahkan
`String` saat memanggil `.right`


{lang="text"}
~~~~~~~~
  scala> 1.right[String]
  res: String \/ Int = \/-(1)
  
  scala> "hello".left[Int]
  res: String \/ Int = -\/(hello)
~~~~~~~~

Sifat simbolis dari `\/`-lah yang mempermudah pembacaan kontainer ini
pada penanda tipe. Harap diperhatikan bahwa tipe simbolis pada Scala
selalu diasosiasikan dari kiri. Ditambah lagi bila kita ingin menggunakan
`\/` berlapis, kita harus menggunakan tanda kurung. Sebagai contoh,
`(A \/ (B \/ (C \/ D)))`. 

`\/` mempunyai kecenderungan untuk memilih bagian kanan (mis, `flatMap`
juga berlaku pada `\/-`) untuk instans kelas tipe:

-   `Monad` / `MonadError`
-   `Traverse` / `Bitraverse`
-   `Plus`
-   `Optional`
-   `Cozip`

dan bergantung pada konten

-   `Equal` / `Order`
-   `Semigroup` / `Monoid` / `Band`

Sebagai tambahan, ada beberapa metoda khususs

{lang="text"}
~~~~~~~~
  sealed abstract class \/[+A, +B] { self =>
    def fold[X](l: A => X, r: B => X): X = self match {
      case -\/(a) => l(a)
      case \/-(b) => r(b)
    }
  
    def swap: (B \/ A) = self match {
      case -\/(a) => \/-(a)
      case \/-(b) => -\/(b)
    }
  
    def |[BB >: B](x: =>BB): BB = getOrElse(x) // Optional[_]
    def |||[C, BB >: B](x: =>C \/ BB): C \/ BB = orElse(x) // Optional[_]
  
    def +++[AA >: A: Semigroup, BB >: B: Semigroup](x: =>AA \/ BB): AA \/ BB = ...
  
    def toEither: Either[A, B] = ...
  
    final class SwitchingDisjunction[X](right: =>X) {
      def <<?:(left: =>X): X = ...
    }
    def :?>>[X](right: =>X) = new SwitchingDisjunction[X](right)
    ...
  }
~~~~~~~~

`.fold` mirip dengan `Maybe.cata` dan mengharuskan kedua sisi dipetakan
ke tipe yang sama.

`.swap` menukar sisi kiri ke kanan dan sebaliknya.

`|` yang merupakan alias dari `getOrElse` terlihat mirip dengan `Maybe`.
Kita juga bisa menggunakan `|||` sebagai alias untuk `orElse`.

`+++` merupakan penggabungan disjungsi dengan kecenderungan untuk memilih
bagian kiri:

-   `right(v1) +++ right(v2)` menghasilkan `right(v1 |+| v2)`
-   `right(v1) +++ left (v2)` menghasilkan `left (v2)`
-   `left (v1) +++ right(v2)` menghasilkan `left (v1)`
-   `left (v1) +++ left (v2)` menghasilkan `left (v1 |+| v2)`

`.toEither` disediakan untuk kompatibilitas terbalik dengan pustaka
standar Scala.

Untuk kombinasi dari `:?>>` dan `<<?:` memperkenankan kita untuk
menghiraukan isi dari sebuah `\/`, namun berdasarkan tipe dari isinya. 

{lang="text"}
~~~~~~~~
  scala> 1 <<?: foo :?>> 2
  res: Int = 2 // foo is a \/-
  
  scala> 1 <<?: foo.swap :?>> 2
  res: Int = 1
~~~~~~~~


### Validation

Secara sekilas, `Validation` yang mempunyai alias dengan `\?/,
terlihat seperti salinan dari `Disjunction`:

{lang="text"}
~~~~~~~~
  sealed abstract class Validation[+E, +A] { ... }
  final case class Success[A](a: A) extends Validation[Nothing, A]
  final case class Failure[E](e: E) extends Validation[E, Nothing]
  
  type ValidationNel[E, +X] = Validation[NonEmptyList[E], X]
  
  object Validation {
    type \?/[+E, +A] = Validation[E, A]
  
    def success[E, A]: A => Validation[E, A] = Success(_)
    def failure[E, A]: E => Validation[E, A] = Failure(_)
    def failureNel[E, A](e: E): ValidationNel[E, A] = Failure(NonEmptyList(e))
  
    def lift[E, A](a: A)(f: A => Boolean, fail: E): Validation[E, A] = ...
    def liftNel[E, A](a: A)(f: A => Boolean, fail: E): ValidationNel[E, A] = ...
    def fromEither[E, A](e: Either[E, A]): Validation[E, A] = ...
    ...
  }
~~~~~~~~

Dengan sintaks

{lang="text"}
~~~~~~~~
  implicit class ValidationOps[A](self: A) {
    def success[X]: Validation[X, A] = Validation.success[X, A](self)
    def successNel[X]: ValidationNel[X, A] = success
    def failure[X]: Validation[A, X] = Validation.failure[A, X](self)
    def failureNel[X]: ValidationNel[A, X] = Validation.failureNel[A, X](self)
  }
~~~~~~~~

Namun, struktur data tersebut tidak mewakili cerita yang melatar-
belakanginya. `Validation` memang dimaksudkan untuk tidak memiliki
instans dari `Monad` dan membatasi dirinya berdasarkan versi
yang diharapkan dari:

-   `Applicative`
-   `Traverse` / `Bitraverse`
-   `Cozip`
-   `Plus`
-   `Optional`

dan berdasarkan konten

-   `Equal` / `Order`
-   `Show`
-   `Semigroup` / `Monoid`

Keuntungan utama atas pembatasann yang hanya sampai pada `Applicative`
adalah pada saat kita membutuhkan semua galat dilaporkan, `Validation`
akan menerima semua galat tersebut. Berbeda dengan `Disjunction`  yang
berhenti dieksekusi pada saat galat pertama terjadi. Untuk mengakomodasi
akumulusai galat, bentuk paling umum yang ditemui dari `Validation` adalah
`ValidationNel` yang mempunyai `NonEmptyList[E]` pada posisi galat.

Misalkan saat pembaca yang budiman sedang melakukan validasi terhadap
data yang diberikan oleh pengguna menggunakan `Disjunction` dan `flatMap`:

{lang="text"}
~~~~~~~~
  scala> :paste
         final case class Credentials(user: Username, name: Fullname)
         final case class Username(value: String) extends AnyVal
         final case class Fullname(value: String) extends AnyVal
  
         def username(in: String): String \/ Username =
           if (in.isEmpty) "empty username".left
           else if (in.contains(" ")) "username contains spaces".left
           else Username(in).right
  
         def realname(in: String): String \/ Fullname =
           if (in.isEmpty) "empty real name".left
           else Fullname(in).right
  
  scala> for {
           u <- username("sam halliday")
           r <- realname("")
         } yield Credentials(u, r)
  res = -\/(username contains spaces)
~~~~~~~~

Bila kita menggunakan `|@|`

{lang="text"}
~~~~~~~~
  scala> (username("sam halliday") |@| realname("")) (Credentials.apply)
  res = -\/(username contains spaces)
~~~~~~~~

Kita akan tetap mendapat galat pertama saja. Hal ini disebabkan oleh
kelas tipe dari `Disjunction` yang juga mempunyai instans `Monad`.
Metoda `.applyX` harus konsisten dengan `.flatMap` dan tidak mengasumsikan
bahwa semua operasi bisa dijalankan secara bebas. Bandingkan dengan:

{lang="text"}
~~~~~~~~
  scala> :paste
         def username(in: String): ValidationNel[String, Username] =
           if (in.isEmpty) "empty username".failureNel
           else if (in.contains(" ")) "username contains spaces".failureNel
           else Username(in).success
  
         def realname(in: String): ValidationNel[String, Fullname] =
           if (in.isEmpty) "empty real name".failureNel
           else Fullname(in).success
  
  scala> (username("sam halliday") |@| realname("")) (Credentials.apply)
  res = Failure(NonEmpty[username contains spaces,empty real name])
~~~~~~~~

Sekarang, kita bakal mendapat semua galat yang terjadi. 

`Validation` punya beberapa metoda yang mirip dengan yang dipunyai
oleh `Disjunction` seperti, `.fold`, `.swap`, `+++`, dan beberapa tambahan:

{lang="text"}
~~~~~~~~
  sealed abstract class Validation[+E, +A] {
    def append[F >: E: Semigroup, B >: A: Semigroup](x: F \?/ B]): F \?/ B = ...
  
    def disjunction: (E \/ A) = ...
    ...
  }
~~~~~~~~

`.append` (dengan alias `+|+`) mempunyai penanda tipe yang sama dengan `+++`
namun lebih memilih hasil yang `success`

-   `failure(v1) +|+ failure(v2)` menghasilkan `failure(v1 |+| v2)`
-   `failure(v1) +|+ success(v2)` menghasilkan `success(v2)`
-   `success(v1) +|+ failure(v2)` menghasilkan `success(v1)`
-   `success(v1) +|+ success(v2)` menghasilkan `success(v1 |+| v2)`

`.disjunction` mengubah sebuah `Validated[A, B]` menjadi `A \/ B`.
`Disjunction` mencerminkan `.validation` dan `.validationNel` dan
mengubahnya menjadi `Validation`. Sehingga hal ini mempermudah
konversi dari akumulasi galat berurutan dan paralel.

`\/` dan `Validation` merupakan solusi dari pemrograman fungsional yang
setara dengan pemeriksaan pengecualian untuk validasi input.
Selain itu, performa yang ditawarkan lebih tinggi dikarenakan `Validation`
tidak menggunakan *stacktrace* dan tanpa memaksa metoda pemanggil untuk
berurusan dengan galat. Hal semacam ini menghasilkan sistem yang lebih
kokoh. 

A> Salah satu hal yang paling pelan pada JVM adalah pembuatan sebuah pengecualian.
A> Penyebab dari kelambanan ini adalah alokasi sumber daya yang dibutuhkan untuk
A> membuat *stacktrace*. Yang jamak dilakukan saat melakukan validasi input dan
A> penguraian adalah penggunaan eksepsi, walau eksepsi tersebut memakan waktu
A> yang jauh lebih banyak bila dibandingkan dengan menggunakan `\/` atau `Validation`.
A> 
A> Beberapa pihak meng-klaim bahwa eksepsi terduga untuk validasi input
A> adalah transparan secara rujukan. Alasan yang pihak tersebut gunakan adalah
A> eksepsi tersebut pasti akan muncul bila situasi sesuai dengan yang kita
A> definisikan. Namun, *stacktrace* yang ada pada pengecualian bergantung pada
A> rantai panggilan metoda. Hal ini menyebabkan nilai *stacktrace* akan berbeda
A> tergantung pada siapa yang memanggil eksepsi tersebut. Dan pada akhirnya,
A> eksepsi mengaburkan transparansi rujukan.
A> Bagaimanapun juga, melempar sebuah eksepsi dianggap tidak murni karena
A> fungsi tersebut tidak *Total*.


### These

Seperti yang telah kita temui pada bab sebelumnya mengenai `Align`,
`These` berbicara mengenai penyandian data dengan logika inklusif atau
yang disebut juga `OR`.

{lang="text"}
~~~~~~~~
  sealed abstract class \&/[+A, +B] { ... }
  object \&/ {
    type These[A, B] = A \&/ B
  
    final case class This[A](aa: A) extends (A \&/ Nothing)
    final case class That[B](bb: B) extends (Nothing \&/ B)
    final case class Both[A, B](aa: A, bb: B) extends (A \&/ B)
  
    def apply[A, B](a: A, b: B): These[A, B] = Both(a, b)
  }
~~~~~~~~

dengan sintaks konstruktor

{lang="text"}
~~~~~~~~
  implicit class TheseOps[A](self: A) {
    final def wrapThis[B]: A \&/ B = \&/.This(self)
    final def wrapThat[B]: B \&/ A = \&/.That(self)
  }
  implicit class ThesePairOps[A, B](self: (A, B)) {
    final def both: A \&/ B = \&/.Both(self._1, self._2)
  }
~~~~~~~~

`These` mempunyai instans kelas tipe untuk

-   `Monad`
-   `Bitraverse`
-   `Traverse`
-   `Cobind`

dan bergantung dengan konten

-   `Semigroup` / `Monoid` / `Band`
-   `Equal` / `Order`
-   `Show`

`These` (`\&/`) mempunyai banyak metoda yang setara dengan metoda
dari `Disjunction` (`\/`) dan `Validation` (`\?/`)

{lang="text"}
~~~~~~~~
  sealed abstract class \&/[+A, +B] {
    def fold[X](s: A => X, t: B => X, q: (A, B) => X): X = ...
    def swap: (B \&/ A) = ...
  
    def append[X >: A: Semigroup, Y >: B: Semigroup](o: =>(X \&/ Y)): X \&/ Y = ...
  
    def &&&[X >: A: Semigroup, C](t: X \&/ C): X \&/ (B, C) = ...
    ...
  }
~~~~~~~~

`.append` mempunyai 9 cara penyusunan yang mungkin dibuat dan data tidak
pernah dibuang dikarenakan `This` dan `That` selalu bisa dikonversi menjadi
`Both`.  

`.flatMap` merupakan metoda yang cenderung memilih parameter sebelah kanan.
`.flatMap` menerima sebuah `Semigroup` pada konten bagian kiri (`This`)
untuk digabungkan, bukan meng-arus-pendekkannya. `&&&` dapat digunakan
untuk menggabungkan dua `These` dan membuat sebuah tuple di bagian kanan
dan memmbuang data yang bersangkutan bila data tersebut tidak ada pada
kedua sisi `These`.

Walaupun merupakan hal yang menggiurkan untuk menggunakan `\&/` pada
tipe kembalian, penggunaan berlebihan merupakan salah satu *anti-pattern*.
Alasan utama untuk menggunakan `\&/` adalah untuk menggabungkan atau
memecah aliran data yang bisa jadi tak hingga pada memori yang hingga.
Fungsi pembantu ada pada objek pendamping bila dibutuhkan bila berurusan
dengan `EphemeralStream` atau apapun dengan sebuah `MonadPlus`.

{lang="text"}
~~~~~~~~
  type EStream[A] = EphemeralStream[A]
  
  object \&/ {
    def concatThisStream[A, B](x: EStream[A \&/ B]): EStream[A] = ...
    def concatThis[F[_]: MonadPlus, A, B](x: F[A \&/ B]): F[A] = ...
  
    def concatThatStream[A, B](x: EStream[A \&/ B]): EStream[B] = ...
    def concatThat[F[_]: MonadPlus, A, B](x: F[A \&/ B]): F[B] = ...
  
    def unalignStream[A, B](x: EStream[A \&/ B]): (EStream[A], EStream[B]) = ...
    def unalign[F[_]: MonadPlus, A, B](x: F[A \&/ B]): (F[A], F[B]) = ...
  
    def merge[A: Semigroup](t: A \&/ A): A = ...
    ...
  }
~~~~~~~~


### Higher Kinded Either

Tipe data `Coproduct` (berbeda dengan konsep umum ko-produk pada sebuah ADT)
membungkus `Disjunction` untuk konstruktor tipe:

{lang="text"}
~~~~~~~~
  final case class Coproduct[F[_], G[_], A](run: F[A] \/ G[A]) { ... }
  object Coproduct {
    def leftc[F[_], G[_], A](x: F[A]): Coproduct[F, G, A] = Coproduct(-\/(x))
    def rightc[F[_], G[_], A](x: G[A]): Coproduct[F, G, A] = Coproduct(\/-(x))
    ...
  }
~~~~~~~~

Instans kelas tipe diserahkan ke fungtor `F[_]` dan `G[_]`. 

Penggunaan `Coproduct` yang paling jamak dijumpai adalah saat kita ingin
membuat sebuah ko-produk anonimus untuk sebuah GADT.


### Jangan Terburu-Buru

Tipe data tuple bawaan dari pustaka standar Scala dan tipe data sederhana
seperti `Maybe` dan `Disjunction` merupakan tipe dengan nilai yang selalu
dievaluasi secara tegas.

Untuk memudahkan pemakaian, alternatif *by-name* untuk `Name` juga disediakan
beserta beberapa instans kelas tipe:

{lang="text"}
~~~~~~~~
  sealed abstract class LazyTuple2[A, B] {
    def _1: A
    def _2: B
  }
  ...
  sealed abstract class LazyTuple4[A, B, C, D] {
    def _1: A
    def _2: B
    def _3: C
    def _4: D
  }
  
  sealed abstract class LazyOption[+A] { ... }
  private final case class LazySome[A](a: () => A) extends LazyOption[A]
  private case object LazyNone extends LazyOption[Nothing]
  
  sealed abstract class LazyEither[+A, +B] { ... }
  private case class LazyLeft[A, B](a: () => A) extends LazyEither[A, B]
  private case class LazyRight[A, B](b: () => B) extends LazyEither[A, B]
~~~~~~~~

Pembaca yang teliti akan memperhatikan bahwa `Lazy*` merupakan salah kaprah
dan tipe data ini seharusnya `ByNameTupleX`, `ByNameOption`, and `ByNameEither`.


### Const

`Const`, untuk konstan, merupakan pelapis untuk nilai dari tipe `A`, beserta
sebuah tipe parameter cadangan `B`.

{lang="text"}
~~~~~~~~
  final case class Const[A, B](getConst: A)
~~~~~~~~

`Const` menyediakan sebuah instans dari `Applicative[Const[A, ?]]` bila
`Monoid[A]` tersedia:

{lang="text"}
~~~~~~~~
  implicit def applicative[A: Monoid]: Applicative[Const[A, ?]] =
    new Applicative[Const[A, ?]] {
      def point[B](b: =>B): Const[A, B] =
        Const(Monoid[A].zero)
      def ap[B, C](fa: =>Const[A, B])(fbc: =>Const[A, B => C]): Const[A, C] =
        Const(fbc.getConst |+| fa.getConst)
    }
~~~~~~~~

Yang menjadi esensi dari `Applicative` ini adalah `Applicative` tersebut
menghiraukan parameter `B` dan melanjutkan eksekusi dengan lancar dan
hanya mengkombinasikan nilai konstan yang ditemu.

Kembali ke contoh aplikasi `drone-dynamic-agents`, kita harus me-*refaktor*
berkas `logic.scala` terlebih dahulu agar menggunakan `Applicative`.
Sebelumnya, kita menggunakan `Monad` karena kita tidak tahu ada alternatif
yang lebih sesuai untuk konteks permasalahan ini.

{lang="text"}
~~~~~~~~
  final class DynAgentsModule[F[_]: Applicative](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {
    ...
    def act(world: WorldView): F[WorldView] = world match {
      case NeedsAgent(node) =>
        M.start(node) >| world.copy(pending = Map(node -> world.time))
  
      case Stale(nodes) =>
        nodes.traverse { node =>
          M.stop(node) >| node
        }.map { stopped =>
          val updates = stopped.strengthR(world.time).toList.toMap
          world.copy(pending = world.pending ++ updates)
        }
  
      case _ => world.pure[F]
    }
    ...
  }
~~~~~~~~

Karena logika bisnis kita hanya membutuhkan sebuah `Applicative`, kita bisa
menulis tiruan implementasi `F[a]` dengan `Const[String, a]`. Pada setiap
kasus, kita mengembalikan nama dari fungsi yang dipanggil.

{lang="text"}
~~~~~~~~
  object ConstImpl {
    type F[a] = Const[String, a]
  
    private val D = new Drone[F] {
      def getBacklog: F[Int] = Const("backlog")
      def getAgents: F[Int]  = Const("agents")
    }
  
    private val M = new Machines[F] {
      def getAlive: F[Map[MachineNode, Epoch]]     = Const("alive")
      def getManaged: F[NonEmptyList[MachineNode]] = Const("managed")
      def getTime: F[Epoch]                        = Const("time")
      def start(node: MachineNode): F[Unit]        = Const("start")
      def stop(node: MachineNode): F[Unit]         = Const("stop")
    }
  
    val program = new DynAgentsModule[F](D, M)
  }
~~~~~~~~

Dengan interpretasi program kita semacam ini, kita dapat memastikan pada
metoda-metoda yang ada bahwa ada (lol, wat?)

{lang="text"}
~~~~~~~~
  it should "call the expected methods" in {
    import ConstImpl._
  
    val alive    = Map(node1 -> time1, node2 -> time1)
    val world    = WorldView(1, 1, managed, alive, Map.empty, time4)
  
    program.act(world).getConst shouldBe "stopstop"
  }
~~~~~~~~

Bisa juga kita menghitung jumlah pemanggilan metoda secara keseluruhan
dengan menggunakan `Const[Int, ?]` atau `IMap[String, Int]`. 

Dengan tes semacam ini, kita sudah jauh melampau tes tiruan tradisional
dengan menggunakan test `Const` yang memeriksa apa yang dites tanpa harus
menyediakan implementasi. Hal semacam ini berguna bila spesifikasi kita
mengharuskan untuk menerima input untuk panggilan-panggilan tertentu.
Terlebih lagi, kita mencapai hasil ini dengan keamanan waktu kompilasi.

Melanjutkan penggunaan pola pikir semacam ini sedikit lebih jauh, misal
kita ingin memonitor node yang kita hentikan pada `act`. Kita bisa
membuat implementasi `Drone` dan `Machines` dengan `Const` dan memanggilnya
dari metoda `act`

{lang="text"}
~~~~~~~~
  final class Monitored[U[_]: Functor](program: DynAgents[U]) {
    type F[a] = Const[Set[MachineNode], a]
    private val D = new Drone[F] {
      def getBacklog: F[Int] = Const(Set.empty)
      def getAgents: F[Int]  = Const(Set.empty)
    }
    private val M = new Machines[F] {
      def getAlive: F[Map[MachineNode, Epoch]]     = Const(Set.empty)
      def getManaged: F[NonEmptyList[MachineNode]] = Const(Set.empty)
      def getTime: F[Epoch]                        = Const(Set.empty)
      def start(node: MachineNode): F[Unit]        = Const(Set.empty)
      def stop(node: MachineNode): F[Unit]         = Const(Set(node))
    }
    val monitor = new DynAgentsModule[F](D, M)
  
    def act(world: WorldView): U[(WorldView, Set[MachineNode])] = {
      val stopped = monitor.act(world).getConst
      program.act(world).strengthR(stopped)
    }
  }
~~~~~~~~

Kita bisa melakukan hal semacam ini karena `monitor` merupakan metoda
murni yang berjalan tanpa menghasilkan efek samping.

Potongan kode ini menjalankan program dengan `ConstImpl` yang dilanjutkan
dengan mengekstrak semua pemanggilan ke `Machines.stop` dan pada akhirnya
mengembalikan bersama `WorldView`. Kita bisa mengetesnya dengan:

{lang="text"}
~~~~~~~~
  it should "monitor stopped nodes" in {
    val underlying = new Mutable(needsAgents).program
  
    val alive = Map(node1 -> time1, node2 -> time1)
    val world = WorldView(1, 1, managed, alive, Map.empty, time4)
    val expected = world.copy(pending = Map(node1 -> time4, node2 -> time4))
  
    val monitored = new Monitored(underlying)
    monitored.act(world) shouldBe (expected -> Set(node1, node2))
  }
~~~~~~~~

Kita sudah menggunakan `Const` untuk melakukan apa yang terlihat seperti
Pemrograman Berorientasi Aspek, yang dulu pernah populer di Java. Kita
membangun logika bisnis kita untuk mendukung pemantauan tanpa harus
mengaburkan logika bisnis.

Dan menariknya, kita dapat menjalankan `ConstImpl` pada lingkungan produksi
untuk mengumpulkan apa yang ingin kita `stop` dan menyediakan implementasi
teroptimis dari `act` yang bisa menggunakan kelompok panggilan implementasi
khusus.

Namun, pahlawan tanpa tanda jasa dari cerita ini adalah `Applicative`.
`Const` memperkenankan kita untuk menunjukkan apa yang bisa kita lakukan.
Bila kita harus mengubah program kita untuk meminta sebuah `Monad`, kita
tidak dapat lagi menggunakan `Const` dan harus menulis ulang tiruan secara
menyeluruh untuk dapat memastikan apa yang dipanggil pada input tertentu.
*Rule of Least Power* memaksa kita untuk memilih menggunakan `Applicative`
dibandingkan `Monad` bila memungkinkan.


## Koleksi

Berbeda halnya dengan APA Koleksi dari pustaka standar, pendekatan Scalaz
atas perilaku koleksi dideskripsikan dengan hierarki kelas tipe, misalkan
`Foldable`, `Traverse`, `Monoid`. Yang tersisa untuk dipelajari adalah
implementasi strukutur data yang mempunyai karakteristik performa yang
cukup berbeda dan metoda relung.

This section goes into the implementation details for each data type. It is not
essential to remember everything presented here: the goal is to gain a high
level understanding of how each data structure works.

Because all the collection data types provide more or less the same list of
typeclass instances, we shall avoid repeating the list, which is often some
variation of:

Karena semua tipe data koleksi menyediakan instans kelas tipe yang kurang lebih
sama, kita akan melewatkan daftar instans tersebut, yang biasanya terdiri
atas variasi dari:

-   `Monoid`
-   `Traverse` / `Foldable`
-   `MonadPlus` / `IsEmpty`
-   `Cobind` / `Comonad`
-   `Zip` / `Unzip`
-   `Align`
-   `Equal` / `Order`
-   `Show`

Struktur data yang sudah terbukti tidak kosong akan menyediakan

-   `Traverse1` / `Foldable1`

Dan menyediakan `Semigroup`, bukan `Monooid`, dan `Plus`, bukan `IsEmpty`.


### Senarai

Kita sudah menggunakan `IList[A]` dan `NonEmptyList[A]` berulang kali
sehingga akan terasa familiar. Kedua struktur data tersebut mengkodifikasikan
struktur data klasik senarai berantai:

{lang="text"}
~~~~~~~~
  sealed abstract class IList[A] {
    def ::(a: A): IList[A] = ...
    def :::(as: IList[A]): IList[A] = ...
    def toList: List[A] = ...
    def toNel: Option[NonEmptyList[A]] = ...
    ...
  }
  final case class INil[A]() extends IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  
  final case class NonEmptyList[A](head: A, tail: IList[A]) {
    def <::(b: A): NonEmptyList[A] = nel(b, head :: tail)
    def <:::(bs: IList[A]): NonEmptyList[A] = ...
    ...
  }
~~~~~~~~

A> Pada kode sumber Scalaz 7.3, `INil` diimplementasikan dengan
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   sealed abstract case class INil[A] private() extends IList[A]
A>   object INil {
A>     private[this] val value: INil[Nothing] = new INil[Nothing]{}
A>     def apply[A](): IList[A] = value.asInstanceOf[IList[A]]
A>   }
A> ~~~~~~~~
A>
A> yang menggunakan detail implementasi JVM untuk menghindari alokasi objek pada
A> saat membuat sebuah `INil`.
A> 
A> Optimisasi semacam ini ditulis secara manual pada semua kelas tanpa parameter.
A> Dan memang, Scalaz dipenuhi dengan optimisasi semacam ini setelah didiskusikan
A> dan ditunjukkan bahwa terdapat peningkatan perfroma yang signifikan dan
A> tanpa memperkenalkan perubahan semantik.

Keuntungan utama dari `IList` atas `List` milik pustaka standar adalah
tidak adanya metoda yang tidak aman seperti `.head` yang melempar eksepsi
pada sebuah senarai kosong.

Sebagai tambahan, `IList` jauh lebih sederhana, tanpa hierarki dan mempunyai
jejak bytecode yang jauh lebih kecil. Terlebih lagi, `List` pustaka standar
mempunayi implementasi yang mengerikan dengan menggunakan `var` untuk
mengakali masalah performa pada desain koleksi pustaka standar:

{lang="text"}
~~~~~~~~
  package scala.collection.immutable
  
  sealed abstract class List[+A]
    extends AbstractSeq[A]
    with LinearSeq[A]
    with GenericTraversableTemplate[A, List]
    with LinearSeqOptimized[A, List[A]] { ... }
  case object Nil extends List[Nothing] { ... }
  final case class ::[B](
    override val head: B,
    private[scala] var tl: List[B]
  ) extends List[B] { ... }
~~~~~~~~

Pembuatan `List` membutuhkan sinkronisasi `Thread` yang hati hati dan pelan
untuk memastikan keamanan. `IList` tidak membutuhkan *hack* (lol, lupa)
semacam itu sehingga mempunyai performa yang lebih bagus bila dibandingkan
`List`.

A> Bukannya `NonEmptyList` sama halnya dengan `ICons`? Ya, pada tingkat struktur
A> data. Namun yang membedakan adalah `ICons` merupakan TDA dari `IList` sedangkan
A> `NonEmptyList` tidak. Instans kelas tipe harus selalu disediakan pada tingkat
A> ADT untuk menghindari kerumitan yang tidak perlu.


### `EphemeralStream`

Struktur data `Stream` dari pustaka standar merupakan bentuk lundung
dari `List`, namun implementasinya dipenuhi dengan kebocoran memori dan metoda
tak aman. Untuk menghilangkan masalah semacam ini, `EphemerealStream` tidak
menyimpan rujukan pada nilai yang telah dikomputasi. Selain itu, sama halnya
dengan apa yang dilakukan pada `IList`, `EphemerealStream` juga menghilangkan
penggunaan metoda-metoda tidak aman.

{lang="text"}
~~~~~~~~
  sealed abstract class EphemeralStream[A] {
    def headOption: Option[A]
    def tailOption: Option[EphemeralStream[A]]
    ...
  }
  // private implementations
  object EphemeralStream extends EphemeralStreamInstances {
    type EStream[A] = EphemeralStream[A]
  
    def emptyEphemeralStream[A]: EStream[A] = ...
    def cons[A](a: =>A, as: =>EStream[A]): EStream[A] = ...
    def unfold[A, B](start: =>B)(f: B => Option[(A, B)]): EStream[A] = ...
    def iterate[A](start: A)(f: A => A): EStream[A] = ...
  
    implicit class ConsWrap[A](e: =>EStream[A]) {
      def ##::(h: A): EStream[A] = cons(h, e)
    }
    object ##:: {
      def unapply[A](xs: EStream[A]): Option[(A, EStream[A])] =
        if (xs.isEmpty) None
        else Some((xs.head(), xs.tail()))
    }
    ...
  }
~~~~~~~~

A> Penggunaan kata *stream* pada struktur data semacam ini merupakan sisa-sisa
A> peninggalan masa lalu. *Stream* sekarang digunakan oleh bagian pemasaran
A> dan ✨ *Reactive Manifesto* ✨ beserta pembuatan *framework* seperti Akka Stream.

`.cons`, `.unfold`, dan `.iterate` digunakan untuk membuat *stream*. Sedangkan
untuk `##::`, operator ini digunakan untuk menambah elemen baru pada bagian
awal dari `Estream`. Untuk `.unfold`, seringkali digunakan untuk membuat
*stream* hingga (walaupun bisa saja merupakan *stream* tak hingga) dengan
mengaplikasikan fungsi `f` secara berulang untuk mendapatkan nilai selanjutnya
dan input untuk fungsi `f` itu sendiri. `.iterate` membuat sebuah *stream*
tak hingga dengan mengulang fungsi `f` pada elemen sebelumnya.

`Estream` bisa saja muncul pada *pattern match* dengan simobl `##::` dengan
mencocokkan sintaks untuk `.cons`.

A> `##::` mirip dengan Exogorth, seekor cacing raksasa yang tinggal di
A> asteroid.

Walau `Estream` menjawab masalah memori, struktur data ini bisa saja
tetap terkena masalah memori bila ada nilai yang masih dirujuk terletak
pada bagian ujung awal dari sebuah *stream* tak hingga. Masalah semacam ini,
sebagaimana halnya dengan kebutuhan untuk membangun *stream* dengan efek,
merupakan alasan dibuatnya fs2.


### `CorecursiveList`

Korekursi adalah saat kita memulai sesuatu dari sebuah kondisi awal dan
membuat langkah-langkah selanjutnya secara deterministik yang sama halnya
dengan `EphemerealStream.unfold` yang baru saja kita pelajari.

{lang="text"}
~~~~~~~~
  def unfold[A, B](b: =>B)(f: B => Option[(A, B)]): EStream[A] = ...
~~~~~~~~

Sangat berbeda dengan rekursi, yang memecah data menjadi kondisi dasar
lalu berakhir.

`CorecursiveList` merupakan penyandian data dari `EphemerealStream.unfold`
yang memberikan alternatif untuk `EStream` yang berpeluang untuk memberikan
performa yang lebih bagus dalam beberapa situasi tertentu:

{lang="text"}
~~~~~~~~
  sealed abstract class CorecursiveList[A] {
    type S
    def init: S
    def step: S => Maybe[(S, A)]
  }
  
  object CorecursiveList {
    private final case class CorecursiveListImpl[S0, A](
      init: S0,
      step: S0 => Maybe[(S0, A)]
    ) extends CorecursiveList[A] { type S = S0 }
  
    def apply[S, A](init: S)(step: S => Maybe[(S, A)]): CorecursiveList[A] =
      CorecursiveListImpl(init, step)
  
    ...
  }
~~~~~~~~

Korekursi berguna saat mengimplementasikan `Comonad.cojoin`, seperti contoh
pada `Hood`. `CorecursiveList` merupakan contoh untuk mengkodifikasi persamaan
non-linear berulang seperti yang digunakan pada pemodelan biologi populasi,
sistem kontrol, ekonomi makro, dan investasi perbankan.


### `ImmutableArray`

Sebuah pembungkus sederhana untuk struktur data `Array` dengan spesialisasi
primitif: 

{lang="text"}
~~~~~~~~
  sealed abstract class ImmutableArray[+A] {
    def ++[B >: A: ClassTag](o: ImmutableArray[B]): ImmutableArray[B]
    ...
  }
  object ImmutableArray {
    final class StringArray(s: String) extends ImmutableArray[Char] { ... }
    sealed class ImmutableArray1[+A](as: Array[A]) extends ImmutableArray[A] { ... }
    final class ofRef[A <: AnyRef](as: Array[A]) extends ImmutableArray1[A](as)
    ...
    final class ofLong(as: Array[Long]) extends ImmutableArray1[Long](as)
  
    def fromArray[A](x: Array[A]): ImmutableArray[A] = ...
    def fromString(str: String): ImmutableArray[Char] = ...
    ...
  }
~~~~~~~~

Bila kita berbicara mengenai performa pembacaan dan ukuran *heap*, tidak
ada yang mengalahkan `Array`. Namun, pembagian struktural sama sekali
tidak ada saat pembuatan array baru. Tiadanya penggunaan struktur memori
yang sama seperti ini merupakan salah satu alasan untuk menggunakan deret
(*array*) untuk data yang tidak diharapkan untuk berubah.


### `Dequeue`

`Dequeue`, diucapkan seperti "dek" kapal, merupakan senarai berantai
yang memperkenankan penambahan dan pengambilan item dari depan maupun
dari belakang dengan waktu konstan. Penghapusan elemen dari ujung-ujungnya
juga menggunakan waktu konstan.

{lang="text"}
~~~~~~~~
  sealed abstract class Dequeue[A] {
    def frontMaybe: Maybe[A]
    def backMaybe: Maybe[A]
  
    def ++(o: Dequeue[A]): Dequeue[A] = ...
    def +:(a: A): Dequeue[A] = cons(a)
    def :+(a: A): Dequeue[A] = snoc(a)
    def cons(a: A): Dequeue[A] = ...
    def snoc(a: A): Dequeue[A] = ...
    def uncons: Maybe[(A, Dequeue[A])] = ...
    def unsnoc: Maybe[(A, Dequeue[A])] = ...
    ...
  }
  private final case class SingletonDequeue[A](single: A) extends Dequeue[A] { ... }
  private final case class FullDequeue[A](
    front: NonEmptyList[A],
    fsize: Int,
    back: NonEmptyList[A],
    backSize: Int) extends Dequeue[A] { ... }
  private final case object EmptyDequeue extends Dequeue[Nothing] { ... }
  
  object Dequeue {
    def empty[A]: Dequeue[A] = EmptyDequeue()
    def apply[A](as: A*): Dequeue[A] = ...
    def fromFoldable[F[_]: Foldable, A](fa: F[A]): Dequeue[A] = ...
    ...
  }
~~~~~~~~

Cara kerja dari `Dequeue` adalah dengan menggunakan dua daftar, satu
di depan dan lainnya di belakang. Anggap sebuah instans yang berisi
simbol `a0, a1, a2, a3, a4, a5, a6`

{lang="text"}
~~~~~~~~
  FullDequeue(
    NonEmptyList('a0, IList('a1, 'a2, 'a3)), 4,
    NonEmptyList('a6, IList('a5, 'a4)), 3)
~~~~~~~~

yang dapat digambarkan sebagai

{width=30%}
![](images/dequeue.png)

Harap perhatikan bahwa senarai pada `back` disusun secara terbalik.

Untuk membaca `snoc` (elemen paling akhir) hanya merupakan pencarian
sederhana pada `back.head`. Sedangkan untuk penambahan sebuah elemen
pada akhir `Dequeue` dilakukan dengan menambahkan sebuah elemen pada
bagian awal dari `back` dan membuat ulang kulit `FullDequeue` (yang
akan menambah ukuran `backSize`). Hampir semua struktur data awal
akan digunakan ulang bila terjadi perubahan. Sebagai perbandingan,
penambahan sebuah elemen pada ujung belakan `IList` akan menciptakan
seluruh struktur yang baru.

`frontSize` dan `backSize` digunakan untuk menyeimbangkan ulang `front`
dan `back` sehingga ukuran keduanya kurang lebih sama. Penyeimbangan ulang
juga berarti bahwa beberapa operasi akan lebih lamban bila dibandingkan
dengan operasi lainnya (mis, saat pembangunan ulang struktur secara
menyeluruh). Namun, hal ini hanya kadang terjadi. Untuk penyederhanaan,
kita bisa mengambil rerata dari waktu penggunaan dan menganggapnya
konstan.


### `DList`

Senarai berantai mempunyai karakteristik performa yang kurang baik bila
senarai berukuran besar digabungkan. Sebagai gambaran, silakan diperhatikan
operasi yang berjalan saat mengevaluasi potongan kode berikut:

{lang="text"}
~~~~~~~~
  ((as ::: bs) ::: (cs ::: ds)) ::: (es ::: (fs ::: gs))
~~~~~~~~

{width=50%}
![](images/dlist-list-append.png)

Operasi tersebut membuat enam senarai sementara, melangkahi, dan membangun
ulang tiap senarai sebanyak tiga kali (kecuali `gs` yang dibagi pada
semua tahap).

`DList` (*difference list*) merupakan solusi yang lebih efisien untuk
skenario semacam ini. Kita tidak melakukan evaluasi pada tiap tahap,
namun kita merepresentasikannya sebagai sebuah fungsi `IList[A] => IList[A]`

{lang="text"}
~~~~~~~~
  final case class DList[A](f: IList[A] => IList[A]) {
    def toIList: IList[A] = f(IList.empty)
    def ++(as: DList[A]): DList[A] = DList(xs => f(as.f(xs)))
    ...
  }
  object DList {
    def fromIList[A](as: IList[A]): DList[A] = DList(xs => as ::: xs)
  }
~~~~~~~~

A> Potongan kode disamping merupakan implementasi yang disederhanakan:
A> terdapat kutu *stack overflow* yang akan kita benahi pada bab Monad
A> Lanjutan.

Kalkulasi yang ekuivalen adalah (simbol dibuat dengan menggunakan
`DList.fromIList`) 

{lang="text"}
~~~~~~~~
  (((a ++ b) ++ (c ++ d)) ++ (e ++ (f ++ g))).toIList
~~~~~~~~

yang membagi tugas menjadi penambahan dengan sifat *asosiatif-kanan*

{lang="text"}
~~~~~~~~
  (as ::: (bs ::: (cs ::: (ds ::: (es ::: (fs ::: gs))))))
~~~~~~~~

menggunakan konstruktor pada `IList`.

Sebagaimana biasanya, selalu ada harga yang harus dibayar. Terdapat
alokasi memori tambahan yang dapat memperlambat kode yang berasal dari
penambahan yang bersifat asosiatif-kanan. Operasi yang mendapatkan
percepatan paling besar adalah ketika operasi pada `IList` bersifat
asosiatif kiri, mis,

{lang="text"}
~~~~~~~~
  ((((((as ::: bs) ::: cs) ::: ds) ::: es) ::: fs) ::: gs)
~~~~~~~~

Bila `DList` bernama `ListBuilderFactory`, sangat memungkinkan
bahwa struktur data ini akan ada pada pustaka standar. Namun
karena reputasi yang buruk, hal ini tidak terjadi.


### `ISet`

Struktur pohon dikenal sangat cocok untuk menyimpan data yang terurut
dengan tiap simpul berisi elemen bernilai yang lebih kecil dari pada
satu cabang dan lebih besar bila dibandingkan pada cabang lainnya.
Namun implementasi naif atas struktur data pohon dapat menyebabkan
tidak seimbangnya pohon tersebut pada saat penyisipan elemen. Juga
memungkinkan untuk memiliki pohon yang seimbang namun sangat tidak
efisien dilakukan karena tiap kali penyisipan elemen dilakukan, pohon
tersebut akan dibangun ulang.

`ISet` merupakan implementasi dari pohon dengan keseimbangan berbatas yang
berarti pohon ini diperkirakan seimbang, dengan menggunakan ukuran (`size`)
dari tiap cabang untuk menyeimbangka sebuah simpul.

{lang="text"}
~~~~~~~~
  sealed abstract class ISet[A] {
    val size: Int = this match {
      case Tip()        => 0
      case Bin(_, l, r) => 1 + l.size + r.size
    }
    ...
  }
  object ISet {
    private final case class Tip[A]() extends ISet[A]
    private final case class Bin[A](a: A, l: ISet[A], r: ISet[A]) extends ISet[A]
  
    def empty[A]: ISet[A] = Tip()
    def singleton[A](x: A): ISet[A] = Bin(x, Tip(), Tip())
    def fromFoldable[F[_]: Foldable, A: Order](xs: F[A]): ISet[A] =
      xs.foldLeft(empty[A])((a, b) => a insert b)
    ...
  }
~~~~~~~~

`ISet` mengharap `A` untuk mempunyai kelas tipe `Order`. Instans `Order[A]`
harus tetap sama disela tiap pemanggilan. Bila tidak, asumsi internal akan
invalid dan menyebabkan korupsi data: mis, kita mengasumsikan koherensi
kelas tipe dimana `Order[A]` unik untuk tiap `A`.

Sayangnya, ADT `ISet` melarang adanya pohon invalid. Kita akan berusaha untuk
menulis ADT yang mendeskripsikan secara lengkap mengenai apa yang valid dan
tidak dengan menggunakan pembatasan tipe. Namun, kadang kala ada beberapa
situasi yang menyebabkan hal ini hanya dapat dicapai saat mendapat bisikan
dari leluhur. `Tip` / `Bin` dibuat `private` untuk mencegah pengguna tanpa
sadar membuat pohon yang invalid. `.insert` merupakan satu-satunya cara
untuk membuat `ISet`. Sehingga, `.insert` merupakan pendefinisian dari
sebuah pohon yang valid.

{lang="text"}
~~~~~~~~
  sealed abstract class ISet[A] {
    ...
    def contains(x: A)(implicit o: Order[A]): Boolean = ...
    def union(other: ISet[A])(implicit o: Order[A]): ISet[A] = ...
    def delete(x: A)(implicit o: Order[A]): ISet[A] = ...
  
    def insert(x: A)(implicit o: Order[A]): ISet[A] = this match {
      case Tip() => ISet.singleton(x)
      case self @ Bin(y, l, r) => o.order(x, y) match {
        case LT => balanceL(y, l.insert(x), r)
        case GT => balanceR(y, l, r.insert(x))
        case EQ => self
      }
    }
    ...
  }
~~~~~~~~

Metoda internal `.balanceL` dan `.balanceR` merupakan pencerminan satu sama
lain. Sehingga, kita hanya perlu mempelajari `.balanceL` yang akan dipanggil
ketika nilai yang kita sisipkan kurang dari nilai yang ada pada simpul saat ini.
Metoda ini juga dipanggil oleh metoda `.delete`.

{lang="text"}
~~~~~~~~
  def balanceL[A](y: A, left: ISet[A], right: ISet[A]): ISet[A] = (left, right) match {
  ...
~~~~~~~~

Menyeimbangkan sebuah pohon mengharuskan kita untuk mengklasifikasi
skenario yang mungkin terjadi. Kita akan melihat satu persatu dan memvisualisasi
`(y, left, right)` yang ada pada bagian kira laman dan struktur yang sudah
diseimbangkan pada bagian kanan. Hal ini juga dikenal sebagai pohon yang dirotasi.

-   lingkaran yang terisi melambangkan sebuah `Tip`
-   tiga kolom melambangkan nilai `left | value | right` dari `Bin`
-   wajik melambangkan `ISet`

Skenario pertama merupakan contoh sepele, dimana kedua sisi merupakan `Tip`.
Nyatanya, kita tidak akan pernah menemui hal semacam ini dari pemanggilan
`.insert`. Namun, kita akan menemukannya dengan pemanggilan `.delete`

{lang="text"}
~~~~~~~~
  case (Tip(), Tip()) => singleton(y)
~~~~~~~~

{width=50%}
![](images/balanceL-1.png)

Pada contoh kedua, `left` merupakan sebuah `Bin` yang hanya berisi sebuah
`Tip`. Kita tidak perlu menyeimbangkan apaun, cukup membuat kesimpulan
sederhana:

{lang="text"}
~~~~~~~~
  case (Bin(lx, Tip(), Tip()), Tip()) => Bin(y, left, Tip())
~~~~~~~~

{width=60%}
![](images/balanceL-2.png)

Contoh ketiga-lah yang menarik: `left` berupa sebuah `Bin` yang merisi `Bin`
pada `right`

{lang="text"}
~~~~~~~~
  case (Bin(lx, Tip(), Bin(lrx, _, _)), Tip()) =>
    Bin(lrx, singleton(lx), singleton(y))
~~~~~~~~

{width=70%}
![](images/balanceL-3.png)

Apa yang terjadi pada kedua wajik yang ada pada di bawah `lrx`?
Apakah kita akan kehilangan informasi? Tentu tidak, kita tidak kehilangan
informasi karena kita dapat menalar (menggunakan penyeimbangan ukuran) bahwa
kedua wajik tersebut menjadi `Tip`.
Tidak ada aturan khusus untuk skenario berikut (atau pada `.balanceR`) yang
dapat membuat sebuah pohon dimana sebuah wajik-lah yang menjadi `Bin`.

Contoh keempat merupakan kebalikan dari contoh ketiga.

{lang="text"}
~~~~~~~~
  case (Bin(lx, ll, Tip()), Tip()) => Bin(lx, ll, singleton(y))
~~~~~~~~

{width=70%}
![](images/balanceL-4.png)

Untuk contoh ke lima, kita mempunyai pohon yang lengkap pada kedua sisi
dari `left` dan kita harus menggunakan ukuran relatifnya untuk menentukan
bagaimana kita harus menyeimbangkan.

{lang="text"}
~~~~~~~~
  case (Bin(lx, ll, lr), Tip()) if (2*ll.size > lr.size) =>
    Bin(lx, ll, Bin(y, lr, Tip()))
  case (Bin(lx, ll, Bin(lrx, lrl, lrr)), Tip()) =>
    Bin(lrx, Bin(lx, ll, lrl), Bin(y, lrr, Tip()))
~~~~~~~~

Pada cabang pertama, `2ll.size > lr.size`

{width=50%}
![](images/balanceL-5a.png)

dan untuk cabang kedua `2ll.size <= lr.size`

{width=75%}
![](images/balanceL-5b.png)

Pada skenario ke enam, kita akan mendapatkan sebuah pohon pada `right`.
Saat `left` kosong, kita menarik sebuah sambungan sederhana. Skenario ini
tidak pernah muncul dari `.insert` karena `left` tidak boleh kosong:

{lang="text"}
~~~~~~~~
  case (Tip(), r) => Bin(y, Tip(), r)
~~~~~~~~

{width=50%}
![](images/balanceL-6.png)

Skenario akhir adalah kondisi dimana kita tidak mempunyai pohon yang tidak kosong
pada kedua sisinya. Bila `left` tidak lebih dari tiga kali ukuran dari `right`
kita hanya tinggal membuat sebuah `Bin`

{lang="text"}
~~~~~~~~
  case _ if l.size <= 3 * r.size => Bin(y, l, r)
~~~~~~~~

{width=50%}
![](images/balanceL-7a.png)

Namun, bila `left` berukuran tiga kali ataupun lebih bila dibandingkan `right`,
kita harus menyeimbangkan pohon tersebut dahulu berdasarkan ukuran dari
`ll` dan `lr` seperti pada skenario ke lima.

{lang="text"}
~~~~~~~~
  case (Bin(lx, ll, lr), r) if (2*ll.size > lr.size) =>
    Bin(lx, ll, Bin(y, lr, r))
  case (Bin(lx, ll, Bin(lrx, lrl, lrr)), r) =>
    Bin(lrx, Bin(lx, ll, lrl), Bin(y, lrr, r))
~~~~~~~~

{width=60%}
![](images/balanceL-7b.png)

{width=75%}
![](images/balanceL-7c.png)

Skenario ini menutup pembelajaran kita atas metoda `.insert` dan bagaimana
`ISet` dibangun. Seharusnya bukan hal yang mengejutkan bila `Foldable`
diimplementasikan dalam bentuk pencarian pertama mendalam pada `left` dan `right`.
Metoda semacam `.minimum` dan `.maximum` akan optimum diimplementasikan karena
struktur data sudah tersandikan berurutan.

Hal yang patut diperhatikan adalah beberapa metoda pada kelas tipe tidak dapat
diterapkan secara efisien. Misal, penanda untuk `Foldable.element`

{lang="text"}
~~~~~~~~
  @typeclass trait Foldable[F[_]] {
    ...
    def element[A: Equal](fa: F[A], a: A): Boolean
    ...
  }
~~~~~~~~

Penerapan yang paling jelas untuk `.element` adalah dengan menunda pencarian
biner `ISet.contains`. Walaupun demikian, hal ini tidak mungkin dilakukan karena
`.element` menyediakan `Equal`, sedangkan `.contains` meminta `Order`.

Karena beberapa hal, `ISet` tidak dapat menyediakan `Functor`. Di lapangan,
ternyata hal ini menjadi batasan yang masuk akal: melakukan pemetaan `.map`
juga berarti membangun ulang struktur secara keseluruhan. Tentu hal yang
masuk akal untuk mengkonversi tipe data lain, seperti `IList`, dilanjutkan
dengan melakukan pemetaan `.map`, dan diakhiri dengan konversi ulang.
Sebuah konsekuensi yang muncul adalah kita tidak mungkin mempunyai `Traverse[ISet]`
maupun `Applicative[ISet]`.


### `IMap`

{lang="text"}
~~~~~~~~
  sealed abstract class ==>>[A, B] {
    val size: Int = this match {
      case Tip()           => 0
      case Bin(_, _, l, r) => 1 + l.size + r.size
    }
  }
  object ==>> {
    type IMap[A, B] = A ==>> B
  
    private final case class Tip[A, B]() extends (A ==>> B)
    private final case class Bin[A, B](
      key: A,
      value: B,
      left: A ==>> B,
      right: A ==>> B
    ) extends ==>>[A, B]
  
    def apply[A: Order, B](x: (A, B)*): A ==>> B = ...
  
    def empty[A, B]: A ==>> B = Tip[A, B]()
    def singleton[A, B](k: A, x: B): A ==>> B = Bin(k, x, Tip(), Tip())
    def fromFoldable[F[_]: Foldable, A: Order, B](fa: F[(A, B)]): A ==>> B = ...
    ...
  }
~~~~~~~~

Terlihat familiar, bukan? Dan memang demikian adanya. `IMap` yang mempunyai
alias `==>>`, merupakan pohon dengan ukuran yang diseimbangkan dan ditambah
dengan sebuah bidang tambahan `value: B` pada tiap cabang biner.
Tambahan ini memperkenankan pohon data ini untuk menyimpan pasangan kunci/nilai.
Batasan untuk kunci/nilai pada pohon ini hanyalah tipe kunci `A` harus mempunyai
instans `Order`. Selain itu, ada beberapa metoda tersedia yang dapat digunakan
untuk memutakhirkan isi dari pohon ini.

{lang="text"}
~~~~~~~~
  sealed abstract class ==>>[A, B] {
    ...
    def adjust(k: A, f: B => B)(implicit o: Order[A]): A ==>> B = ...
    def adjustWithKey(k: A, f: (A, B) => B)(implicit o: Order[A]): A ==>> B = ...
    ...
  }
~~~~~~~~


### `StrictTree` dan `Tree`

`StrictTree` dan `Tree` merupakan penerapan dari pohon beringin.
Pohon beringin sendiri merupakan struktur pohon dengan jumlah cabang yang tak
dibatasi pada tiap simpulnya. Kedua struktur data ini, dibangun dengan
menggunakan pustaka koleksi dari pustaka standar dikarenakan alasan
peninggalan masa lalu:

{lang="text"}
~~~~~~~~
  case class StrictTree[A](
    rootLabel: A,
    subForest: Vector[StrictTree[A]]
  )
~~~~~~~~

`Tree` merupakan versi *by-need* dari `StrictTree` dengan konstruktor

{lang="text"}
~~~~~~~~
  class Tree[A](
    rootc: Need[A],
    forestc: Need[Stream[Tree[A]]]
  ) {
    def rootLabel = rootc.value
    def subForest = forestc.value
  }
  object Tree {
    object Node {
      def apply[A](root: =>A, forest: =>Stream[Tree[A]]): Tree[A] = ...
    }
    object Leaf {
      def apply[A](root: =>A): Tree[A] = ...
    }
  }
~~~~~~~~

Secara umum, pengguna pohon beringin diharapkan untuk menyeimbangkan pohon ini
secara manual. Dengan demikian, struktur ini cocok untuk digunakan pada
domain tertentu untuk menyandikan hierarki pada struktur data. Sebagai contoh,
pada kecerdasan buatan, sebuah pohon beringin dapat digunakan pada [algoritma pengelompokan](https://arxiv.org/abs/1203.3468)
untuk mengelompokkan data menjadi sebuah hierarki atas hal hal yang semakin
mirip. Struktur ini juga bisa digunakan untuk merepresentasikan dokumen XML.

Saat bekerja dengan struktur data hierarkis, adalah cukup bijak untuk
mempertimbangkan untuk menggunakan struktur data ini, bukan membuat struktur
data sendiri.


### `FingerTree`

*Finger Tree* (selanjutnya disebut pohon palem) merupakan deretan yang digeneralisasikan
dengan beban pencarian konstan yang teramortisasi dan penggabungan logaritmik.
`A` merupakan tipe data dan untuk saat ini hiraukan `V`:

{lang="text"}
~~~~~~~~
  sealed abstract class FingerTree[V, A] {
    def +:(a: A): FingerTree[V, A] = ...
    def :+(a: =>A): FingerTree[V, A] = ...
    def <++>(right: =>FingerTree[V, A]): FingerTree[V, A] = ...
    ...
  }
  object FingerTree {
    private class Empty[V, A]() extends FingerTree[V, A]
    private class Single[V, A](v: V, a: =>A) extends FingerTree[V, A]
    private class Deep[V, A](
      v: V,
      left: Finger[V, A],
      spine: =>FingerTree[V, Node[V, A]],
      right: Finger[V, A]
    ) extends FingerTree[V, A]
  
    sealed abstract class Finger[V, A]
    final case class One[V, A](v: V, a1: A) extends Finger[V, A]
    final case class Two[V, A](v: V, a1: A, a2: A) extends Finger[V, A]
    final case class Three[V, A](v: V, a1: A, a2: A, a3: A) extends Finger[V, A]
    final case class Four[V, A](v: V, a1: A, a2: A, a3: A, a4: A) extends Finger[V, A]
  
    sealed abstract class Node[V, A]
    private class Node2[V, A](v: V, a1: =>A, a2: =>A) extends Node[V, A]
    private class Node3[V, A](v: V, a1: =>A, a2: =>A, a3: =>A) extends Node[V, A]
    ...
  }
~~~~~~~~

A> `<++>` mirip dengan `|+|` milik `Monoid`.

`FingerTree` digambarkan sebagai titk, `Finger` sebagai persegi, dan `Node`
sebagai persegi dalam persegi:

{width=35%}
![](images/fingertree.png)

Penambahanan elemen pada bagian depan sebuah `FingerTree` dengan `+:` efisien
karena `Deep` hanya menambah elemen baru pada bagian kiri (`left`) dari palem.
Bila palem berupa sebuah `Four`, kita akan membangun ulang batang (`spine`)
untuk mengambil 3 elemen sebagai sebuah `Node3`. Sama halnya dengan penambahan
sebuah elemen pada bagian belakang menggunakan `:+`, namun dibalik.

Penambahan menggunakan `|+|` dan `<++>` lebih efisien bila dibandingkan dengan
menambahkan sebuah elemen satu persatu karena dua pohon `Deep` mampu memelihara
cabang bagian luar, membangun batang (`spine`) berdasarkan 16 kombinasi yang
mungkin dari dua nilai `Finger` pada bagian tengah.

Di atas, kita melewatkan `V`. Yang tidak diperlihatkan pada deskripsi
ADT merupakan sebuah `implicit measurer: Reducer[A, V]` pada tiap elemen
dari ADT.

A> Menyimpan instans kelas tipe pada TDA dianggap gaya penulisan yang buruk
A> dan dapat menambah kebutuhan penggunaan memori sebanyak 64 bit untuk
A> tiap catatan. Implementasi `FingerTree` sudah hampir satu dekade
A> dan waktunya penulisan ulang.

`Reducer` merupakan sebuah ekstensi dari `Monoid` yang memperkenankan agar
sebuah elemen dapat ditambahkan ke sebuah `M`

{lang="text"}
~~~~~~~~
  class Reducer[C, M: Monoid] {
    def unit(c: C): M
  
    def snoc(m: M, c: C): M = append(m, unit(c))
    def cons(c: C, m: M): M = append(unit(c), m)
  }
~~~~~~~~

Sebagai contoh, `Reducer[A, IList[A]]` menyediakan implementasi `.cons`
yang efisien

{lang="text"}
~~~~~~~~
  implicit def reducer[A]: Reducer[A, IList[A]] = new Reducer[A, IList[A]] {
    override def unit(a: A): IList[A] = IList.single(a)
    override def cons(a: A, as: IList[A]): IList[A] = a :: as
  }
~~~~~~~~

A> `Reducer` seharusnya dinamai `CanActuallyBuildFrom` untuk menghormati
A> `class` dari pustaka standar yang juga memiliki fungsionalitas yang sama
A> dalam hal pembangunan koleksi.


#### `IndSeq`

Bila kita menggunakan `Int` sebagai `V`, kita bisa mendapatkan barisan terindeks
dimana yang menjadi ukuran adalah jumlah satuan `V`. Hal ini memperkenankan
kita untuk melakukan pencarian berdasarkan indeks dengan membandingkan indeks
yang diinginkan dengan ukuran dari tiap cabang pada struktur:

{lang="text"}
~~~~~~~~
  final class IndSeq[A](val self: FingerTree[Int, A])
  object IndSeq {
    private implicit def sizer[A]: Reducer[A, Int] = _ => 1
    def apply[A](as: A*): IndSeq[A] = ...
  }
~~~~~~~~

Penggunaan lain dari `FingerTree` adalah barisan terurut, dimana yang menjadi
ukuran merupakan nilai terbesar dari setiap cabang:


#### `OrdSeq`

{lang="text"}
~~~~~~~~
  final class OrdSeq[A: Order](val self: FingerTree[LastOption[A], A]) {
    def partition(a: A): (OrdSeq[A], OrdSeq[A]) = ...
    def insert(a: A): OrdSeq[A] = ...
    def ++(xs: OrdSeq[A]): OrdSeq[A] = ...
  }
  object OrdSeq {
    private implicit def keyer[A]: Reducer[A, LastOption[A]] = a => Tag(Some(a))
    def apply[A: Order](as: A*): OrdSeq[A] = ...
  }
~~~~~~~~

`OrdSeq` tidak mempunyai instans kelas tipe dikarenakan struktur data ini
hanya berguna untuk pembangunan deret terurut secara bertahap dengan duplikat.
Kita dapat mengakses `FingerTree` yang melandasi struktur data ini bila dibutuhkan.


#### `Cord`

Penggunaan `FingerTree` yang paling jamak adalah wadah sementara untuk representasi
`String` pada `Show`. Pembuatan sebuah `String` bisa saja ribuan kali lebih cepat
bila dibandingkan dengan implementasi `case class` berlapis dari `.toString` yang
membangun sebuah `Sring` untuk tiap lapisan pada ADT.

{lang="text"}
~~~~~~~~
  final case class Cord(self: FingerTree[Int, String]) {
    override def toString: String = {
      val sb = new java.lang.StringBuilder(self.measure)
      self.foreach(sb.append) // locally scoped side effect
      sb.toString
    }
    ...
  }
~~~~~~~~

Sebagai contoh, instans `Cord[String]` mengembalikan sebuah `Three` dengan
string pada bagian tengah dan tanda petik pada kedua sisi

{lang="text"}
~~~~~~~~
  implicit val show: Show[String] = s => Cord(FingerTree.Three("\"", s, "\""))
~~~~~~~~

Sehingga, sebuah `String` memberikan hasil sebagaimana yang tertulis pada
kode sumber

{lang="text"}
~~~~~~~~
  scala> val s = "foo"
         s.toString
  res: String = foo
  
  scala> s.show
  res: Cord = "foo"
~~~~~~~~

A> Yang disayangkan adalah,`Cord` pada Scalaz 7.2 tidak se-efisien yang diharapkan.
A> Hal ini sudah dibenahi pada Scalaz 7.3 dengan menggunakan [struktur data teroptimasi
A> untuk penggabungan `String`](https://github.com/scalaz/scalaz/pull/1793).


### Antrian Prioritas `Heap`

Antrian prioritas merupakan struktur data yang memperkenankan untuk penyisipan
yang relatif singkat pada elemen terurut yang memperbolehkan adanya duplikasi
elemen dan memiliki waktu akses yang cepat pada nilai minimum atau prioritas
tertinggi. Struktur ini tidak wajibkan untuk menyimpan elemen non-minimal
secara berurutan. Implementasi naif dari antrian prioritas dapat berupa

{lang="text"}
~~~~~~~~
  final case class Vip[A] private (val peek: Maybe[A], xs: IList[A]) {
    def push(a: A)(implicit O: Order[A]): Vip[A] = peek match {
      case Maybe.Just(min) if a < min => Vip(a.just, min :: xs)
      case _                          => Vip(peek, a :: xs)
    }
  
    def pop(implicit O: Order[A]): Maybe[(A, Vip[A])] = peek strengthR reorder
    private def reorder(implicit O: Order[A]): Vip[A] = xs.sorted match {
      case INil()           => Vip(Maybe.empty, IList.empty)
      case ICons(min, rest) => Vip(min.just, rest)
    }
  }
  object Vip {
    def fromList[A: Order](xs: IList[A]): Vip[A] = Vip(Maybe.empty, xs).reorder
  }
~~~~~~~~

`push` bisa sangat cepat (`O(1)`)  walau `reorder` (dan `pop`) sangat bergantung
pada `IList.sorted` yang bernilai `O(n log n)`.

Scalaz menyandikan antrian prioritas dengan struktur pohon dimana setiap simpul
mempunyai nilai kurang dari anaknya. `Heap` mempunyai waktu operasi `insert`,
`union`, `size`, `uncons, dan `minimumO`:

{lang="text"}
~~~~~~~~
  sealed abstract class Heap[A] {
    def insert(a: A)(implicit O: Order[A]): Heap[A] = ...
    def +(a: A)(implicit O: Order[A]): Heap[A] = insert(a)
  
    def union(as: Heap[A])(implicit O: Order[A]): Heap[A] = ...
  
    def uncons(implicit O: Order[A]): Option[(A, Heap[A])] = minimumO strengthR deleteMin
    def minimumO: Option[A] = ...
    def deleteMin(implicit O: Order[A]): Heap[A] = ...
  
    ...
  }
  object Heap {
    def fromData[F[_]: Foldable, A: Order](as: F[A]): Heap[A] = ...
  
    private final case class Ranked[A](rank: Int, value: A)
  
    private final case class Empty[A]() extends Heap[A]
    private final case class NonEmpty[A](
      size: Int,
      tree: Tree[Ranked[A]]
    ) extends Heap[A]
  
    ...
  }
~~~~~~~~

A> `size` dimemoisasi pada TDA untuk memperkenankan kalkulasi instan berdasarkan
A> `Foldable.length` dengan ganti rugi sebesar 64 bit untuk tiap catatan.
A> Varian dari `Heap` bisa dibuat dengan ukuran yang lebih kecil namun dengn
A> waktu eksekusi `Foldable.length` yang lebih lamban.

`Heap` diimplementasi dengan Pohon Palem berdasarkan nilai `Ranked` dimana
`rank`ing merupakan kedalaman dari cabang pohon. Hal ini memperkenankan kita untuk
menyeimbangkan kedalaman dari struktur pohon tersebut. Kita juga mempertahankan
secara manual agar nilai `minimum` selalu pada bagian paling atas. Keuntungan
dari penyandian nilai minimum pada struktur data adalah `minimumO` adalah biaya
pencarian gratis:

{lang="text"}
~~~~~~~~
  def minimumO: Option[A] = this match {
    case Empty()                        => None
    case NonEmpty(_, Tree.Node(min, _)) => Some(min.value)
  }
~~~~~~~~

Ketika menyisipkan sebuah catatan, kita membandingkan nilai minimum saat ini
dan menggantinya dengan catatan baru bila ternyata lebih rendah:

{lang="text"}
~~~~~~~~
  def insert(a: A)(implicit O: Order[A]): Heap[A] = this match {
    case Empty() =>
      NonEmpty(1, Tree.Leaf(Ranked(0, a)))
    case NonEmpty(size, tree @ Tree.Node(min, _)) if a <= min.value =>
      NonEmpty(size + 1, Tree.Node(Ranked(0, a), Stream(tree)))
  ...
~~~~~~~~

Penyisipan nilai nilai non-minimal menghasilkan struktur tak-urut pada
cabang minimum. Saat kita menemukan dua atau lebih sub-pohon dengan `rank`ing
yang sama, kita akan menempatkan nilai minimum pada bagian depan:

{lang="text"}
~~~~~~~~
  ...
    case NonEmpty(size, Tree.Node(min,
           (t1 @ Tree.Node(Ranked(r1, x1), xs1)) #::
           (t2 @ Tree.Node(Ranked(r2, x2), xs2)) #:: ts)) if r1 == r2 =>
      lazy val t0 = Tree.Leaf(Ranked(0, a))
      val sub =
        if (x1 <= a && x1 <= x2)
          Tree.Node(Ranked(r1 + 1, x1), t0 #:: t2 #:: xs1)
        else if (x2 <= a && x2 <= x1)
          Tree.Node(Ranked(r2 + 1, x2), t0 #:: t1 #:: xs2)
        else
          Tree.Node(Ranked(r1 + 1, a), t1 #:: t2 #:: Stream())
  
      NonEmpty(size + 1, Tree.Node(Ranked(0, min.value), sub #:: ts))
  
    case NonEmpty(size,  Tree.Node(min, rest)) =>
      val t0 = Tree.Leaf(Ranked(0, a))
      NonEmpty(size + 1, Tree.Node(Ranked(0, min.value), t0 #:: rest))
  }
~~~~~~~~

Dengan menghindari pengurutan secara menyeluruh terhadap pohon, `insert` menjadi
sangat cepat (dengan kompleksitas `O(1)`), dimana yang melakukan operasi ini
tidak terbebani oleh operasi ini. Namun, saat melakukan `uncons` dengan `deleteMin`,
kita akan mendapati bahwa operasi ini mempunyai kompleksitas `O(log n)` yang disebabkan
oleh pencarian nilai minimum dan menghapusnya dari pohon dengan membangun ulang.
Secara umum, hal ini lebih cepat bila dibandingkan dengan implementasi naif.

Operasi `union` juga dapat menghambat pengurutan sehingga operasi ini mempunyai
kompleksitas `O(1)`.

Bila `Order[Foo]` tidak dapat dengat tepat menentukan prioritas yang kita inginkan
atas `Heap[Foo]`, kita dapat menggunakan `Tag` dan menyediakan instans
`Order[Foo @@ Custom]` khusus untuk `Head[Foo @@ Custom]`.


### `Diev` (Interval Diskrit)

Kita dapat dengan mudah menyandikan nilai integer antara 6, 9, 2, 13, 8, 14, 10,
7, 5 sebagai interval inklusif `[2, 2], [5, 10], [13, 14]`. `Diev` merupakan
metoda penyadian efisien atas *interval* untuk elemen `A` yangc mempunyai instans
kelas tipe `Enum[A]` yang akan semuakin efisien bila isi dari struktur data ini
semakin padat.

{lang="text"}
~~~~~~~~
  sealed abstract class Diev[A] {
    def +(interval: (A, A)): Diev[A]
    def +(value: A): Diev[A]
    def ++(other: Diev[A]): Diev[A]
  
    def -(interval: (A, A)): Diev[A]
    def -(value: A): Diev[A]
    def --(other: Diev[A]): Diev[A]
  
    def intervals: Vector[(A, A)]
    def contains(value: A): Boolean
    def contains(interval: (A, A)): Boolean
    ...
  }
  object Diev {
    private final case class DieVector[A: Enum](
      intervals: Vector[(A, A)]
    ) extends Diev[A]
  
    def empty[A: Enum]: Diev[A] = ...
    def fromValuesSeq[A: Enum](values: Seq[A]): Diev[A] = ...
    def fromIntervalsSeq[A: Enum](intervals: Seq[(A, A)]): Diev[A] = ...
  }
~~~~~~~~

Saat memutakirkan `Diev`, interval yang berdekatan akan digabungkan (dan diurutkan)
sehingga sebuah set nilai akan mempunyai sebuah representasi yang unik.

{lang="text"}
~~~~~~~~
  scala> Diev.fromValuesSeq(List(6, 9, 2, 13, 8, 14, 10, 7, 5))
  res: Diev[Int] = ((2,2)(5,10)(13,14))
  
  scala> Diev.fromValuesSeq(List(6, 9, 2, 13, 8, 14, 10, 7, 5).reverse)
  res: Diev[Int] = ((2,2)(5,10)(13,14))
~~~~~~~~

Salah satu contoh penggunaan untuk `Diev` adalah penyimpanan periode waktu.
Sebagai contoh konkret, pada `TradeTemplate` kita pada bab sebelumnya.

{lang="text"}
~~~~~~~~
  final case class TradeTemplate(
    payments: List[java.time.LocalDate],
    ccy: Option[Currency],
    otc: Option[Boolean]
  )
~~~~~~~~

kita akan menemui bahwa `payments` sangat padat, kita mungkin berharap untuk
menggantinya dengan representasi `Diev` dengan alasan performa tanpa mengubah
logika bisnis dikarenakan kita menggunakan `Monoid`, bukan `List`. Walaupun hal
itu berarti kita harus menyediakan instance `Enum[LocalDate]`.


### `OneAnd`

Seperti yang sudah dipelajari, `Foldable` merupakan pustaka setara untuk
pustaka koleksi dan `Foldable1` untuk koleksi non-kosong. Sementara ini, kita
baru melihat `NonEmptyList` untuk menyediakan instans `Foldable1`. Struktur data
sederhana `OneAnd` melapisi semua koleksi lain menkadi `Foldable1`:

{lang="text"}
~~~~~~~~
  final case class OneAnd[F[_], A](head: A, tail: F[A])
~~~~~~~~

`NonEmptyList` bisa merupakan alias untuk `OneAnd[IList]`. Sama halnya dengan
alias dari struktur data ini, kita bisa membuat `Stream`, `DList`, dan `Tree`.
Namun, penggunaan ini dapat menghapus penyusunan dan keunikan dari struktur yang
melandasinya: sebuah `OneAnd[ISet, A]` adalah struktur non-kosong dari `ISet`.
Namun, elemen pertama dari struktur ini pasti tidak kosong dan bisa jadi juga
merupakan elemen dari `ISet` sehingga struktur ini tidak menjadi unik lagi.


## Kesimpulan

Pada bab ini, kita sudah mempelajari secara sekilas tentang tipe data yang
ditawarkan oleh Scalaz.

Pembaca yang budiman tidak harus menghafal struktur data yang ada pada bab
ini, dan cukup menganggap bab ini sebagai pengantar.

Pada jagad pemrograman fungsional, struktur data fungsional merupakan area
riset yang aktif. Publikasi akademis juga sering muncul dengan pendekatan baru
atas permasalahan yang sudah lama dikenal. Menerapkan sebuah struktur data
fungsional dari literatur semacam itu merupakan kontrubusi yang sangat diterima
untuk ekosistem Scalaz.


# Monad Lanjutan

Untuk menjadi pemrogram dengan aliran fungsional, pembaca budiman harus
menguasai beberapa hal, seperti Monad Lanjutan.

Namun, karena kita merupakan pengembang yang mendambakan hal yang sederhana,
juga tidak melupakan bahwa apa yang kita sebut sebagai "lanjutan" juga tetap
sederhana. Sebagai konteks: `scala.concurrent.Future` lebih rumit dan penuh
dengan nuansa bila dibandingkan dengan semua `Monad` yang ada pada bab ini. 

Pada bab ini, kita akan mempelajari beberapa penerapan paling penting atas
`Monad`.


## Masa Depan yang Kabur

Masalah paling besar dengan `Future` adalah struktur ini segera menjadwalkan
tugas pada saat konstruktsi. Sebagaimana yang telah kita bicarakan pada
perkenalan, `Future` menggabungkan antara definisi program dengan
*menerjemahkannya*.

Dan bila dilihat dari sudut pandang performa, `Future` tidak begitu menarik:
setiap kali `.flatMap` dipanggil, sebuah *closure* diserahkan kepada sebuah
`Executor` sehingga menyebabkan penjadwalan dan pertukaran konteks yang tak perlu.
Bukan hal yang jarang terjadi bila kita melihat 50% penggunaan CPU saat berurusan
dengan penjadwalan utas, bukan saat melakukan komputasi program. Bahkan, bukan
hal yang tidak mungkin untuk mendapatkan hasil komputasi paralel yang lebih lambat
saat menggunakan `Future`.

Bila evaluasi tegas dan penyerahan eksekutor digunakan secara bersamaan, pengguna
tidak akan tahu kapan tugas akan dimulai, selesai, atau sub-tugas yang dibuat
untuk menghitung hasil akhir. Seharusnya, bukan hal yang mengejutkan bila
*solusi* untuk melakukan pengawasan atas *framework* yang dibuat berdasarkan
`Future` memang pantas disebut sebagai tukang tipe.

Terlebih lagi, `Future.flatMap` mengharuskan sebuah `ExecutionContext` berada
pada cakupan implisit: pengguna dipaksa untuk memikirkan logika bisnis dan
semantik dari eksekusi pada saat yang bersamaan. 


## Efek dan Efek Samping

Bila kita tidak boleh memanggil metoda dengan efek samping pada logika bisnis
kita, atau pada `Future` (atau pada `Id`, `Either`, ataupun `Const`, dll),
**kapan kita bisa**? Tentu jawabannya ada pada `Monad` yang menunda eksekusi
sampai pada waktunya `Monad` ini diinterpretasi pada titik awal aplikasi.
Mulai dari sini, kita akan merujuk I/O dan mutasi sebagai *efek* pada dunia luar
yang ditangkap oleh sistem tipe, bukan sistem dengan *efek samping* tersembunyi.

Implementasi paling sederhana dari sebuah `Monad` adalah `IO`, yang memformalkan
apa yang telah kita tulis pada bagian perkenalan sebagai:

{lang="text"}
~~~~~~~~
  final class IO[A](val interpret: () => A)
  object IO {
    def apply[A](a: =>A): IO[A] = new IO(() => a)
  
    implicit val monad: Monad[IO] = new Monad[IO] {
      def point[A](a: =>A): IO[A] = IO(a)
      def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(f(fa.interpret()).interpret())
    }
  }
~~~~~~~~

Metoda `.interpret` hanya dipanggil sekali pada titik awal sebuah aplikasi:

{lang="text"}
~~~~~~~~
  def main(args: Array[String]): Unit = program.interpret()
~~~~~~~~

Namun, ada dua masalah utama pada `IO` sederhana semacam ini:

1.  dapat menyebabkan *stack overflow*
2.  tidak mendukung komputasi paralel.

Kedua masalah ini akan diselesaikan pada bab ini. Namun, serumit apapun
implmentasi internal dari sebuah `Monad`, prinsip yang dijabarkan disini tidak
berubah: kita memodularisasi pendefinisian dari sebuah program dan eksekusinya
sehingga kita dapat menangkap efek yang muncul pada penanda tipe, dan pada akhirnya
memperkenankan kita untuk menalar hasil modularisasi program tersebut dan
menghasilkan penggunaan ulang kode yang lebih banyak.

A> Kompiler Scala memperkenankan kita untuk memanggil metoda dengan efek samping
A> pada blok kode tak aman. [Scalafix](https://scalacenter.github.io/scalafix)
A> yang merupakan alat bantu untuk pelarangan metoda dengan efek samping pada
A> saat kompilasi dapat digunakan untuk memastikan bahwa semua metoda dengan
A> efek samping dipanggil didalam sebuah `Monad` seperti `IO`.


## Keamanan *Stack*

Pada JVM, setiap pemanggilan metoda menambah sebuah catatan pada *stack* panggilan
pada `Thread`, mirip dengan penambahan sebuah elemen pada bagian depan `List`.
Ketika sebuah metoda selesai dipanggil, metoda pada bagian `head` akan dibuang.
Jumlah maksimal dari *stack* panggilan ini ditentukan oleh panji `-Xss` ketika
memulai `java`. Pemanggilan metoda *tail recursive* dideteksi oleh komplire
Scala dan catatan panggilan tidak akan ditambahkan. Bila kita mencapai batas,
misal dengan pemanggilan rantai metoda yang sangat banyak, kita akan mendapatkan
sebuah `StackOverflowError`.

Sayangnya, tiap panggilan berlapis pada `.flatMap` milik `IO`, sebuah metoda
akan ditambahkan ke *stack*. Cara paling mudah untuk menebak apakah metoda ini
akan dijalankan selamanya atau hanya beberapa saat saja, kita bisa menggunakan
`.forever` dari `Apply` (atasan `Monad`):

{lang="text"}
~~~~~~~~
  scala> val hello = IO { println("hello") }
  scala> Apply[IO].forever(hello).interpret()
  
  hello
  hello
  hello
  ...
  hello
  java.lang.StackOverflowError
      at java.io.FileOutputStream.write(FileOutputStream.java:326)
      at ...
      at monadio.IO$$anon$1.$anonfun$bind$1(monadio.scala:18)
      at monadio.IO$$anon$1.$anonfun$bind$1(monadio.scala:18)
      at ...
~~~~~~~~

Scalaz mempunyai sebuah kelas tipe yang dapat diimplementasikan oleh struktur
data yang memiliki instans `Monad` bila struktur data tersebut aman dari segi
penggunaan *stack*: `BindRec` yang mumbutuhkan ruang *stack* konstan untuk
`bind` rekursif: 

{lang="text"}
~~~~~~~~
  @typeclass trait BindRec[F[_]] extends Bind[F] {
    def tailrecM[A, B](f: A => F[A \/ B])(a: A): F[B]
  
    override def forever[A, B](fa: F[A]): F[B] = ...
  }
~~~~~~~~

Kita tidak perlu menggunakan `BindRec` untk semua program. Namun, kelas tipe ini
penting untuk implementasi umum dari `Monad`.

Cara yang digunakan untuk mendapatkan keamanan *stack* adalah dengan mengkonversi
pemanggilan metoda menjadi rujukan ke sebuah ADT, atau yang dikenal dengan
monad `Free`:

{lang="text"}
~~~~~~~~
  sealed abstract class Free[S[_], A]
  object Free {
    private final case class Return[S[_], A](a: A)     extends Free[S, A]
    private final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
    private final case class Gosub[S[_], A0, B](
      a: Free[S, A0],
      f: A0 => Free[S, B]
    ) extends Free[S, B] { type A = A0 }
    ...
  }
~~~~~~~~

A> `SUSPEND`, `RETURN`, dan `GOSUB` merupakan penghormatan untuk perintah pada
A> bahasa pemrograman `BASIC` untuk mejeda, menyelesaikan, dan melanjutkan
A> sub-rutin.

TDA `Free` merupakan representasi tipe data natural untuk antarmuka `Monad`:

1.  `Return` merepresentasikan `.point`
2.  `Gosub` merepresentasikan `.bind` / `.flatMap`

Ketika sebuah TDA mencerminkan argumen yang berhubungan dengan fungsi yang berhubungan,
pencerminan ini disebut dengan penyandian Church (dari nama Alonzo Church).

`Free` mendapat nama seperti itu karena dapat didapatkan secara cuma-cuma (sebagaimana
dengan "Free Beer") untuk setiap `S[_]`. Sebagai contoh, kita dapat menganggap
`S` sebagai alkabar dari `Drone` atau `Machines` pada bab 3 dan membuat
representasi struktur data dari program kita. Kita akan kembali mempelajari mengapa
hal ini berguna pada akhir bab ini.


### `Trampoline`

Untuk sementara ini, `Free` lebih umum daripada yang kita butuhkan. Dengan
mengatur aljabar `S[_]` menjadi `() => ?`, atau komputasi yang ditangguhkan,
kita mendapatkan struktur `Trampoline` dan pada akhirnya dapat menerapkan
`Monad` dengan aman

{lang="text"}
~~~~~~~~
  object Free {
    type Trampoline[A] = Free[() => ?, A]
    implicit val trampoline: Monad[Trampoline] with BindRec[Trampoline] =
      new Monad[Trampoline] with BindRec[Trampoline] {
        def point[A](a: =>A): Trampoline[A] = Return(a)
        def bind[A, B](fa: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] =
          Gosub(fa, f)
  
        def tailrecM[A, B](f: A => Trampoline[A \/ B])(a: A): Trampoline[B] =
          bind(f(a)) {
            case -\/(a) => tailrecM(f)(a)
            case \/-(b) => point(b)
          }
      }
    ...
  }
~~~~~~~~

Implementasi `BindRec`, `.tailrecM`, menjalankan `.bind` sampai kita mendapat
sebuah `B`. Walau secara teknis hal ini bukan merupakan implmentasi `@tailrec`,
implementasi ini menggunakan ruang *stack* secara konstan karena tiap panggilan
mengembalikan sebuah objek heap dengan rekursi yang dijeda.

A> Disebut sebagai `Trampoline` karena tiap kali kita memanggil `.bind` pada stack,
A> kita akan terpantul kembali ke *heap*.

Fungsi pembantu yang disediakan untuk membuat sebuah `Trampoline` secara
sigap adalah dengan `.done` atau bisa juga dibuat dengan sebuah jeda menggunakan
metoda `.delay`. Kita juga bisa membuat sebuah `Trampoline` dengan menggunakan
`Trampoline` *by-name` dengan metoda `.suspend`: 

{lang="text"}
~~~~~~~~
  object Trampoline {
    def done[A](a: A): Trampoline[A]                  = Return(a)
    def delay[A](a: =>A): Trampoline[A]               = suspend(done(a))
    def suspend[A](a: =>Trampoline[A]): Trampoline[A] = unit >> a
  
    private val unit: Trampoline[Unit] = Suspend(() => done(()))
  }
~~~~~~~~

Saat kita melihat `Trampoline[A]` pada basis kode, kita bisa menggantinya pada
visualisasi mental kita dengan sebuah `A`. Hal ini disebabkan oleh penambahan
keamanan *stack* demi kemurnian komputasi. Kita mendapatkan `A` dengan menginterpretasikan
`Free` dengan metoda `.run` yang telah disediakan.

A> Walaupun tidak perlu, untuk memahami bagaimana `Free.run` diimplementasikan,
A> namun banyak hal yang dapat dipelajari. Sebagai contoh: `.resume` mengevaluasi
A> satu lapis dari `Free` dan `go` menjalankan `Free` sampai selesai.
A>
A> Pada `Trampoline[A]` berikut, trampoline ini digunakan sebagai sinonim dari
A> `Free[() => ?, A]` agar kode lebih mudah untuk dipahami.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   sealed abstract class Trampoline[A] {
A>     def run: A = go(f => f())
A>   
A>     def go(f: () => Trampoline[A] => Trampoline[A]): A = {
A>       @tailrec def go2(t: Trampoline[A]): A = t.resume match {
A>         case -\/(s) => go2(f(s))
A>         case \/-(r) => r
A>       }
A>       go2(this)
A>     }
A>   
A>     @tailrec def resume: () => Trampoline[A] \/ A = this match {
A>       case Return(a) => \/-(a)
A>       case Suspend(t) => -\/(t.map(Return(_)))
A>       case Gosub(Return(a), f) => f(a).resume
A>       case Gosub(Suspend(t), f) => -\/(t.map(f))
A>       case Gosub(Gosub(a, g), f) => a >>= (z => g(z) >>= f).resume
A>     }
A>     ...
A>   }
A> ~~~~~~~~
A> 
A> Permasalahan yang biasanya menjadi penyebab kaburnya pemahaman adalah saat
A> kita mempunyai `Gosub` berlapis: mengaplikasikan fungsi bagian dalam `g` lalu
A> melewatkannya ke fungsi `f` di bagian luar. Padahal, hal ini cuma sekedar
A> komposisi fungsi.


### Contoh: `DList` dengan Keamanan *Stack*

Pada bab sebelumnya, kita mendeskripsikan tipe data `DList` dengan

{lang="text"}
~~~~~~~~
  final case class DList[A](f: IList[A] => IList[A]) {
    def toIList: IList[A] = f(IList.empty)
    def ++(as: DList[A]): DList[A] = DList(xs => f(as.f(xs)))
    ...
  }
~~~~~~~~

Namun, implementasi yang sesungguhnya adalah seperti berikut:

{lang="text"}
~~~~~~~~
  final case class DList[A](f: IList[A] => Trampoline[IList[A]]) {
    def toIList: IList[A] = f(IList.empty).run
    def ++(as: =>DList[A]): DList[A] = DList(xs => suspend(as.f(xs) >>= f))
    ...
  }
~~~~~~~~

Kita tidak menggunakan panggilan berlapis pada `f`, namun kita menggunakan
`Trampoline` yang dibekukan. Interpretasi `.run` hanya dilakukan bila memang
benar dibutuhkan, seperti pada `toIList`. Perubahan yang dilakukan sebenarnya
sedikit, namun kita berhasil mencapai keamanan *stack* atas `DList` yang dapat
melakukan penggabungan `list` dalam jumlah besar tanpa harus memenuhi *stack*.


### `IO` dengan Keamanan *Stack*

Hal yang sama dapat dilakukan untuk mengamankan `IO` dengan menggunakan
`Trampoline`:

{lang="text"}
~~~~~~~~
  final class IO[A](val tramp: Trampoline[A]) {
    def unsafePerformIO(): A = tramp.run
  }
  object IO {
    def apply[A](a: =>A): IO[A] = new IO(Trampoline.delay(a))
  
    implicit val Monad: Monad[IO] with BindRec[IO] =
      new Monad[IO] with BindRec[IO] {
        def point[A](a: =>A): IO[A] = IO(a)
        def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
          new IO(fa.tramp >>= (a => f(a).tramp))
        def tailrecM[A, B](f: A => IO[A \/ B])(a: A): IO[B] = ...
      }
  }
~~~~~~~~

Penerjemah di atas, `.unsafePerformIO()`, memang sengaja dinamai seperti itu
untuk menakut-nakuti pengguna agar tidak menggunakannya selain di titik awal
aplikasi.

Sekarang, kita tidak akan mendapat galat mengenai *stack overflow*:

{lang="text"}
~~~~~~~~
  scala> val hello = IO { println("hello") }
  scala> Apply[IO].forever(hello).unsafePerformIO()
  
  hello
  hello
  hello
  ...
  hello
~~~~~~~~

Penggunaan `Trampoline` biasanya menimbulkan penurunan performa bila dibandingkan
dengan rujukan biasa. Hal ini dikarenakan `Free` disini adalah dibuat tanpa
biaya, bukan digunakan tanpa biaya.

A> Selalu ukur apapun mengenai performa: Bisa jadi pengumpul sampah bekerja lebih
A> baik pada sebuah aplikasi yang menggunakan `Free` karena ukuran objek yang disimpan
A> memang lebih kecil pada *stack*.


## Pustaka Transformator Monad

Transformator monad merupakan struktur data yang membungkus nilai yang mendasari
dan menyediakan efek monadik.

Sebagai contoh, pada bab 2 kita menggunakan `OptionT` agar kita dapat menggunakan
`F[Option[A]]` pada komprehensi `for` sebagaimana kita menggunakan
`F[A]`. Hal semacam ini menambahkan efek dari nilai opsional pada program kita.
Atau bisa juga kita menggunakan `MonadPlus` untuk mendapatkan efek yang sama.

Subset tipe data ini dan perpanjangan dari `Monad` biasa disebut sebagai
Pustaka Transformator Monad atau Monad Transformer Library (MTL) yang dirangkum
di bawah. Pada bagian ini, kita akan membahas tiap transformator, apa guna
mereka, dan bagaimana cara mereka bekerja.

| Efek               | Pendasaran            | Transformator | Kelas Tipe     |
|-------------------- |--------------------- |----------- |------------- |
| pilihan              | `F[Maybe[A]]`         | `MaybeT`    | `MonadPlus`   |
| galat                | `F[E \/ A]`           | `EitherT`   | `MonadError`  |
| nilai waktu jalan    | `A => F[B]`           | `ReaderT`   | `MonadReader` |
| jurnal / tugas ganda | `F[(W, A)]`           | `WriterT`   | `MonadTell`   |
| perubahan kondisi    | `S => F[(S, A)]`      | `StateT`    | `MonadState`  |
| jalan terus saja     | `F[E \&/ A]`          | `TheseT`    |               |
| kontrol alur         | `(A => F[B]) => F[B]` | `ContT`     |               |


### `MonadTrans`

Tiap transformator mempunyai bentuk umum `T[F[_], A]`, dan menyediakan setidaknya
satu instans `Monad` dan `Hoist` sehingga disebut `MonadTrans`:

{lang="text"}
~~~~~~~~
  @typeclass trait MonadTrans[T[_[_], _]] {
    def liftM[F[_]: Monad, A](a: F[A]): T[F, A]
  }
  
  @typeclass trait Hoist[F[_[_], _]] extends MonadTrans[F] {
    def hoist[M[_]: Monad, N[_]](f: M ~> N): F[M, ?] ~> F[N, ?]
  }
~~~~~~~~

A> `T[_[_], _]` merupakan contoh lain dari jenis tipe tinggi (lol, kill me mate).
A> Penanda tipe ini dibaca sebagai: `T` menerima dua parameter tipe, yang pertama
A> juga menerima sebuah parameter tipe, yang ditulis sebagai `_[_]` dan yang kedua
A> tidak menerima parameter tipe apapun yang ditulis dengan `_`.

`.liftM` memperkenankan kita untuk membuat sebuah transformator monad bila kita
mempunyai sebuah `F[A]`. Sebagai contoh, kita dapat membuat sebuah `OptionT[IO, String]`
dengan memanggil `.liftM[OptionT]` pada sebuah `IO[String].

Mirip dengan `.liftM`, `.hoist` digunakan untuk transformasi natural.

Secara umum, ada tiga cara untuk membuat sebuah transformator monad:

-   dengan menggunakan konstruktor transformator
-   dari sebuah nilai `A` dengan menggunakan `.pure` dari sintaks `Monad`
-   dari sebuah `F[A] dengan menggunakan `.liftM` dari sintaks `MonadTrans`

Dikarenakan cara kerja dari penebak tipe pada Scala, sering kali parameter tipe
yang kompleks harus tersurat. Untuk menyiasati hal ini, transformator biasanya
menyediakan alat bantu konstruktor pada pasangan, sehingga dapat dengan mudah
digunakan.


### `MaybeT`

`OptionT`, `MaybeT`, dan `LazyOptionT` mempunyai implementasi yang mirip.
Mereka sama sama menyediakan opsionalitas melalui `Option`, `Maybe`, dan `LazyOption`.
Kita akan fokus pada `MaybeT` untuk menghindari pengulangan pembahasan.

{lang="text"}
~~~~~~~~
  final case class MaybeT[F[_], A](run: F[Maybe[A]])
  object MaybeT {
    def just[F[_]: Applicative, A](v: =>A): MaybeT[F, A] =
      MaybeT(Maybe.just(v).pure[F])
    def empty[F[_]: Applicative, A]: MaybeT[F, A] =
      MaybeT(Maybe.empty.pure[F])
    ...
  }
~~~~~~~~

menyediakan sebuah instans untuk `MonadPlus`

{lang="text"}
~~~~~~~~
  implicit def monad[F[_]: Monad] = new MonadPlus[MaybeT[F, ?]] {
    def point[A](a: =>A): MaybeT[F, A] = MaybeT.just(a)
    def bind[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] =
      MaybeT(fa.run >>= (_.cata(f(_).run, Maybe.empty.pure[F])))
  
    def empty[A]: MaybeT[F, A] = MaybeT.empty
    def plus[A](a: MaybeT[F, A], b: =>MaybeT[F, A]): MaybeT[F, A] = a orElse b
  }
~~~~~~~~

Monad ini memang terlihat agak janggal. Namun, monad ini hanya mendelegasi
semuanya ke `Monad[F]` dan pada akhirnya membungkus ulang dengan sebuah `MaybeT`.
Hal ini yang disebut dengan pertukangan.

Dengan monad ini, kita dapat menulis logika yang menangani opsionalitas pada
konteks `F[]`, tidak dengan membawa-bawa `Option` maupun `Maybe`.

Sebagai contoh, misalkan kita menggunakan sebuah situs media sosial untuk menghitung
jumlah bintang yang dimiliki oleh seorang pengguna. Situs tersebut memberikan sebuah
`String` yang mungkin bisa berisi informasi tentang pengguna dan bisa juga tidak.
Selain itu, kita memilki aljabar berikut:

{lang="text"}
~~~~~~~~
  trait Twitter[F[_]] {
    def getUser(name: String): F[Maybe[User]]
    def getStars(user: User): F[Int]
  }
  def T[F[_]](implicit t: Twitter[F]): Twitter[F] = t
~~~~~~~~

Kita harus memanggil `getUser` dan dilanjutkan dengan `getStars`. Bila kita
menggunakan `Monad` sebagai konteks dari pemanggilan ini, kita akan kesulitan
menulis fungsi untuk ini karena kita harus menangani kondisi `Empty`:

{lang="text"}
~~~~~~~~
  def stars[F[_]: Monad: Twitter](name: String): F[Maybe[Int]] = for {
    maybeUser  <- T.getUser(name)
    maybeStars <- maybeUser.traverse(T.getStars)
  } yield maybeStars
~~~~~~~~

Namun, bila kita mempunyai sebuah `MonadPlus` sebagai konteks, kita dapat
memasukkan `Maybe` ke dalam `F[]` dengan `.orEmpty` dan mengabaikan apa yang
terjadi selanjutnya:

{lang="text"}
~~~~~~~~
  def stars[F[_]: MonadPlus: Twitter](name: String): F[Int] = for {
    user  <- T.getUser(name) >>= (_.orEmpty[F])
    stars <- T.getStars(user)
  } yield stars
~~~~~~~~

Namun, dengan menambahkan persyaratan `MonadPlus`, akan muncul permasalah
bila konteks hilir tidak mempunyai instans monad tersebut. Solusi yang bisa
digunakan adalah antara mengganti konteks menjadi `MaybeT[F, ?]` (mengangkat
`Monad[F]` menjadi `MonadPlus`), atau secara tersurat menggunakan `MaybeT` pada
tipe kembalian, walaupun harus menulis kode sedikit lebih banyak:

{lang="text"}
~~~~~~~~
  def stars[F[_]: Monad: Twitter](name: String): MaybeT[F, Int] = for {
    user  <- MaybeT(T.getUser(name))
    stars <- T.getStars(user).liftM[MaybeT]
  } yield stars
~~~~~~~~

Keputusan untuk menggunakan `Monad` atau mengembalikan sebuah transformator
pada akhirnya merupakan hal yang harus diputuskan oleh tim pembaca yang budiman
berdasarkan pada interpreter yang digunakan pada program pembaca budiman.


### `EitherT`


Nilai opsional merupakan sebuah kasus khusus dimana sebuah nilai bisa saja
berupa sebuah galat, namun kita tidak tahu apapun mengenai galat tersebut.
`EitherT` (dan varian lundungnya, `LazyEitherT`) memperkenankan kita untuk
menggunakan tipe apapun yang kita inginkan sebagai nilai galat beserta
menyediakan informasi kontekstual mengenai apa yang salah. 

`EitherT` merupakan pembungkus atas sebuah `F[A \/ B]`

{lang="text"}
~~~~~~~~
  final case class EitherT[F[_], A, B](run: F[A \/ B])
  object EitherT {
    def either[F[_]: Applicative, A, B](d: A \/ B): EitherT[F, A, B] = ...
    def leftT[F[_]: Functor, A, B](fa: F[A]): EitherT[F, A, B] = ...
    def rightT[F[_]: Functor, A, B](fb: F[B]): EitherT[F, A, B] = ...
    def pureLeft[F[_]: Applicative, A, B](a: A): EitherT[F, A, B] = ...
    def pure[F[_]: Applicative, A, B](b: B): EitherT[F, A, B] = ...
    ...
  }
~~~~~~~~

`Monad` pada konteks berikut adalah sebuah `MonadError`

{lang="text"}
~~~~~~~~
  @typeclass trait MonadError[F[_], E] extends Monad[F] {
    def raiseError[A](e: E): F[A]
    def handleError[A](fa: F[A])(f: E => F[A]): F[A]
  }
~~~~~~~~

`.raiseError` dan `.handleError` cukup jelas: keduanya ekuivalen dengan metoda
`throw` dan `catch` sebuah galat.

`MonadError` mempunyai beberapa sintaks tambahan untuk menangani masalah-masalah
umum:

{lang="text"}
~~~~~~~~
  implicit final class MonadErrorOps[F[_], E, A](self: F[A])(implicit val F: MonadError[F, E]) {
    def attempt: F[E \/ A] = ...
    def recover(f: E => A): F[A] = ...
    def emap[B](f: A => E \/ B): F[B] = ...
  }
~~~~~~~~

`.attempt` mengubah galat menjadi nilai, yang berguna untuk menampakkan galat
pada subsistem sebagai nilai utama.

`.recover` digunakan untuk mengubah sebuah galat menjadi nilai untuk semua
kasus yang mungkin terjadi. Sebaliknya, `.handleError` menerima sebuah
`F[A]` dan pada akhirnya memperkenankan pemulihan sebagian.

`.emap`, yang merupakan pemetaan atas *either*, mengaplikasikan transformasi
yang bisa saja gagal.

`MonadError` untuk `EitherT` adalah:

{lang="text"}
~~~~~~~~
  implicit def monad[F[_]: Monad, E] = new MonadError[EitherT[F, E, ?], E] {
    def monad[F[_]: Monad, E] = new MonadError[EitherT[F, E, ?], E] {
    def bind[A, B](fa: EitherT[F, E, A])
                  (f: A => EitherT[F, E, B]): EitherT[F, E, B] =
      EitherT(fa.run >>= (_.fold(_.left[B].pure[F], b => f(b).run)))
    def point[A](a: =>A): EitherT[F, E, A] = EitherT.pure(a)
  
    def raiseError[A](e: E): EitherT[F, E, A] = EitherT.pureLeft(e)
    def handleError[A](fa: EitherT[F, E, A])
                      (f: E => EitherT[F, E, A]): EitherT[F, E, A] =
      EitherT(fa.run >>= {
        case -\/(e) => f(e).run
        case right => right.pure[F]
      })
  }
~~~~~~~~

Seharusnya bukan hal yang mengejutkan bila kita dapat menulis ulang contoh
dari `MonadPlus` dengan menggunakan `MonadError` dan menyisipkan pesan galat
yang informatif:


{lang="text"}
~~~~~~~~
  def stars[F[_]: Twitter](name: String)
                          (implicit F: MonadError[F, String]): F[Int] = for {
    user  <- T.getUser(name) >>= (_.orError(s"user '$name' not found")(F))
    stars <- T.getStars(user)
  } yield stars
~~~~~~~~

dimana `.orError` merupakan metoda bantuan pada `Maybe`

{lang="text"}
~~~~~~~~
  sealed abstract class Maybe[A] {
    ...
    def orError[F[_], E](e: E)(implicit F: MonadError[F, E]): F[A] =
      cata(F.point(_), F.raiseError(e))
  }
~~~~~~~~

A> It is also common practice to name the implicit parameter after the primary
A> type, in this case `F`.
A>
A> Juga hal yang umum untuk menggunakan parameter blok `implicit` setelah
A> tipe utama yang pada hal ini adalah `F`. 

Versi yang menggunakan `EitherT` terlihat sebagai berikut

{lang="text"}
~~~~~~~~
  def stars[F[_]: Monad: Twitter](name: String): EitherT[F, String, Int] = for {
    user <- EitherT(T.getUser(name).map(_ \/> s"user '$name' not found"))
    stars <- EitherT.rightT(T.getStars(user))
  } yield stars
~~~~~~~~

Instans paling sederhana dari `MonadError` adalah `\/` yang sangat cocok untuk
testing logika bisnis yang membutuhkan sebuah `MonadError`. Sebagai contoh,

{lang="text"}
~~~~~~~~
  final class MockTwitter extends Twitter[String \/ ?] {
    def getUser(name: String): String \/ Maybe[User] =
      if (name.contains(" ")) Maybe.empty.right
      else if (name === "wobble") "connection error".left
      else User(name).just.right
  
    def getStars(user: User): String \/ Int =
      if (user.name.startsWith("w")) 10.right
      else "stars have been replaced by hearts".left
  }
~~~~~~~~

Tes unit kita untuk `.stars` mungkin mencakup hal berikut:

{lang="text"}
~~~~~~~~
  scala> stars("wibble")
  \/-(10)
  
  scala> stars("wobble")
  -\/(connection error)
  
  scala> stars("i'm a fish")
  -\/(user 'i'm a fish' not found)
  
  scala> stars("fommil")
  -\/(stars have been replaced by hearts)
~~~~~~~~

Sebagaimana yang telah kita saksikan beberapa kali, kita dapat fokus pada testing
untuk logika bisnis seutuhnya.

Dan pada akhirnya, kita kembali ke aljabar `JsonClient` pada bab 4.3

{lang="text"}
~~~~~~~~
  trait JsonClient[F[_]] {
    def get[A: JsDecoder](
      uri: String Refined Url,
      headers: IList[(String, String)]
    ): F[A]
    ...
  }
~~~~~~~~

harap diingat bahwa kita hanya menulis jalur lancar pada API. Bila interpreter
kita untuk aljabar ini hanya bekerja pada `F` yang memiliki `MonadError`, kita
dapat mendefinisikan jenis galat sebagai permasalahan yang berhubungan.
Dan memang pada kenyataannya, kita dapat mempunyai **dua** lapis galat bila kita
mendefinisikan interpreter untuk sebuah `EitherT[IO, JsonClient.Error, ?]` 

{lang="text"}
~~~~~~~~
  object JsonClient {
    sealed abstract class Error
    final case class ServerError(status: Int)       extends Error
    final case class DecodingError(message: String) extends Error
  }
~~~~~~~~

Yang mencakup masalah I/O, status peladen, dan masalah masalah pada pemodelan
dari muatan JSON dari peladen.


#### Memilih Tipe Galat

Komunitas Scalaz masih belum dapat menyimpulkan mengenai strategi terbaik
untuk tipe galat `E` di `MonadError`.

Salah satu mahzab berpendapat bahwa kita harus memilih yang umum, seperti `String`.
Mahzab lain berpendapat bahwa sebuah aplikasi harus mempunyai ADT untuk galat
yang memperkenankan penanganan galat yang disesuaikan. Kaum air di daun talas
sendiri lebih memilih untuk menggunakan `Throwable` demi kompatibilitas penuh
atas `JVM`.

Ada dua masalah yang muncul bila kita menggunakan ADT galat pada tingkat
aplikasi:

-   sangat canggung bila kita membuat sebuah galat baru. Satu berkas menjadi
    lumbung galat utama, mengagregasi galat dari semua subsismet.
-   tidak peduli betapa granular galat yang ada, resolusi yang dipakai cenderung sama:
    catat galat tersebut lalu coba lagi atau berhenti. Kita tidak perlu ADT untuk
    hal semacam ini.

Sebuah ADT galat menjadi berguna bila tiap catatan menerima penanganan pemulihan
yang berbeda.

Sebuah kompromi antara galat ADT dan `String` adalah format pertengahan.
JSON merupakan pilihan yang bagus karena format ini dipahami oleh kebanyakan
*framework* pengawasan dan pencatatan log.

Masalah yang muncul bila kita tidak memiliki *stacktrace* adalah sulitnya
mencari kode yang menjadi sumber galat. Dengan [`sourcecode` oleh Li Haoyi](https://github.com/lihaoyi/sourcecode/),
kita dapat mengikutsertakan informasi kontekstual sebagai metadata pada galat kita:

{lang="text"}
~~~~~~~~
  final case class Meta(fqn: String, file: String, line: Int)
  object Meta {
    implicit def gen(implicit fqn: sourcecode.FullName,
                              file: sourcecode.File,
                              line: sourcecode.Line): Meta =
      new Meta(fqn.value, file.value, line.value)
  }
  
  final case class Err(msg: String)(implicit val meta: Meta)
~~~~~~~~

Walau `Err` dapat dirujuk secara transparan, konstruksi implisit dari sebuah
`Meta` secara sekilas tidak terlihat bisa dirujuk secara transparan bila
dibaca seperti biasa: dua panggilan ke `Meta.gen` (dipanggil secara implisit
saat membuat sebuah `Err`) akan menghasilkan nilai yang berbeda karena lokasi
dari kode sumber berhubungan dengan nilai yang dikembalikan:

{lang="text"}
~~~~~~~~
  scala> println(Err("hello world").meta)
  Meta(com.acme,<console>,10)
  
  scala> println(Err("hello world").meta)
  Meta(com.acme,<console>,11)
~~~~~~~~

Untuk memahami hal ini, kita harus mengapresiais bahwa metoda `sourcecode.*`
merupakan makro yang menggenerasi kode sumber untuk kita. Bila kita harus menulis
kode di atas secara eksplisit, apa yang terjadi akan menjadi jelas:

{lang="text"}
~~~~~~~~
  scala> println(Err("hello world")(Meta("com.acme", "<console>", 10)).meta)
  Meta(com.acme,<console>,10)
  
  scala> println(Err("hello world")(Meta("com.acme", "<console>", 11)).meta)
  Meta(com.acme,<console>,11)
~~~~~~~~

Betul, kita sudah bersekutu dengan iblis makro, namun kita dapat menulis `Meta`
secara manual.


### `ReaderT`

Monad pembaca membungkus `A => F[B]` sehingga memperkenankan program `F[B]` untuk
bergantung kepada nilai waktu-jalan `A`. Bagi pembaca yang sudah akrab dengan
penyuntikan dependensi (dependency injection), monad pembaca ekuivalen dengan
anotasi `@Inject` milik Spring maupun Guice. Namun, tanpa disertai dengan refleksi
maupun XML.

`ReaderT` hanya merupakan alias untuk tipe data yang lebih umum yang dinamai
berdasarkan matematikawan *Heinrich Kleisli*.

{lang="text"}
~~~~~~~~
  type ReaderT[F[_], A, B] = Kleisli[F, A, B]
  
  final case class Kleisli[F[_], A, B](run: A => F[B]) {
    def dimap[C, D](f: C => A, g: B => D)(implicit F: Functor[F]): Kleisli[F, C, D] =
      Kleisli(c => run(f(c)).map(g))
  
    def >=>[C](k: Kleisli[F, B, C])(implicit F: Bind[F]): Kleisli[F, A, C] = ...
    def >==>[C](k: B => F[C])(implicit F: Bind[F]): Kleisli[F, A, C] = this >=> Kleisli(k)
    ...
  }
  object Kleisli {
    implicit def kleisliFn[F[_], A, B](k: Kleisli[F, A, B]): A => F[B] = k.run
    ...
  }
~~~~~~~~

A> Beberapa orang menyebut `>=>` sebagai operator ikan. Tentu selalu ada ikan
A> yang lebih besar, seperti `>==>`. Operator itupun juga disebut sebagai
A> panah Kleisli.

Konversi `implicit` pada objek pendamping memperkenankan kita untuk menggunakan
sebuah `Kleisli` pada bagian yang seharusnya menjadi tempat untuk sebuah fungsi.
Hal ini memperkenankan kita untuk menggunakan struktur data ini sebagai
parameter pada `.bind` atau `>>=` dari sebuah monad.

Penggunaan paling jamak untuk `ReaderT` adalah sebagai penyedia informasi lingkungan
jalan untuk sebuah progarm. Pada `drone-dynamic-agents`, kita membutuhkan akses
untuk OAuth 2.0 Refresh Token milik pengguna agar dapat menghubungi Google.
Tentu hal yang paling mudah dilakukan adalah memuat informasi tersebut dari diska
dan membuat tiap metoda menerima sebuah parameter `RefreshToken`. Bahkan,
hal semacam ini merupakan persyaratan umum yang diajukan oleh Martin Odersky
pada proposal [implicit function](https://www.scala-lang.org/blog/2016/12/07/implicit-function-types.html).

Sebuah solusi yang lebih jitu untuk program kita adalah dengan membuat sebuah
aljabar yang menyediakan konfigurasi saat dibutuhkan. Misalnya,

{lang="text"}
~~~~~~~~
  trait ConfigReader[F[_]] {
    def token: F[RefreshToken]
  }
~~~~~~~~

Kita sudah membuat ulang `MonadReader`, kelas tipe yang berhubungan dekat dengan
`ReaderT`, dimana `.ask` sama dengan `.token` pada potongan diatas, dan `S` sebagai
`RefreshToken`:

{lang="text"}
~~~~~~~~
  @typeclass trait MonadReader[F[_], S] extends Monad[F] {
    def ask: F[S]
  
    def local[A](f: S => S)(fa: F[A]): F[A]
  }
~~~~~~~~

dengan implmentasi

{lang="text"}
~~~~~~~~
  implicit def monad[F[_]: Monad, R] = new MonadReader[Kleisli[F, R, ?], R] {
    def point[A](a: =>A): Kleisli[F, R, A] = Kleisli(_ => F.point(a))
    def bind[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]) =
      Kleisli(a => Monad[F].bind(fa.run(a))(f))
  
    def ask: Kleisli[F, R, R] = Kleisli(_.pure[F])
    def local[A](f: R => R)(fa: Kleisli[F, R, A]): Kleisli[F, R, A] =
      Kleisli(f andThen fa.run)
  }
~~~~~~~~

Hukum dari `MonadReader` adalah `S` tidak boleh berubah diantara tiap pemanggilan.
Sebagai contoh, `ask >> ask === ask`. Untuk penggunaan `MonadReader` pada program
kita, kita hanya perlu membaca konfigurasi kita satu kali saja. Bila kita ingin
memuat ulang konfigurasi tiap kali kita membutuhkannya, misalkan agar kita dapat
mengubah token tanpa harus menjalankan ulang aplikasi, kita dapat memperkenalkan
`ConfigReader` yang tidak mempunyai hukum semacam ini.

Pada implementasi OAuth 2.0 kita, kita dapat memindah `Monad` ke metoda:

{lang="text"}
~~~~~~~~
  def bearer(refresh: RefreshToken)(implicit F: Monad[F]): F[BearerToken] =
    for { ...
~~~~~~~~

lalu dilanjutkan dengan melakukan refaktorisasi parameter `refresh` agar
menjadi bagian dari `Monad`

{lang="text"}
~~~~~~~~
  def bearer(implicit F: MonadReader[F, RefreshToken]): F[BearerToken] =
    for {
      refresh <- F.ask
~~~~~~~~

Tiap parameter dapat dipindahkan ke `MonadReader`. Yang paling penting untuk pemanggil
adalah saat pemanggil hanya perlu untuk menelisik infromsai ini dari hierarki
pemanggilan paling atas. Dengan `ReaderT`, kita tidak perlu menggunakan blok
parameter `implicit` sehingga mengurang beban mental saat menggunakan Scala.

Metoda lain pada `MonadReader` adalah `.local`

{lang="text"}
~~~~~~~~
  def local[A](f: S => S)(fa: F[A]): F[A]
~~~~~~~~

Kita dapat mengubah `S` dan menjalankan sebuah program `fa` delam konteks lokal
tersebut dan mengembalikan `S` asli. Contoh penggunaan `.local` adalah saat
membuat "stack trace" yang sesuai untuk domain kita, pencatatan log berlapis!
Sebagaimana pada struktur data `Meta` pada bab sebelumnya, kita mendefinisikan
sebuah fungsi pada titik pemeriksaan:

{lang="text"}
~~~~~~~~
  def traced[A](fa: F[A])(implicit F: MonadReader[F, IList[Meta]]): F[A] =
    F.local(Meta.gen :: _)(fa)
~~~~~~~~

dan kita dapat menggunakannya untuk membungkus fungsi yang beroperasi pada
konteks ini.

{lang="text"}
~~~~~~~~
  def foo: F[Foo] = traced(getBar) >>= barToFoo
~~~~~~~~

akan lolos secara otomatis untuk semua yang tidak ditentukan sebelumnya.
Sebuah tambahan kompilasi atau sebuah makro dapat melakukan hal yang sebaliknya,
memaksa untuk memilih semuanya.

Bila kita mengakses `.ask`, kita dapat melihat jejak langkah bagaimana kita
dipanggil, tanpa harus dikaburkan oleh detail implementasi bytecode.
Hal ini merupakan contoh dari *stack trace* yang dirujuk secara transparan.

Pengembang yang memilih untuk bermain aman mungkin berharap untuk memecah `IList[Meta]`
pada ukuran tertentu untuk menghindari sesuatu yang mirip dengan *stack overflow*.
Dan memang pada kenyataannya, struktu data yang cocok adalah `Dequeue`.

`.local` juga dapat digunakan untuk mencatat informasi kontekstual yang relevan
secara langsung pada tugas saat itu, seperti jumlah spasi yang harus digunakan
untuk melekuk sebuah baris saat mencetak format berkas yang dapat dibaca manusia
dengan mudah. Misal, menambah dua spasi ketika kita memasuki sebuah struktur
berlapis.

A> Bukan empat spasi. Bukan delapan spasi. Bukan TAB.
A>
A> Dua spasi. Pas dua spasi. Ini satu-satunya angka yang bisa kita gunakan
A> secara langsung karena angka lain adalah **sesat**!

Dan paling penting, bila kita tidak dapat meminta sebuah `MonadReader` karena
aplikasi kita tidak menyediakannya, kita dapat mengembalikan sebuah `ReaderT`

{lang="text"}
~~~~~~~~
  def bearer(implicit F: Monad[F]): ReaderT[F, RefreshToken, BearerToken] =
    ReaderT( token => for {
    ...
~~~~~~~~

Bila sebuah pemanggil menerima `ReaderT` dan mereka mempunyai parameter `token`,
mereka dapat memanggil `access.run(token)` dan mendapatkan sebuah `F[BearerToken]`.

Terus terang, karena kita tidak mempunyai banyak pemanggil, kita hanya perlu mengubah
sebuah parameter fungsi. `MonadReader` paling berguna saat:

1.  kita ingin melakukan refaktor (lol, help) kode suatu saat untuk memuat ulang
    konfigurasi
2.  nilai tidak dibutuhkan oleh pemanggil perantara
3.  atau kita ingin menentukan cakupan beberapa variabel secara lokal

Dotty boleh saja tetap menggunakan fungsi implisit, karena kita mempunyai `ReaderT`
dan `MonadReader`.


### `WriterT`

Yang menjadi kebalikan dari pembacaan adalah penulisan nilai. Transformator monad
`WriterT` biasanya digunakan untuk menulis ke sebuah jurnal.

{lang="text"}
~~~~~~~~
  final case class WriterT[F[_], W, A](run: F[(W, A)])
  object WriterT {
    def put[F[_]: Functor, W, A](value: F[A])(w: W): WriterT[F, W, A] = ...
    def putWith[F[_]: Functor, W, A](value: F[A])(w: A => W): WriterT[F, W, A] = ...
    ...
  }
~~~~~~~~

Tipe yang dibungkus adalah `F[(W, A)]` dengan jurnal yang terakumulasi pada `W`.

Tidak hanya satu monad yang berhubungan dengan `WriterT`, namun ada 2.
`MonadTell` dan `MonadListen`

{lang="text"}
~~~~~~~~
  @typeclass trait MonadTell[F[_], W] extends Monad[F] {
    def writer[A](w: W, v: A): F[A]
    def tell(w: W): F[Unit] = ...
  
    def :++>[A](fa: F[A])(w: =>W): F[A] = ...
    def :++>>[A](fa: F[A])(f: A => W): F[A] = ...
  }
  
  @typeclass trait MonadListen[F[_], W] extends MonadTell[F, W] {
    def listen[A](fa: F[A]): F[(A, W)]
  
    def written[A](fa: F[A]): F[W] = ...
  }
~~~~~~~~

`MonadTell` digunakan untuk menulis pada jurnal sedangkan `MonadListen` digunakan
untuk memperoleh nilai yang sudah ditulis. Implementasi dari `WriterT` adalah
sebagai berikut

{lang="text"}
~~~~~~~~
  implicit def monad[F[_]: Monad, W: Monoid] = new MonadListen[WriterT[F, W, ?], W] {
    def point[A](a: =>A) = WriterT((Monoid[W].zero, a).point)
    def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = WriterT(
      fa.run >>= { case (wa, a) => f(a).run.map { case (wb, b) => (wa |+| wb, b) } })
  
    def writer[A](w: W, v: A) = WriterT((w -> v).point)
    def listen[A](fa: WriterT[F, W, A]) = WriterT(
      fa.run.map { case (w, a) => (w, (a, w)) })
  }
~~~~~~~~

Contoh paling jelas adalah dengan menggunakan `MonadTell` untuk pencatatan log
ataupun pelaporan audit. Dengan menggunakan ulang `Meta` dari pelaporan galat,
kita dapat membayangkan untuk membuat struktur log sebagai berikut


{lang="text"}
~~~~~~~~
  sealed trait Log
  final case class Debug(msg: String)(implicit m: Meta)   extends Log
  final case class Info(msg: String)(implicit m: Meta)    extends Log
  final case class Warning(msg: String)(implicit m: Meta) extends Log
~~~~~~~~

dan menggunakan `Dequeue[Log]` sebagai tipe jurnal kita. Kita dapat mengganti
metoda `authenticate` OAuth2 kita menjadi

{lang="text"}
~~~~~~~~
  def debug(msg: String)(implicit m: Meta): Dequeue[Log] = Dequeue(Debug(msg))
  
  def authenticate: F[CodeToken] =
    for {
      callback <- user.start :++> debug("started the webserver")
      params   = AuthRequest(callback, config.scope, config.clientId)
      url      = config.auth.withQuery(params.toUrlQuery)
      _        <- user.open(url) :++> debug(s"user visiting $url")
      code     <- user.stop :++> debug("stopped the webserver")
    } yield code
~~~~~~~~

Kita juga bisa menggabungkannya dengan bekas jejak dari `ReaderT` untuk mendapatkan
log terstruktur.

Pemanggil dapat mengembalikan log dengan menggunakan `.written` dan bebas melakukan
apapun dengannya.

Namun, ada sebuah argumen kuat yang menyatakan bahwa pencatatan log berhak mendapatkan
aljabarnya sendiri. Pembagian tingkat log seringkali dibutuhkan dengan alasan performa.
Dan sering kali, penulisan log dilakukan pada tingkat aplikasi, bukan pada komponen.

`W` pada `WriterT` mempunyai sebuah `Monoid` yang memperkenankan kita untuk
mencatat semua jenis kalkulasi *monoidik* sebagai nilai sekunder bersamaan dengan
program utama kita. Sebagai contoh, menghitung berapa kali kita melakukan sesuatu,
membangun sebuah penjelasan dari sebuah kalkulasi, ataupun membangun sebuah
`TradeTemplate` untuk *trade* (lol, help) baru saat kita menakar harganya.

Spesialisasi yang populer dari `WriterT` adalah saat monad yang digunakan adalah
`Id`, yang juga berarti bahwa nilai `run` yang melandasinya hanyalah merupakan
sebuah tuple sederhana `(W, A)`.

{lang="text"}
~~~~~~~~
  type Writer[W, A] = WriterT[Id, W, A]
  object WriterT {
    def writer[W, A](v: (W, A)): Writer[W, A] = WriterT[Id, W, A](v)
    def tell[W](w: W): Writer[W, Unit] = WriterT((w, ()))
    ...
  }
  final implicit class WriterOps[A](self: A) {
    def set[W](w: W): Writer[W, A] = WriterT(w -> self)
    def tell: Writer[A, Unit] = WriterT.tell(self)
  }
~~~~~~~~

yang memperkenankan kita agar nilai apapun dapat membawa kalkulasi monoidal kedua
tanpa harus membutuhkan konteks `F[_]`.

Singkat kata, `WriterT` / `MonadTell` merupakan cara untuk melakukan tugas-ganda
pada pemrograman fungsional.


### `StateT`

`StateT` memperkenankan kita untuk melakukan `.put`, `.get`, dan `.modify` pada
sebuah nilai yang sedang ditangani pada konteks monadik. Monad ini merupakan
pengganti `var` pada pemrograman fungsional.

Bila kita harus menulis sebuah metoda tak murni (lol, help) yang mempunyai
akses ke beberapa kondisi yang tidak tetap dan disimpan pada sebuah `var`, metoda
ini mungkin mempunyai penanda `() => F[A]` dan mengembalikan nilai yang berbeda
pada tiap kali pemanggilan dan pada akhirnya mengaburkan perujukan. Dengan
pemrograman fungsional murni (lol, help), fungsi tersebut menerima sebuah keadaan
(*state*) sebagai masukan dan mengembalikan keadaan yang termutakhirkan sebagai
keluaran. Ini-lah yang menjadi pendasaran mengapa tipe dasar dari `StateT` adalah
`S => F[(S, A)]`.

Monad yang terasosiasi dengan `StateT` adalah `MonadState`

{lang="text"}
~~~~~~~~
  @typeclass trait MonadState[F[_], S] extends Monad[F] {
    def put(s: S): F[Unit]
    def get: F[S]
  
    def modify(f: S => S): F[Unit] = get >>= (s => put(f(s)))
    ...
  }
~~~~~~~~

A> `S` harus berupa tipe tak berubah: `.modify` bukan pintu darurat untuk memutakhirkan
A> sebuah struktur tak tetap. Ketidak-tetapan itu tidak murni dan hanya diperkenankan
A> pada blok `IO`.

`StateT` diimplementasikan sedikit berbeda dengan transformator monad yang sudah
kita pelajari sampai saat ini. `StateT` bukan berupa `case class`, namun merupakan
sebuah ADT yang berisi dua anggota:

{lang="text"}
~~~~~~~~
  sealed abstract class StateT[F[_], S, A]
  object StateT {
    def apply[F[_], S, A](f: S => F[(S, A)]): StateT[F, S, A] = Point(f)
  
    private final case class Point[F[_], S, A](
      run: S => F[(S, A)]
    ) extends StateT[F, S, A]
    private final case class FlatMap[F[_], S, A, B](
      a: StateT[F, S, A],
      f: (S, A) => StateT[F, S, B]
    ) extends StateT[F, S, B]
    ...
  }
~~~~~~~~

yang merupakan bentuk khusus dari `Trampoline` dan memberikan kita keamanan
*stack* bila kita ingin mengembalikan struktur data standar dengan `.run`:

{lang="text"}
~~~~~~~~
  sealed abstract class StateT[F[_], S, A] {
    def run(initial: S)(implicit F: Monad[F]): F[(S, A)] = this match {
      case Point(f) => f(initial)
      case FlatMap(Point(f), g) =>
        f(initial) >>= { case (s, x) => g(s, x).run(s) }
      case FlatMap(FlatMap(f, g), h) =>
        FlatMap(f, (s, x) => FlatMap(g(s, x), h)).run(initial)
    }
    ...
  }
~~~~~~~~

`StateT` dapat dengan mudah mengimplementasikan `MonadState` dengan ADT-nya:

{lang="text"}
~~~~~~~~
  implicit def monad[F[_]: Applicative, S] = new MonadState[StateT[F, S, ?], S] {
    def point[A](a: =>A) = Point(s => (s, a).point[F])
    def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]) =
      FlatMap(fa, (_, a: A) => f(a))
  
    def get       = Point(s => (s, s).point[F])
    def put(s: S) = Point(_ => (s, ()).point[F])
  }
~~~~~~~~

With `.pure` mirrored on the companion as `.stateT`:

{lang="text"}
~~~~~~~~
  object StateT {
    def stateT[F[_]: Applicative, S, A](a: A): StateT[F, S, A] = ...
    ...
  }
~~~~~~~~

dan `MonadTrans.liftM` menyediakan konstruktor `F[A] => StateT[F, S, A]`.

Varian umum dari `StateT` adalah saat `F = Id` yang memberikan tipe dasar sebagai
`S => (S, A)`. Scalaz menyediakan sebuah alias tipe dan fungsi pembantu untuk
berinteraksi dengan transformator monad `State` secara langsung, dan mencerminkan
`MonadState`:

{lang="text"}
~~~~~~~~
  type State[a] = StateT[Id, a]
  object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = StateT[Id, S, A](f)
    def state[S, A](a: A): State[S, A] = State((_, a))
  
    def get[S]: State[S, S] = State(s => (s, s))
    def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
    def modify[S](f: S => S): State[S, Unit] = ...
    ...
  }
~~~~~~~~

Sebagai contoh, kita dapat kembali ke tes logika bisnis dari `drone-dynamic-agents`.
Harap diingat kembali pada bab 3 kita telah membuat `Mutable` sebagai penerjemah
tes untuk aplikasi kita dan menyimpan perhitungan `started` dan `stoped` pada sebuah
`var`.

{lang="text"}
~~~~~~~~
  class Mutable(state: WorldView) {
    var started, stopped: Int = 0
  
    implicit val drone: Drone[Id] = new Drone[Id] { ... }
    implicit val machines: Machines[Id] = new Machines[Id] { ... }
    val program = new DynAgentsModule[Id]
  }
~~~~~~~~

Sekarang kita tahu bahwa kita dapat membuat simulator tes yang jauh lebih baik
dengan menggunakan `State`. Kita akan menggunakan kesempatan ini untuk meningkatkan
akurasi dari simulasi tersebut. Mohon diingat bahwa objek domain utama kita merupakan
pandangan aplikasi kita terhadap dunia luar:

{lang="text"}
~~~~~~~~
  final case class WorldView(
    backlog: Int,
    agents: Int,
    managed: NonEmptyList[MachineNode],
    alive: Map[MachineNode, Epoch],
    pending: Map[MachineNode, Epoch],
    time: Epoch
  )
~~~~~~~~

Karena kita menulis simulasi dari dunia luar untuk tes kita, kita dapat menulis
sebuah tipe data yang membawa nilai-nilai kebenaran untuk aplikasi kita

{lang="text"}
~~~~~~~~
  final case class World(
    backlog: Int,
    agents: Int,
    managed: NonEmptyList[MachineNode],
    alive: Map[MachineNode, Epoch],
    started: Set[MachineNode],
    stopped: Set[MachineNode],
    time: Epoch
  )
~~~~~~~~

A> Kita belum menulis ulang aplikasi kita untuk menggunakan tipe data dan kelas
A> tipe Scalaz sepenuhnya. Saat ini, kita masih bergantung pada pustaka koleksi
A> dari pustaka standar. Selain itu, tidak ada urgensi untuk menggantinya karena
A> masih sederhana dan tipe-tipe ini bisa digunakan dengan gaya pemrograman
A> fungsional murni. (lol, help. hard sentence.)

Pembeda utama adalah simpul `started` dan `stopped` dapat dipisahkan.
Penerjemah kita dapat diimplementasikan menggunakan `State[World, a]` dan kita
dapat menulis tes kita untuk memeriksa bagaimanakah bentuk dari `World` dan
`WorldView` setelah logika bisnis berjalan.

Penerjemah, yang meniru penghubungan layanan eksternal Drone dan Google, dapat
diimplementasikan seperti berikut:

{lang="text"}
~~~~~~~~
  import State.{ get, modify }
  object StateImpl {
    type F[a] = State[World, a]
  
    private val D = new Drone[F] {
      def getBacklog: F[Int] = get.map(_.backlog)
      def getAgents: F[Int]  = get.map(_.agents)
    }
  
    private val M = new Machines[F] {
      def getAlive: F[Map[MachineNode, Epoch]]   = get.map(_.alive)
      def getManaged: F[NonEmptyList[MachineNode]] = get.map(_.managed)
      def getTime: F[Epoch]                      = get.map(_.time)
  
      def start(node: MachineNode): F[Unit] =
        modify(w => w.copy(started = w.started + node))
      def stop(node: MachineNode): F[Unit] =
        modify(w => w.copy(stopped = w.stopped + node))
    }
  
    val program = new DynAgentsModule[F](D, M)
  }
~~~~~~~~

dan kita dapat menulis ulang tes kita agar mengikuti konvensi dimana:

-   `world1` merupakan keadaan dunia luar sebelum program berjalan
-   `view1` merupakan apa yang aplikasi kita ketahui tentang dunia luar
-   `world2` merupakan keadaan dunia luar setelah program berjalan
-   `view2` merupakan apa yang aplikasi kita ketahui tentang dunia luar setelah
    program berjalan

Sebagai contoh,

{lang="text"}
~~~~~~~~
  it should "request agents when needed" in {
    val world1          = World(5, 0, managed, Map(), Set(), Set(), time1)
    val view1           = WorldView(5, 0, managed, Map(), Map(), time1)
  
    val (world2, view2) = StateImpl.program.act(view1).run(world1)
  
    view2.shouldBe(view1.copy(pending = Map(node1 -> time1)))
    world2.stopped.shouldBe(world1.stopped)
    world2.started.shouldBe(Set(node1))
  }
~~~~~~~~

Mungkin akan dimaafkan bila kita melihat kembali ikalan logika bisnis kita

{lang="text"}
~~~~~~~~
  state = initial()
  while True:
    state = update(state)
    state = act(state)
~~~~~~~~

dan menggunakan `StateT` untuk mengatur `state`. Namun, logika bisnis `DynAgents`
kita hanya membutuhkan `Applicative` dan kita akan melanggar *Rule of Least Power*
yang meminta kuasa lebih dari `MonadState`. Jadi, cukup masuk akal bila kita
menangani keadaan secara manual dengan melemparnya secara langsung ke `update`
dan `act`, dan membiarkan siapapun yang ingin memanggil kita dengan menggunakan
`StateT`, bila itu yang mereka inginkan.


### `IndexedStateT`

Kode yang telah kita pelajari selama ini masih belum menunjukkan bagaimana
Scalaz mengimplementasikan `StateT`. Dan pada kenyataannya, `StateT` hanya
berupa alias tipe untuk `IndexedStateT`

{lang="text"}
~~~~~~~~
  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
~~~~~~~~

Implementasi dari `IndexedStateT` kurang lebih sama dengan dengan apa yang telah
kita pelajari sampai pada bab ini, dengan beberapa tambahan parameter tipe
yang memperbolehkan agar masukan `S1` dan keluaran `S2` berbeda: 

{lang="text"}
~~~~~~~~
  sealed abstract class IndexedStateT[F[_], -S1, S2, A] {
    def run(initial: S1)(implicit F: Bind[F]): F[(S2, A)] = ...
    ...
  }
  object IndexedStateT {
    def apply[F[_], S1, S2, A](
      f: S1 => F[(S2, A)]
    ): IndexedStateT[F, S1, S2, A] = Wrap(f)
  
    private final case class Wrap[F[_], S1, S2, A](
      run: S1 => F[(S2, A)]
    ) extends IndexedStateT[F, S1, S2, A]
    private final case class FlatMap[F[_], S1, S2, S3, A, B](
      a: IndexedStateT[F, S1, S2, A],
      f: (S2, A) => IndexedStateT[F, S2, S3, B]
    ) extends IndexedStateT[F, S1, S3, B]
    ...
  }
~~~~~~~~

`IndexedStateT` tidak mempunyai instans `MonadState` bila `S1 != S2`, walaupun
mempunyai `Monad`.

Contoh berikut diadaptasi dari presentasi [Index Your State](https://www.youtube.com/watch?v=JPVagd9W4Lo)
oleh Vincent Marquez. Bayangkan sebuah skenario dimana kita harus mendesain antarmuka
aljabaris untuk sebuah pencarian `String` berdasarkan sebuah `Int`. Antarmuka ini
bisa saja mempunyai implementasi yang berhubungan dengan implementasi lainnya
dan urutan panggilan sangat penting. Percobaan pertama kita mungkin akan terlihat
seperti berikut:

{lang="text"}
~~~~~~~~
  trait Cache[F[_]] {
    def read(k: Int): F[Maybe[String]]
  
    def lock: F[Unit]
    def update(k: Int, v: String): F[Unit]
    def commit: F[Unit]
  }
~~~~~~~~

dengan galat waktu-jalan bila `.update` atau `.commit` dipanggil tanpa sebuah
`.lock`. Desain yang lebih kompleks mungkin menggunakan beberapa *trait* dan
DSL khusus yang tidak ada yan mengingat tentangnya.

Atau, kita bisa menggunakan `IndexedStateT` yang memaksa pemanggil memang pada
tempat yang tepat. Pertama, kita mendefinisikan keadaan yang mungkin sebagai
sebuah ADT

{lang="text"}
~~~~~~~~
  sealed abstract class Status
  final case class Ready()                          extends Status
  final case class Locked(on: ISet[Int])            extends Status
  final case class Updated(values: Int ==>> String) extends Status
~~~~~~~~

dan memeriksa kembali aljabar kita

{lang="text"}
~~~~~~~~
  trait Cache[M[_]] {
    type F[in, out, a] = IndexedStateT[M, in, out, a]
  
    def read(k: Int): F[Ready, Ready, Maybe[String]]
    def readLocked(k: Int): F[Locked, Locked, Maybe[String]]
    def readUncommitted(k: Int): F[Updated, Updated, Maybe[String]]
  
    def lock: F[Ready, Locked, Unit]
    def update(k: Int, v: String): F[Locked, Updated, Unit]
    def commit: F[Updated, Ready, Unit]
  }
~~~~~~~~

yang akan memberikan galat waktu-kompilasi bila kita mencoba untuk melakukan
`.update` tanpa `.lock`

{lang="text"}
~~~~~~~~
  for {
        a1 <- C.read(13)
        _  <- C.update(13, "wibble")
        _  <- C.commit
      } yield a1
  
  [error]  found   : IndexedStateT[M,Locked,Ready,Maybe[String]]
  [error]  required: IndexedStateT[M,Ready,?,?]
  [error]       _  <- C.update(13, "wibble")
  [error]          ^
~~~~~~~~

namun memperkenankan kita untuk membuat fungsi yang dapat dikomposisi dengan
mengikutsertakannya secara tersurat:

{lang="text"}
~~~~~~~~
  def wibbleise[M[_]: Monad](C: Cache[M]): F[Ready, Ready, String] =
    for {
      _  <- C.lock
      a1 <- C.readLocked(13)
      a2 = a1.cata(_ + "'", "wibble")
      _  <- C.update(13, a2)
      _  <- C.commit
    } yield a2
~~~~~~~~

A> Kita memperkenalkan duplikasi kode pada API kita saat kita mendefinisikan
A> beberapa operasi `.read`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   def read(k: Int): F[Ready, Ready, Maybe[String]]
A>   def readLocked(k: Int): F[Locked, Locked, Maybe[String]]
A>   def readUncommitted(k: Int): F[Updated, Updated, Maybe[String]]
A> ~~~~~~~~
A>
A> Bukan
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   def read[S <: Status](k: Int): F[S, S, Maybe[String]]
A> ~~~~~~~~
A>
A> Alasan kita tidak mempergunakan kode tersebut adalah karena *subtyping*.
A> Kode ini bisa dikompilasi setelah menebak penanda tipe `F[Nothing, Ready, Maybe[String]]`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   for {
A>     a1 <- C.read(13)
A>     _  <- C.update(13, "wibble")
A>     _  <- C.commit
A>   } yield a1
A> ~~~~~~~~
A>
A> Scala mempunyai tipe `Nothing` yang merupakan subtipe dari semua tipe lainnya.
A> Untungnya, kode ini bisa digunakan pada waktu-jalan, karena tidak mungkin memanggilnya.
A> Selain itu, API ini merupakan API yang buruk karena pengguna harus mengingat
A> untuk menambah tipe tambahan.
A>
A> Pendekatan lain yang mungkin dilakukan adalah dengan memaksa kompilator untuk
A> tidak menebak `Nothing`. Scalaz menyediakan bukti tersirat untuk memeriksa
A> apakah sebuah tipe ditebak sebagai `Nothing` atau tidak. Kita akan menggunakan
A> bukti ini:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   def read[S <: Status](k: Int)(implicit NN: NotNothing[S]): F[S, S, Maybe[String]]
A> ~~~~~~~~
A>
A> Mana yang dipilih dari tiga alternatif API ini diserahkan kepada selera
A> perancang API sendiri. Dan ingat, Pria Punya Selera!


### `IndexedReaderWriterStateT`

Bagi pembaca yang menginginkan untuk menggabungkan `ReaderT`, `WriterT`, dan
`IndexedStateT` dapat menggunakan `IndexedReaderWriterStateT` yang mempunyai
penanda tipe `(R, S1) => F[(W, A, S2)]` dengan `R` yang memiliki semantik `Reader`,
`W` memiliki semantik penulisan monoidik, dan `S` untuk pembaruan keadaan terindeks.

{lang="text"}
~~~~~~~~
  sealed abstract class IndexedReaderWriterStateT[F[_], -R, W, -S1, S2, A] {
    def run(r: R, s: S1)(implicit F: Monad[F]): F[(W, A, S2)] = ...
    ...
  }
  object IndexedReaderWriterStateT {
    def apply[F[_], R, W, S1, S2, A](f: (R, S1) => F[(W, A, S2)]) = ...
  }
  
  type ReaderWriterStateT[F[_], -R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A]
  object ReaderWriterStateT {
    def apply[F[_], R, W, S, A](f: (R, S) => F[(W, A, S)]) = ...
  }
~~~~~~~~

Singkatan disediakan karena bila tidak, tidak ada yang mau menulis kata sepanjang itu:

{lang="text"}
~~~~~~~~
  type IRWST[F[_], -R, W, -S1, S2, A] = IndexedReaderWriterStateT[F, R, W, S1, S2, A]
  val IRWST = IndexedReaderWriterStateT
  type RWST[F[_], -R, W, S, A] = ReaderWriterStateT[F, R, W, S, A]
  val RWST = ReaderWriterStateT
~~~~~~~~

`IRWST` merupakan implementasi yang lebih efisien bila dibandingkan dengan
membuat transformator *stack* dari `ReaderT[WriterT[IndexedStateT[F, ...], ...], ...]`
secara manual.


### `TheseT`

`TheseT` memperkenankan agar galat dapat diakumulasi bila ada beberapa komputasi
berhasil diselesaikan atau untuk membatalkan komputasi secara keseluruhan.

The underlying data type is `F[A \&/ B]` with `A` being the error type,
requiring a `Semigroup` to enable the accumulation of errors.

{lang="text"}
~~~~~~~~
  final case class TheseT[F[_], A, B](run: F[A \&/ B])
  object TheseT {
    def `this`[F[_]: Functor, A, B](a: F[A]): TheseT[F, A, B] = ...
    def that[F[_]: Functor, A, B](b: F[B]): TheseT[F, A, B] = ...
    def both[F[_]: Functor, A, B](ab: F[(A, B)]): TheseT[F, A, B] = ...
  
    implicit def monad[F[_]: Monad, A: Semigroup] = new Monad[TheseT[F, A, ?]] {
      def bind[B, C](fa: TheseT[F, A, B])(f: B => TheseT[F, A, C]) =
        TheseT(fa.run >>= {
          case This(a) => a.wrapThis[C].point[F]
          case That(b) => f(b).run
          case Both(a, b) =>
            f(b).run.map {
              case This(a_)     => (a |+| a_).wrapThis[C]
              case That(c_)     => Both(a, c_)
              case Both(a_, c_) => Both(a |+| a_, c_)
            }
        })
  
      def point[B](b: =>B) = TheseT(b.wrapThat.point[F])
    }
  }
~~~~~~~~

Tidak ada monad khusus yang diasosiasikan dengan `TheseT` karena `TheseT` hanya
merupakan `Monad` biasa. Bila kita ingin membatalkan sebuah kalkulasi, kita dapat
mengembalikan nilai `This`. Namun, bila kita ingin mengakumulasi galat, kita harus
mengembalikan sebuah `Both` yang juga berisi bagian komputasi yang berhasil
diselesaikan.

`TheseT` juga bisa dilihat dari sudut pandang lain: `A` tidak harus berupa sebuah
galat. Hal yang sama dengan `Writer`, `A` bisa saja berupa hasil kalkulasi kedua
yang kita proses bersama dengan kalkulasi utama `B`. `TheseT` memperkenankan
pemutusan dini bila sesuatu yang tak biasa terjadi pada `A` dan mengaharuskannya.
Sebagaimana ketika Charlie Bucket menemukan tiket emas (`A`), dia membuang
batang coklatnya (`B`).


### `ContT`

*Continuation Passing Style* merupakan gaya pemrograman dimana fungsi tidak
pernah mengembalikan nilai, namun *melanjutkan* komputasi selanjutnya. CPS
populer pada Javascript dan Lisp karena gaya ini memperkenankan operasi I/O
asinkronus melalui panggilan balik saat data tersedia. Penulisan ulang untuk
pola semacam ini pada Scala dengan gaya tidak murni (lol, help) kurang lebih
seperti ini:

{lang="text"}
~~~~~~~~
  def foo[I, A](input: I)(next: A => Unit): Unit = next(doSomeStuff(input))
~~~~~~~~

Kita dapat membuatnya menjadi murni (lol, help) dengan memperkenalkan konteks
`F[_]`

{lang="text"}
~~~~~~~~
  def foo[F[_], I, A](input: I)(next: A => F[Unit]): F[Unit]
~~~~~~~~

dan melakukan refaktor (lol, help) agar mengembalikan sebuah fungsi yang
menerima masukan yang disediakan

{lang="text"}
~~~~~~~~
  def foo[F[_], I, A](input: I): (A => F[Unit]) => F[Unit]
~~~~~~~~

`ContT` sebenarnya hanya berupa kontainer untuk penanda ini, dengan sebuah instans
`Monad`

{lang="text"}
~~~~~~~~
  final case class ContT[F[_], B, A](_run: (A => F[B]) => F[B]) {
    def run(f: A => F[B]): F[B] = _run(f)
  }
  object IndexedContT {
    implicit def monad[F[_], B] = new Monad[ContT[F, B, ?]] {
      def point[A](a: =>A) = ContT(_(a))
      def bind[A, C](fa: ContT[F, B, A])(f: A => ContT[F, B, C]) =
        ContT(c_fb => fa.run(a => f(a).run(c_fb)))
    }
  }
~~~~~~~~

dan sintaks pembantu untuk membuat sebuah `ContT` dari sebuah nilai monadik:

{lang="text"}
~~~~~~~~
  implicit class ContTOps[F[_]: Monad, A](self: F[A]) {
    def cps[B]: ContT[F, B, A] = ContT(a_fb => self >>= a_fb)
  }
~~~~~~~~

Namun, penggunaan panggilan ulang sederhana untuk *continuation* (lol, help)
tidak memberikan apapun untuk pemrograman fungsional murni (lol, help) karena
kita sudah mengetahui bagaimana mengurutkan komputasi asinkoronus yang memungkinkan
untuk didistribusi dengan menggunakan `Monad` beserta `bind` atau panah `Kleisli`. 
Agar kita dapat melihat mengapa *continuation* berguna, kita harus memperhitungkan
contoh yang lebih kompleks pada batasan desain yang lebih kaku.


#### Kontrol Alur

Misalkan, bila kita telah memodularkan aplikasi kita menjadi beberapa komponen
yang dapat melakukan operasi I/O, dan tiap komponen dimiliki oleh tim pengembang
lain:

{lang="text"}
~~~~~~~~
  final case class A0()
  final case class A1()
  final case class A2()
  final case class A3()
  final case class A4()
  
  def bar0(a4: A4): IO[A0] = ...
  def bar2(a1: A1): IO[A2] = ...
  def bar3(a2: A2): IO[A3] = ...
  def bar4(a3: A3): IO[A4] = ...
~~~~~~~~

Tujuan kita adalah menghasilkan sebuah `A0` bila kita memiliki sebuah `A1`.
Bila Javascript dan Lisp akan memilih untuk menggunakan kontinyuasi untuk
menyelesaikan masalah ini (karena operasi I/O dapat mencegah operasi lainnya
dijalankan), kita cukup merangkai fungsi-fungsi di atas

{lang="text"}
~~~~~~~~
  def simple(a: A1): IO[A0] = bar2(a) >>= bar3 >>= bar4 >>= bar0
~~~~~~~~

Kita dapat mengangkat `.simple` menjadi bentuk kontinyuasi dengan menggunakan
sintaks pembantu, `.cps`, dan sedikit *boilerplate* (lol, help) untuk tiap
langkah:

{lang="text"}
~~~~~~~~
  def foo1(a: A1): ContT[IO, A0, A2] = bar2(a).cps
  def foo2(a: A2): ContT[IO, A0, A3] = bar3(a).cps
  def foo3(a: A3): ContT[IO, A0, A4] = bar4(a).cps
  
  def flow(a: A1): IO[A0]  = (foo1(a) >>= foo2 >>= foo3).run(bar0)
~~~~~~~~

Jadi, apa yang kita dapatkan dari perubahan diatas? Pertama, alur eksekusi aplikasi
ini berjalan dari kiri ke kanan

{width=60%}
![](images/contt-simple.png)

Bila kita merupakan penulis untuk `foo2` dan ingin melakukan pemrosesan lebih
lanjut terhadap `a0` yang kita terima dari bagian kanan, misal kita ingin memecah
menjadi `foo2a` dan `foo2b`

{width=75%}
![](images/contt-process1.png)

Juga jangan lupa untuk menambah batasan bahwa kita tidak dapat mengubah definisi
dari `flow` atau `bar0`. Bisa jadi karena keduanya bukan kode kita maupun sudah
ditentukan oleh *framework* yang kita gunakan.

Kita juga tidak bisa memproses keluaran dari `a0` dengan mengubah metoda `barX`
lainnya. Namun, dengan `ContT` kita dapat mengubah `foo2`agar memproses hasil
dari kontinyuasi selanjutnya (`next`):

{width=45%}
![](images/contt-process2.png)

Yang bisa kita definisikan sebagai

{lang="text"}
~~~~~~~~
  def foo2(a: A2): ContT[IO, A0, A3] = ContT { next =>
    for {
      a3  <- bar3(a)
      a0  <- next(a3)
    } yield process(a0)
  }
~~~~~~~~

Kita tidak hanya bisa untuk menggunakan `.map` pada nilai kembalian, namun juga
bisa melakukan menempelkan `.bind` pada kontrol alur lain. Sehingga mengubah alur
linier menjadi sebuah graf.

{width=50%}
![](images/contt-elsewhere.png)

{lang="text"}
~~~~~~~~
  def elsewhere: ContT[IO, A0, A4] = ???
  def foo2(a: A2): ContT[IO, A0, A3] = ContT { next =>
    for {
      a3  <- bar3(a)
      a0  <- next(a3)
      a0_ <- if (check(a0)) a0.pure[IO]
             else elsewhere.run(bar0)
    } yield a0_
  }
~~~~~~~~

Atau kita tetap menggunakan alur eksekusi yang lama dan mengulangi
semua eksekusi hilir

{width=45%}
![](images/contt-retry.png)

{lang="text"}
~~~~~~~~
  def foo2(a: A2): ContT[IO, A0, A3] = ContT { next =>
    for {
      a3  <- bar3(a)
      a0  <- next(a3)
      a0_ <- if (check(a0)) a0.pure[IO]
             else next(a3)
    } yield a0_
  }
~~~~~~~~

Potongan diatas hanya melakukan perulangan sekali saja, tidak tak hingga.
Sebagai contoh, kita mungkin meminta operasi hilir untuk mengkonfirmasi ulang
sebuah operasi yang mungkin berbahya.

Pada akhirnya, kita dapat melakukan operasi yang khusus untuk konteks dari
`ContT`, dalam kasus ini `IO`, yang memperkenankan kita untuk menangani galat
dan membersihkan sumber daya komputasi:

{lang="text"}
~~~~~~~~
  def foo2(a: A2): ContT[IO, A0, A3] = bar3(a).ensuring(cleanup).cps
~~~~~~~~


#### Saat Nemu Benang Kusut

Bukanlah sebuah kebetulan bila diagram-diagram diatas terlihat seperti benang kusut.
Hal semacam ini memang terjadi bila kita main-main dengan kontrol alur. Semua
mekanisme yang telah kita diskusikan pada bagian ini memang mudah diterapkan
bila kita dapat menyunting definisi dari `flow`, sehingga kita tidak perlu
menggunakan `ContT`.

Namun, bila kita merancang sebuah *framework*, kita harus mempertimbangkan
penyingkapan sistem plugin karena panggilan balik `ContT` memperkenankan
pengguna untuk lebih leluasa mengkontrol alur program mereka. Dan memang
kenyataanya, kadang kala pengguna memang ingin main benang kusut.

Sebagai contoh, bila kompilator Scala ditulis menggunakan *CPS*, kompilator tersebut
akan memperkenankan pendekatan yang jelas dalam komunikasi antar fase kompilasi.
Sebuah plugin kompilator akan mampu melakukan beberapa hal berdasarkan hasil
penebakan dari tipe sebuah ekspresi yang dikomputasi pada tahap selanjutnya
di proses kompilasi. Hal yang sama, kontinyuasi bisa jadi *API* yang baik untuk
penyunting teks ataupun alat bangun yang luwes.

Kekurangan `ContT` adalah tidak terjaminnya keamanan *stack*. Hal ini menyebabkan
`ContT` tidak dapat digunakan untuk program yang berjalan selamanya.


#### Keren, Tong. Jangan Pegang `ContT`.

Varian yang lebih kompleks dari `ContT` adalah `IndexedContT` yang membungkus
`(A => F[B]) => F[C]`. Parameter tipe baru `C` memperkenankan untuk tipe
pengembalian dari komputasi berbeda pada tiap komponennya. Namun, bila `B` tidak
setara dengan `C` maka `Monad` tidak ada.

Tanpa melewatkan kesempatan untuk menggeneralisasi sebanyak mungkin, `IndexedContT`
sebenarnya diimplmentasikan dalam struktur yang bahkan lebih general. Harap
diperhatikan bahwa ada huruf `s` sebagai penanda jamak sebelum huruf `T`

{lang="text"}
~~~~~~~~
  final case class IndexedContsT[W[_], F[_], C, B, A](_run: W[A => F[B]] => F[C])
  
  type IndexedContT[f[_], c, b, a] = IndexedContsT[Id, f, c, b, a]
  type ContT[f[_], b, a]           = IndexedContsT[Id, f, b, b, a]
  type ContsT[w[_], f[_], b, a]    = IndexedContsT[w, f, b, b, a]
  type Cont[b, a]                  = IndexedContsT[Id, Id, b, b, a]
~~~~~~~~

dimana `W[_]` mempunyai sebuah `Comonad` dan `ContT` diimplementasikan sebagai
sebuah alias tipe. Objek pendamping tersedia untuk alias tipe ini sebagai
konstruktor pembantu.

Memang, lima parameter tipe agak berlebihan dalam penggeneralisasian. Namun,
penggeneralisasian yang berlebihan konsisten dengan kontinyuasi.


### Susunan Transformer dan Implisit Ambigu

Sub-sub-bab ini menututup perbincangan kita mengenai transformator monad
pada Scalaz.

Saat beberapa transformator digabungkan, kita memanggil hasil penggabungan ini
sebagai *susunan transformator*. Walaupun lantung, sangat memungkinkan untuk
mengetahui fiturnya dengan membaca transformator yang ada. Sebagai contoh, bila
kita membangun sebuah konteks `F[_]` yang merupakan set dari transformator
yang digabungkan, seperti

{lang="text"}
~~~~~~~~
  type Ctx[A] = StateT[EitherT[IO, E, ?], S, A]
~~~~~~~~

kita tahu bahwa kita menambah penanganan galat dengan tipe galat `E` (ada monad
`MonadError[Ctx, E]` dan kita mengatur keadaan `A` (ada `MonadState[Ctx, S]`).

Namun, ada beberapa kekurangan dari sisi praktik bila menggunakan transformator
monad dan kelas tipe `Monad` pasangannya:

1.  Beberapa parameter `Monad` implisit mengakibatkan kompilator tidak dapat
    menentukan sintaks yang tepat untuk konteks tersebut.
2.  Secara umum, monad tidak dapat digabungkan. Hal ini berarti bahwa urutan
    pelapisan transformator sangat penting.
3.  Semua interpreter harus diangkat ke konteks umum. Sebagai contoh, mungkin
    saja kita mempunyai sebuah implementasi dari aljabar yang menggunakan `IO`
    dan kita harus membungkusnya dengan `StateT` dan `EitherT`, walau kedua
    transformator tersebut tidak digunakan di dalam interpreter.
4.  Akan ada beban performa yang harus dibayar untuk tiap lapis. Dan beberapa
    transformator monad meminta biaya lebih bila dibandingkan monad lain
    terutama `StateT`. Bahkan, `EitherT` dapat menyebabkan masalah alokasi
    memori untuk aplikasi dengan keluaran tinggi.

Maka dari itu, kita harus membahas penyiasatannya


#### Tanpa Sintaks

Misal kita punya sebuah aljabar

{lang="text"}
~~~~~~~~
  trait Lookup[F[_]] {
    def look: F[Int]
  }
~~~~~~~~

dan beberapa tipe data

{lang="text"}
~~~~~~~~
  final case class Problem(bad: Int)
  final case class Table(last: Int)
~~~~~~~~

yang akan kita gunakan pada logika bisnis kita

{lang="text"}
~~~~~~~~
  def foo[F[_]](L: Lookup[F])(
    implicit
      E: MonadError[F, Problem],
      S: MonadState[F, Table]
  ): F[Int] = for {
    old <- S.get
    i   <- L.look
    _   <- if (i === old.last) E.raiseError(Problem(i))
           else ().pure[F]
  } yield i
~~~~~~~~

Masalah pertama yang kita temui adalah potongan kode ini gagal dikompilasi

{lang="text"}
~~~~~~~~
  [error] value flatMap is not a member of type parameter F[Table]
  [error]     old <- S.get
  [error]              ^
~~~~~~~~

Ada beberapa solusi untuk masalah ini. Yang paling jelas adalah membuat semua
parameter menjadi eksplisit

{lang="text"}
~~~~~~~~
  def foo1[F[_]: Monad](
    L: Lookup[F],
    E: MonadError[F, Problem],
    S: MonadState[F, Table]
  ): F[Int] = ...
~~~~~~~~

dan mengharuskan hanya `Monad` yang bisa dilewatkan secara implisit melalui
batasan konteks. Namun, hal ini berarti kita harus menyambungkan `MonadError`
dan `MonadState` secara manual ketika memanggil `foo1` dan saat memanggil metoda
lain yang meminta sebuah `implicit`

Solusi kedua adalah menghilangkan parameter `implicit` dan menggunakan pembayangan
nama agar semua parameter menjadi eksplisit dengan satu pengecualian. Hal ini
memperkenankan operasi hulu untuk menggunakan resolusi implisit saat memanggil
aljabar ini walaupun kita harus tetap mengumpankan parameter secara eksplisit
bila aljabar ini dipanggil.

{lang="text"}
~~~~~~~~
  @inline final def shadow[A, B, C](a: A, b: B)(f: (A, B) => C): C = f(a, b)
  
  def foo2a[F[_]: Monad](L: Lookup[F])(
    implicit
    E: MonadError[F, Problem],
    S: MonadState[F, Table]
  ): F[Int] = shadow(E, S) { (E, S) => ...
~~~~~~~~

bila kita dapat melakukan pembayangan hanya satu monad saja, atau dengan kata
lain menyerahkan sintaks pada monad lain, dan baru menghapus pembayangan tersebut
saat aljabar ini dipanggil oleh metoda lain

{lang="text"}
~~~~~~~~
  @inline final def shadow[A, B](a: A)(f: A => B): B = f(a)
  ...
  
  def foo2b[F[_]](L: Lookup[F])(
    implicit
    E: MonadError[F, Problem],
    S: MonadState[F, Table]
  ): F[Int] = shadow(E) { E => ...
~~~~~~~~

Pilihan ketiga, walaupun lebih berat di awal, adalah dengan membuat kelas tipe
`Monad` khusus yang membawa rujukan `implicit` ke dua kelas `Monad` yang kita
pilih 

{lang="text"}
~~~~~~~~
  trait MonadErrorState[F[_], E, S] {
    implicit def E: MonadError[F, E]
    implicit def S: MonadState[F, S]
  }
~~~~~~~~

dan sebuah derivasi dari kelas tipe berdasarkan sebuah `MonadError` dan
`MonadState`

{lang="text"}
~~~~~~~~
  object MonadErrorState {
    implicit def create[F[_], E, S](
      implicit
        E0: MonadError[F, E],
        S0: MonadState[F, S]
    ) = new MonadErrorState[F, E, S] {
      def E: MonadError[F, E] = E0
      def S: MonadState[F, S] = S0
    }
  }
~~~~~~~~

Sekarang, bila kita ingin mengakses `S` atau `E`, kita bisa mendapatkannya
dengan `F.S` maupun `F.E`

{lang="text"}
~~~~~~~~
  def foo3a[F[_]: Monad](L: Lookup[F])(
    implicit F: MonadErrorState[F, Problem, Table]
  ): F[Int] =
    for {
      old <- F.S.get
      i   <- L.look
      _ <- if (i === old.last) F.E.raiseError(Problem(i))
      else ().pure[F]
    } yield i
~~~~~~~~

Sebagaimana halnya dengan solusi kedua, kita bisa memilih salah satu dari
instans `Monad` dan menjadikannya sebagai konteks `implicit` dalam blok,
kita dapat melakukannya mengimpornya

{lang="text"}
~~~~~~~~
  def foo3b[F[_]](L: Lookup[F])(
    implicit F: MonadErrorState[F, Problem, Table]
  ): F[Int] = {
    import F.E
    ...
  }
~~~~~~~~


#### Menyusun Transformator

`EitherT[StateT[...], ...]` memiliki sebuah instans `MonadError` namun tidak
mempunyai `MonadState`. Sedangkan `StateT[EitherT[...], ..]` mampu menyediakan
keduanya.

Untuk menyiasati hal tersebut, kita dapat mempelajari derivasi implisit pada
objek pendamping dari transformator tersebut dan memastikan bahwa transformator
paling luar menyediakan semua yang kita butuhkan.

Patokan yang dipakai adalah semakin kompleks sebuah transformator, semakin luar
tempat transformator tersebut berada pada susunan. Bab ini akan menyajikan
transformator yang semakin tinggi tingkat kompleksitasnya.


#### Mengangkat Penerjemah

Melanjutkan contoh yang sama, misalkan aljabar `Lookup` kita memiliki interpreter
`IO`

{lang="text"}
~~~~~~~~
  object LookupRandom extends Lookup[IO] {
    def look: IO[Int] = IO { util.Random.nextInt }
  }
~~~~~~~~

namun kita menginginkan agar konteks kita seperti

{lang="text"}
~~~~~~~~
  type Ctx[A] = StateT[EitherT[IO, Problem, ?], Table, A]
~~~~~~~~

agar dapat memberi kita sebuah `MonadError` dan `MonadState`. Hal ini berarti
kita harus membungkus `LookupRandom` agar dapat beroperasi pada `Ctx`.

A> Kemungkinan mendapatkan tipe yang benar pada percobaan pertama kurang lebih
A> 1/3720.

Pertama, kita akan menggunakan sintaks `.liftM` pada `Monad` yang memberikan
`MonadTrans` sehingga dapat mengangkat `F[A]` menjadi `G[F, A]`

{lang="text"}
~~~~~~~~
  final class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def liftM[G[_[_], _]: MonadTrans]: G[F, A] = ...
    ...
  }
~~~~~~~~

Yang penting untuk diperhatikan adalah parameter tipe untuk `.liftM` mempunyai
dua celah tipe dengan bentuk `_[_]` dan `_`. Bila kita membuat alias tipe dengan
bentuk seperti

{lang="text"}
~~~~~~~~
  type Ctx0[F[_], A] = StateT[EitherT[F, Problem, ?], Table, A]
  type Ctx1[F[_], A] = EitherT[F, Problem, A]
  type Ctx2[F[_], A] = StateT[F, Table, A]
~~~~~~~~

Kita dapat mengabstraksi `MonadTrans` agar mengangkat `Lookup[F]` menjadi
`Lookup[G[F, ?]]` dimana G merupakan Transformator Monad:

{lang="text"}
~~~~~~~~
  def liftM[F[_]: Monad, G[_[_], _]: MonadTrans](f: Lookup[F]) =
    new Lookup[G[F, ?]] {
      def look: G[F, Int] = f.look.liftM[G]
    }
~~~~~~~~

Memperkenankan kita untuk membungkus `EitherT` satu kali, dan kemudian
membungkus `StateT`

{lang="text"}
~~~~~~~~
  val wrap1 = Lookup.liftM[IO, Ctx1](LookupRandom)
  val wrap2: Lookup[Ctx] = Lookup.liftM[EitherT[IO, Problem, ?], Ctx2](wrap1)
~~~~~~~~

Cara lain untuk mencapai ini dalam satu langkah adalah dengan menggunakan
`MoonadIO` yang mampu mengangkat sebuah `IO` menjadi sebuah susunan transformator:

{lang="text"}
~~~~~~~~
  @typeclass trait MonadIO[F[_]] extends Monad[F] {
    def liftIO[A](ioa: IO[A]): F[A]
  }
~~~~~~~~

dengan instans `MonadIO` untuk semua kombinasi umum dari transformator.

Plat cetak berlebih untuk mengangkat sebuah interpreter `IO` menjadi semua monad
apapun yang mempunyai instans `MonadIO` hanya dua baris kode untuk definisi
interpreter, ditambah satu baris untuk tiap elemen dari aljabar, dan satu baris
terakhir untuk memanggilnya.

{lang="text"}
~~~~~~~~
  def liftIO[F[_]: MonadIO](io: Lookup[IO]) = new Lookup[F] {
    def look: F[Int] = io.look.liftIO[F]
  }
  
  val L: Lookup[Ctx] = Lookup.liftIO(LookupRandom)
~~~~~~~~

A> Sebuah tambahan kompilator yang secara otomatis membuat `.liftM`, `liftIO`,
A> dan tambahan lain yang diperkenalkan pada bab ini akan sangat membantu
A> ekosistem Scala dan Scalaz!


#### Performa

Masalah paling besar pada Transformator Monad adalah tambahan beban sehingga
performa menurun. Walaupun `EitherT` memiliki tambahan beban yang kecil, tiap
kali pemanggilan `.flatMap` akan membuat banyak objek. Dampak dari hal seperti
ini akan terlihat pada aplikasi yang mempunyai keluaran besar dimana tiap alokasi
objek turut andil dalam gambaran besar. Transformator lain, seperti `StateT`,
akan menambah trampolin yang juga tak kecil bebannya. Terlebih lagi untuk
transformator `ContT` yang menahan semua rantai panggilan pada memori.

A> Beberapa aplikasi tidak begitu mempedulikan mengenai alokasi memori bila
A> aplikasi tersebut bergantung pada jaringan ataupun I/O. Sangat disarankan
A> untuk selalu mengukur performa aplikasi secara mendetail.

Bila performa menjadi masalah, maka solusi satu-satunya adalah dengan tidak
menggunakan Transformator Monad. Atau setidaknya struktur data transformator.
Keuntungan paling besar dari kelas tipe `Monad`, seperti `MonadState` adalah
kita dapat membuat konteks `F[_]` yang teroptimasi untuk aplikasi kita yang
menyediakan kelas tipe secara alami. Kita akan mempelajari bagaimana cara untuk
membuat sebuah konteks `F[_]` yang optimal pada dua bab selanjutnya pada saat
kita membahas mengenai dua struktur data yang sudah kita lihat sebelumnya:
`Free` dan `IO`.


## Makan Gratis

Industri perangkat lunak sangat menginginkan bahasa pemrograman tingkat tinggi
yang memberikan jaminan keamanan sedangkan pengembang *trading* menginginkan
efisiensi dan keandalan dengan performa waktu-jalan yang tinggi.

Kompiler tepat waktu (KTM) pada JVM bekerja dengan sangat baik sampai pada tahap
fungsi-fungsi sederhana dapat mempunyai performa yang setara dengan ekuivalen
yang ditulis pada bahasa pemrograman C maupun C++, bila mengabaikan beban pada
pengumpulan sampah. Namun, KTM hanya bekerja pada *optimisasi tingkat rendah*
seperti: prediksi cabang operasi, *inline* fungsi (lol, help!), membuka ikalan,
dan sejenisnya. 

KTM tidak melakukan optimasi pada logika bisnis kita, sebagai contoh, pengelompokan
panggilan jaringan atau paralelisasi tugas tugas independen. Pengembang bertanggung
jawab untuk menulis logika bisnis dan optimasi pada saat yang bersamaan sehingga
menyebabkan penurunan keterbacaan dan mempersulit pemeliharaan. Akan sangat bagus
bila optimasi menjadi perhatian tangensial.

Bila kita memiliki struktur data yang mendeskripsikan logika bisnis kita pada
konsep tingkat tinggi, bukan instruksi mesin, kita dapat melakukan *optimasi
tingkat tinggi*. Struktur data semacam ini biasanya disebut struktur data *Free*
dan dapat dibuat tanpa membayar apapun untuk anggota dari antarmuka aljabarik
dari program kita. Sebagai contoh, sebuah *Free Applicative* dapat dibuat
sehingga kita dapat mengelompokkan atau penghapusan duplikasi atas I/O jaringan
intensif.

Pada bagian ini, kita akan mempelajari cara untuk membuat struktur data *free*
(gratis) dan cara penggunaannya.


### `Free` (`Monad`)

Pada dasarnya, sebuah monad mendeskripsikan program berurutan dimana setiap
tahap bergantung pada tahap sebelumnya. Maka dari itu, kita tidak bisa serta-merta
mengubah sesuatu yang hanya tahu apa yang telah dijalankan dan apa yang akan
dijalankan.

A> Sekitar tahun 2015, sangat trendi untuk menggunakan struktur *free* saat
A> menulis program dengan bahasa pemrograman fungsional. Jadi, dengan mempelajari
A> `Free` kita dapat memahami cara penggunaan dan penulisannya.
A>
A> Selain itu, ada banyak plat cetak yang digunakan untuk membuat struktur *free*.
A> Kita akan menggunakannya untuk mempelajari bagaimana cara membuat plat cetak. 

Sebagai pengingat, `Free` merupakan representasi struktur data dari sebuah `Monad`
dan didefinisikan dengan tiga anggota

{lang="text"}
~~~~~~~~
  sealed abstract class Free[S[_], A] {
    def mapSuspension[T[_]](f: S ~> T): Free[T, A] = ...
    def foldMap[M[_]: Monad](f: S ~> M): M[A] = ...
    ...
  }
  object Free {
    implicit def monad[S[_], A]: Monad[Free[S, A]] = ...
  
    private final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
    private final case class Return[S[_], A](a: A)     extends Free[S, A]
    private final case class Gosub[S[_], A0, B](
      a: Free[S, A0],
      f: A0 => Free[S, B]
    ) extends Free[S, B] { type A = A0 }
  
    def liftF[S[_], A](value: S[A]): Free[S, A] = Suspend(value)
    ...
  }
~~~~~~~~

-   `Suspend` merepresentasikan sebuah program yang belum diinterpretasi
-   `Return` sama dengan `.pure`
-   `Gosub` sama dengan `.bind`

Sebuah `Free[S, A]` dapat digenerasi secara cuma-cuma untuk semua aljabar `S`.
Agar lebih jelas, anggap aljabar `Machines` pada aplikasi kita

{lang="text"}
~~~~~~~~
  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[Map[MachineNode, Epoch]]
    def start(node: MachineNode): F[Unit]
    def stop(node: MachineNode): F[Unit]
  }
~~~~~~~~

Kita mendefinisikan `Free` yang dibuat secara cuma cuma untuk `Machine` dengan
membuat GADT dengan tipe data untuk tiap elemen dari aljabar. Tiap tipe data
mempunyai parameter masukan yang sama dengan elemen yang sesuai dan diparemeterisasi
atas nilai kembalian dengan nama yang sama:

{lang="text"}
~~~~~~~~
  object Machines {
    sealed abstract class Ast[A]
    final case class GetTime()                extends Ast[Epoch]
    final case class GetManaged()             extends Ast[NonEmptyList[MachineNode]]
    final case class GetAlive()               extends Ast[Map[MachineNode, Epoch]]
    final case class Start(node: MachineNode) extends Ast[Unit]
    final case class Stop(node: MachineNode)  extends Ast[Unit]
    ...
~~~~~~~~

GADT yang mendefinisikan Pohon Sintaks Abstrak (PSA) karena tiap anggota
merepresentasikan sebuah komputasi pada sebuah program.

W> `Free` yang digenerasi secara cuma-cuma untuk `Machine` mempunyai bentuk
W> `Free[Machines.Ast, ?], atau untuk PSA, bukan `Free[Machines, ?]`. Sangat mudah
W> terkecoh karena yang akhir juga dapat dikompilasi, namun tidak ada gunanya.

Lalu kita akan mendefinisikan `.liftF`, sebuah implementasi dari `Machines` dengan
`Free[AST, ?]` sebagai konteksnya. Setiap metoda cukup mendelegasi ke `Free.liftT`
untuk membuat sebuah `Suspend`

{lang="text"}
~~~~~~~~
  ...
    def liftF = new Machines[Free[Ast, ?]] {
      def getTime = Free.liftF(GetTime())
      def getManaged = Free.liftF(GetManaged())
      def getAlive = Free.liftF(GetAlive())
      def start(node: MachineNode) = Free.liftF(Start(node))
      def stop(node: MachineNode) = Free.liftF(Stop(node))
    }
  }
~~~~~~~~

Saat kita membangun program kita yang terparametrisasi atas sebuah `Free`, kita
menjalankannya dengan menyediakan sebuah *interpreter* (transformasi natural
`Ast ~> M`) ke metoda `.foldMap`. Sebagai contoh, bila kita dapat menyediakan
sebuah interpreter yang memetakan ke `IO`, kita dapat membangun sebuah program
`IO[Unit]` dengan menggunakan PSA *free*.

{lang="text"}
~~~~~~~~
  def program[F[_]: Monad](M: Machines[F]): F[Unit] = ...
  
  val interpreter: Machines.Ast ~> IO = ...
  
  val app: IO[Unit] = program[Free[Machines.Ast, ?]](Machines.liftF)
                        .foldMap(interpreter)
~~~~~~~~

Agar lebih lengkap, sebuah interpreter yang mendelegasikan kepada sebuah implementasi
langsung biasanya mudah dalam penulisan. Hal ini mungkin berguna bila bagian aplikasi
yang lain menggunakan `Free` sebagai konteks dan kita juga sudah mempunyai
implementasi `IO` yang ingin kita gunakan:

{lang="text"}
~~~~~~~~
  def interpreter[F[_]](f: Machines[F]): Ast ~> F = λ[Ast ~> F] {
    case GetTime()    => f.getTime
    case GetManaged() => f.getManaged
    case GetAlive()   => f.getAlive
    case Start(node)  => f.start(node)
    case Stop(node)   => f.stop(node)
  }
~~~~~~~~

Namun, logika bisnis kita butuh lebih dari `Machines`, kita juga butuh akses ke
aljabar `Drone` seperti ini

{lang="text"}
~~~~~~~~
  trait Drone[F[_]] {
    def getBacklog: F[Int]
    def getAgents: F[Int]
  }
  object Drone {
    sealed abstract class Ast[A]
    ...
    def liftF = ...
    def interpreter = ...
  }
~~~~~~~~

Yang kita inginkan adalah PSA kita menjadi sebuah kombinasi dari PSA `Machines`
dan `Drone`. Kita telah mempelajari `Coproduct` pada bab 6 yang merupakan
sebuah disjungsi jenis tinggi (lol, help):

{lang="text"}
~~~~~~~~
  final case class Coproduct[F[_], G[_], A](run: F[A] \/ G[A])
~~~~~~~~

Kita dapat menggunakan konteks `Free[Coproduct[Machines.Ast, Drone.Ast, ?], ?]`.

Kita juga bisa saja membuat ko-produk secara manual, namun kita akan mempunyai
plat cetak yang terlalu banyak. Selain itu, kita harus melakukannya berulang kali
bila kita ingin menambah aljabar ketiga.

Kelas tipe `scalaz.Inject` membantu:

{lang="text"}
~~~~~~~~
  type :<:[F[_], G[_]] = Inject[F, G]
  sealed abstract class Inject[F[_], G[_]] {
    def inj[A](fa: F[A]): G[A]
    def prj[A](ga: G[A]): Option[F[A]]
  }
  object Inject {
    implicit def left[F[_], G[_]]: F :<: Coproduct[F, G, ?]] = ...
    ...
  }
~~~~~~~~

Derivasi `implicit` menghasilkan instans `Inject` saat kita membutuhkannya.
Hal ini memperkenankan kita untuk menulis ulang `liftF` agar dapat beroperasi
pada semua kombinasi dari PSA:

{lang="text"}
~~~~~~~~
  def liftF[F[_]](implicit I: Ast :<: F) = new Machines[Free[F, ?]] {
    def getTime                  = Free.liftF(I.inj(GetTime()))
    def getManaged               = Free.liftF(I.inj(GetManaged()))
    def getAlive                 = Free.liftF(I.inj(GetAlive()))
    def start(node: MachineNode) = Free.liftF(I.inj(Start(node)))
    def stop(node: MachineNode)  = Free.liftF(I.inj(Stop(node)))
  }
~~~~~~~~

Sungguh apik bila `F :<: G` dibaca sebagaimana bila `Ast` sebagai salah satu anggota
dari set instruksi lengkap dari `F`.

A> Sebuah plugin kompilator yang secara otomatis memproduksi plat cetak `scalaz.Free`
A> akan sangat membantu ekosistem Scalaz! Tidak hanya menyusahkan bila kita harus
A> menulis plat cetak secara manual, namun juga ada kemungkinan sebuah salah ketik
A> merusak hari kita: bila ada dua anggota dari aljabar yang mempunyai dua penanda
A> tipe yang sama, bisa saja kita tidak memperhatikannya.

Dan menggabungkan semuanya, misalkan kita mempunyai sebuah program yang kita tulis
untuk mengabstraksi `Monad`

{lang="text"}
~~~~~~~~
  def program[F[_]: Monad](M: Machines[F], D: Drone[F]): F[Unit] = ...
~~~~~~~~

dan kita mempunyai implementasi dari `Machines` dan `Drone` yang sudah ada,
kita dapat membuat interpreter dari implementasi tersebut:

{lang="text"}
~~~~~~~~
  val MachinesIO: Machines[IO] = ...
  val DroneIO: Drone[IO]       = ...
  
  val M: Machines.Ast ~> IO = Machines.interpreter(MachinesIO)
  val D: Drone.Ast ~> IO    = Drone.interpreter(DroneIO)
~~~~~~~~

dan menggabungkannya menjadi sebuah set instruksi dengan menggunakan metoda bantuan
dari pasangan `NaturalTransformation`

{lang="text"}
~~~~~~~~
  object NaturalTransformation {
    def or[F[_], G[_], H[_]](fg: F ~> G, hg: H ~> G): Coproduct[F, H, ?] ~> G = ...
    ...
  }
  
  type Ast[a] = Coproduct[Machines.Ast, Drone.Ast, a]
  
  val interpreter: Ast ~> IO = NaturalTransformation.or(M, D)
~~~~~~~~

Lalu menggunakannya untuk menghasilkan `IO`

{lang="text"}
~~~~~~~~
  val app: IO[Unit] = program[Free[Ast, ?]](Machines.liftF, Drone.liftF)
                        .foldMap(interpreter)
~~~~~~~~

Nah, kita jadi berputar-putar! Kita bisa saja menggunakan `IO` sebagai konteks
program kita dan menghindari `Free`. Lalu, kenapa kita harus seperti ini?
Berikut merupakan beberapa contoh dimana `Free` bisa jadi berguna.


#### Testing: Tiruan dan Potongan

Mungkin terlihat tidak masuk akal bila kita mengusulkan untuk menggunakan `Free`
agar kita dapat mengurangi plat cetak namun, di sisi lain, kita telah menulis
kode yang sangat banyak yang berkaitan dengan `Free` sendiri. Akan tetapi,
ada titik kritis dimana `Ast` melunasi semua biaya yang telah kita tulis
saat kita mempunyai banyak tes yang membutuhkan banyak potongan implementasi
kode.

Bila `.Ast` dan `.liftF` didefinisikan untuk sebuah aljabar, kita dapat membuat
*interpreter parsial*

{lang="text"}
~~~~~~~~
  val M: Machines.Ast ~> Id = stub[Map[MachineNode, Epoch]] {
    case Machines.GetAlive() => Map.empty
  }
  val D: Drone.Ast ~> Id = stub[Int] {
    case Drone.GetBacklog() => 1
  }
~~~~~~~~

yang dapat digunakan untuk mengetes program kita

{lang="text"}
~~~~~~~~
  program[Free[Ast, ?]](Machines.liftF, Drone.liftF)
    .foldMap(or(M, D))
    .shouldBe(1)
~~~~~~~~

Dengan menggunakan fungsi parsial, dan bukan fungsi total, kita memaparkan diri
kita pada galat waktu-jalan. Banyak tim yang dengan ringan hati menerima risiko
ini pada tes unit mereka karena tes akan gagal bila pemrogram melakukan kesalahan.

Walaupun kita juga bisa mencapai hal yang sama dengan menulis implementasi dari
aljabar kita yang mengimplementasikan setiap metoda dengan `???` dan mengesampingkan
apa yang kita butuhkan sesuai dengan per kasus.

A> Pustaka [smock](https://github.com/djspiewak/smock) lebih luwes untuk keperluan
A> testing. Namun, demi contoh pendek ini, kita akan mendefinisikan `stub` sendiri
A> menggunakan trik inferensi tipe yang dapat ditemukan di banyak tempat pada
A> kode sumber Scalaz. Penyebab `Stub` didefinisikan pada kelas tersendiri adalah
A> agar kita hanya perlu menyediakan parameter tipe `A` dengan `F` dan `G` ditebak
A> dari penjabaran sisi kiri:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object Mocker {
A>     final class Stub[A] {
A>       def apply[F[_], G[_]](pf: PartialFunction[F[A], G[A]]): F ~> G = new (F ~> G) {
A>         def apply[α](fa: F[α]) = pf.asInstanceOf[PartialFunction[F[α], G[α]]](fa)
A>       }
A>     }
A>     def stub[A]: Stub[A] = new Stub[A]
A>   }
A> ~~~~~~~~


#### Monitoring

Sudah pada umumnya ketika aplikasi peladen diawasi dengan agen waktu-jalan
yang memanipulasi *bytecode* untuk menyisipkan profiler dan mengekstrak
informasi penggunaan dan performa.

Bila konteks dari aplikasi kita adalah `Free`, kita tidak perlu menggunakan
manipulasi *bytecode*. Kita dapat mengimplementasikan monitor dengan efek samping
sebagai sebuah interpreter yang bisa kita atur sepenuhnya.

A> Introspeksi waktu-jalan merupakan salah satu dari sedikit kasus yang dapat
A> membenarkan penggunaan efek samping. Bila pengawasan tidak terlihat dari
A> program itu sendiri, transparansi rujukan masih tetap berlaku. Argumen ini
A> digunakan oleh tim yang menggunakan pencatatan log debug dengan efek samping,
A> dan juga kita gunakan sebagai argumen yang memperkenankan mutasi pada
A> implementasi `Memo`.

Sebagai contoh, misal penggunaan "agen" `Ast ~> Ast`

{lang="text"}
~~~~~~~~
  val Monitor = λ[Demo.Ast ~> Demo.Ast](
    _.run match {
      case \/-(m @ Drone.GetBacklog()) =>
        JmxAbstractFactoryBeanSingletonProviderUtilImpl.count("backlog")
        Coproduct.rightc(m)
      case other =>
        Coproduct(other)
    }
  )
~~~~~~~~

yang mencatat metoda penyelawatan: kita bisa menggunakan rutin dari vendor khusus
pada kode yang nyata digunakan di produksi atau kita bisa melihat pesan khusus
yang kita inginkan dan mencatatnya sebagai alat bantu debug.


Kita dapat menempelkan `Monitor` ke aplikasi `Free` kita yang sudah ada di
tahap produksi dengan

{lang="text"}
~~~~~~~~
  .mapSuspension(Monitor).foldMap(interpreter)
~~~~~~~~

or combine the natural transformations and run with a single

atau menggabungkannya dengan transformasi natural dan menjalankannya dengan
sebaris

{lang="text"}
~~~~~~~~
  .foldMap(Monitor.andThen(interpreter))
~~~~~~~~


#### Tambal Ban

Sebagai tenaga ahli, kita terbiasa dengan permintaan penyiasatan aneh yang akan
ditambahkan pada logika utama dari aplikasi. Mungkin juga kita ingin mengkodifikasi
kasus di luar parameter normal sebagai sebuah *pengecualian* dan menanganinya
sesuai dengan logika inti kita.

Sebagai contoh, misalkan kita mendapat memo dari bagian keuangan yang berisi

> *PENTING: Bob menggunakan simpul `#c0ffee` untuk menjalankan laporan keuangan
> akhir tahun. JANGAN MATIKEUN MESINNYA BEGO!!SEBELAS111

Sangat tidak mungkin untuk mendiskusikan mengapa Bob tidak boleh menggunakan
mesin kita untuk keperluan akuntansinya yang sangat penting. Jadi, kita harus
membedah logika bisnis kita dan menelurkan sebuah rilis ke tahap produksi
secepat mungkin.

Tambalan kita dapat dipetakan menjadi sebuah struktur `Free` yang memperkenankan
kita untuk mengembalikan sebuah hasil yang sudah jadi (`Free.pure`), bukan
instruksi terjadwal. Kita mengkhususkan instruksi tersebut pada sebuah transformasi
natural dengan nilai kembalian:

{lang="text"}
~~~~~~~~
  val monkey = λ[Machines.Ast ~> Free[Machines.Ast, ?]] {
    case Machines.Stop(MachineNode("#c0ffee")) => Free.pure(())
    case other                                 => Free.liftF(other)
  }
~~~~~~~~

pastikan sepasti-pastinya bahwa kode di atas memang benar berjalan sesuai keinginan,
lalu gunakan di lingkungan produksi, atur alarm agar minggu depan untuk mengingatkan
agar kita hapus kode ini, dan hapus akses Bob ke server kita.

Tes unit kita dapat menggunakan `State` sebagai konteks target, sehingga kita
dapat melacak semua simpul yang kita hentikan:

{lang="text"}
~~~~~~~~
  type S = Set[MachineNode]
  val M: Machines.Ast ~> State[S, ?] = Mocker.stub[Unit] {
    case Machines.Stop(node) => State.modify[S](_ + node)
  }
  
  Machines
    .liftF[Machines.Ast]
    .stop(MachineNode("#c0ffee"))
    .foldMap(monkey)
    .foldMap(M)
    .exec(Set.empty)
    .shouldBe(Set.empty)
~~~~~~~~

juga dengan tes untuk simpul "normal" yang tidak kita hentikan.

Keuntungan menggunakan `Free` untuk menghindari penghentian simpul `#c0ffee`
adalah kita dapat memastikan bahwa semua penggunaan tercatat, bukan harus mencari
logika bisnis dan mencari penggunaan `.stop` satu per satu. Bila konteks aplikasi
kita hanya berupa `IO` kita dapat mengimplementasikan logika ini pada implementasi
`Machines,IO]`. Namun, keuntungan menggunakan `Free` adalah kita tidak harus
menyentuk kode yang sudah ada, namun kita hanya perlu mengisolasi dan mengetes
perilaku (sementara) ini tanpa harus terikat pada implementasi `IO`.

### `FreeAp` (`Applicative`)

Walaupun bab ini berjudul **Monad Lanjutan*, poin utama adalah: *kita tidak boleh
menggunakan monad kecuali bila kita memang **benar benar** harus*. Pada bagian
ini, kita akan tahu mengapa `FreeAp` (aplikatif *free*) lebih disukai dibandingkan
monad `Free`.

`FreeAp` didefinisikan sebagai representasi struktur data dari metoda `ap` dan `pure`
dari kelas tipe `Applicative`:

{lang="text"}
~~~~~~~~
  sealed abstract class FreeAp[S[_], A] {
    def hoist[G[_]](f: S ~> G): FreeAp[G,A] = ...
    def foldMap[G[_]: Applicative](f: S ~> G): G[A] = ...
    def monadic: Free[S, A] = ...
    def analyze[M:Monoid](f: F ~> λ[α => M]): M = ...
    ...
  }
  object FreeAp {
    implicit def applicative[S[_], A]: Applicative[FreeAp[S, A]] = ...
  
    private final case class Pure[S[_], A](a: A) extends FreeAp[S, A]
    private final case class Ap[S[_], A, B](
      value: () => S[B],
      function: () => FreeAp[S, B => A]
    ) extends FreeAp[S, A]
  
    def pure[S[_], A](a: A): FreeAp[S, A] = Pure(a)
    def lift[S[_], A](x: =>S[A]): FreeAp[S, A] = ...
    ...
  }
~~~~~~~~

Metoda `.hoist` dan `.foldMap` seperti analog mereka dari `Free`, `.mapSuspension`
dan `.foldMap`.

Agar lebih mudah, kita dapat membuat `Free[S, A]` dari `FreeAp[S, A]` yang sudah
kita punyai dengan menggunakan metoda `.monadic`. Pembuatan ini sangat berguna
terutama saat kita mengoptimasi subsistem `Applicative` yang belum digunakan
sebagai bagian dari program `Free` yang lebih besar.

Sebagaimana `Free`, kita harus membuat `FreeAp` untuk PSA kita. Hal ini juga
berarti kita harus membuat plat cetak lagi...

{lang="text"}
~~~~~~~~
  def liftA[F[_]](implicit I: Ast :<: F) = new Machines[FreeAp[F, ?]] {
    def getTime = FreeAp.lift(I.inj(GetTime()))
    ...
  }
~~~~~~~~


#### Pengelompokan Panggilan Jaringan

Kita akan membuka bab ini dengan klaim luar biasa mengenai performa. Saatnya
membuktikannya.

Versi [manusiawi](https://gist.github.com/hellerbarde/2843375#file-latency_humanized-markdown)
dari [angka latensi](https://norvig.com/21-days.html#answers) dari Peter Norvig
yang ditulis oleh Philip Stark akan menjadi motivasi mengapa kita harus fokus
untuk mengurangi panggilan melalui jaringan untuk mengoptimasi sebuah aplikasi:

| Komputer                          | Skala Waktu Manusia | Analogi Manusia                |
|--------------------------------- |--------------- |------------------------------ |
| Perujukan tembolok L1             | 0.5 detik           | Satu detak jantung             |
| Salah prediksi cabang             | 5 detik             | Satu kali menguap              |
| Perujukan tembolok L2             | 7 detik             | Satu kali menguap panjang      |
| Buka / tutup mutex                | 25 detik            | Buat satu cangkir teh          |
| Perujukan memori utama            | 100 detik           | Gosok gigi                     |
| Kompresi 1Kb dengan Zippy         | 50 menit            | Satu putaran CI kompilator scala |
| Kirim 2Kb melalu jaringan 1Gbps   | 5.5 jam             | Kereta London ke Edinburg      |
| Baca acak SSD                     | 1.7 hari            | Akhir pekan                    |
| Baca 1MB berurutan dari memori    | 2.9 hari            | Akhir pekan panjang            |
| Mengelilingi pusat data yang sama | 5.8 hari            | Liburan panjang AS             |
| Baca 1MB berurutan dari SSD       | 11.6 hari           | Liburan pendek UE              |
| Pencarian di diska                | 16.5 minggu         | Satu semester kampus           |
| Baca 1MB berurutan dari diska     | 7.8 bulan           | Cuti melahirkan di Norwegia    |
| Kirim paket CA->Belanda->CA       | 4.8 tahun           | Satu periode pemerintahan      |

Walaupun `Free` dan `FreeAp` memberikan beban memori tambahan, ekuivalen dari
100 detik untuk manusia, tiap kali kita memanggil dua panggilan berurutan di
sebuah kelompok panggilan, kita bisa menghemat 5 tahun.

Saat kita berada pada konteks `Applicative`, kita dapat mengoptimasi aplikasi
kita dengan aman, tanpa harus menggagalkan ekspektasi apapun dari program asli.
Terlebih lagi, bisa menghindari pengaburan logika bisnis.

Untungnya, logika bisnis utamakita hanya meminta sebuah `Applicative`. Harap
diingat

{lang="text"}
~~~~~~~~
  final class DynAgentsModule[F[_]: Applicative](D: Drone[F], M: Machines[F])
      extends DynAgents[F] {
    def act(world: WorldView): F[WorldView] = ...
    ...
  }
~~~~~~~~

Kita akan mengawali dengan membuat plat cetak `lift` untuk aljabar `Batch` baru

{lang="text"}
~~~~~~~~
  trait Batch[F[_]] {
    def start(nodes: NonEmptyList[MachineNode]): F[Unit]
  }
  object Batch {
    sealed abstract class Ast[A]
    final case class Start(nodes: NonEmptyList[MachineNode]) extends Ast[Unit]
  
    def liftA[F[_]](implicit I: Ast :<: F) = new Batch[FreeAp[F, ?]] {
      def start(nodes: NonEmptyList[MachineNode]) = FreeAp.lift(I.inj(Start(nodes)))
    }
  }
~~~~~~~~

dan kita akan membuat sebuah instans `DynAgentsModule` dengan `FreeAp` sebagai
konteks

{lang="text"}
~~~~~~~~
  type Orig[a] = Coproduct[Machines.Ast, Drone.Ast, a]
  
  val world: WorldView = ...
  val program = new DynAgentsModule(Drone.liftA[Orig], Machines.liftA[Orig])
  val freeap  = program.act(world)
~~~~~~~~

Pada bab 6, kita telah mempelajari tipe data `Const` yang memperkenankan kita
untuk menganalisis sebuah program. Tidak mengherankan bahwa `FreeAp.analyze`
diimplementasikan menggunakan `Const`:

{lang="text"}
~~~~~~~~
  sealed abstract class FreeAp[S[_], A] {
    ...
    def analyze[M: Monoid](f: S ~> λ[α => M]): M =
      foldMap(λ[S ~> Const[M, ?]](x => Const(f(x)))).getConst
  }
~~~~~~~~

Kita menyediakan sebuah transformasi natural untuk mencatat semua pemulaian
simpul dan meng-`.analyze`-is program kita untuk mendapatkan semua simpul
yang harus dijalankan:

{lang="text"}
~~~~~~~~
  val gather = λ[Orig ~> λ[α => IList[MachineNode]]] {
    case Coproduct(-\/(Machines.Start(node))) => IList.single(node)
    case _                                    => IList.empty
  }
  val gathered: IList[MachineNode] = freeap.analyze(gather)
~~~~~~~~

Langkah selanjutnya adalah memperluas set instruksi dari `Orig` menjadi `Extended`
yang juga mengikutsertakan `Batch.Ast` dan menulis sebuah program `FreeAp` yang
memulai semua simpul yang sudah dikumpulkan menggunakan metoda `gathered` dalam
satu panggilan jaringan

{lang="text"}
~~~~~~~~
  type Extended[a] = Coproduct[Batch.Ast, Orig, a]
  def batch(nodes: IList[MachineNode]): FreeAp[Extended, Unit] =
    nodes.toNel match {
      case None        => FreeAp.pure(())
      case Some(nodes) => FreeAp.lift(Coproduct.leftc(Batch.Start(nodes)))
    }
~~~~~~~~

Kita juga harus menghapus semua panggilan ke `Machise.Start` yang dapat kita
lakukan dengan transformasi natural

{lang="text"}
~~~~~~~~
  val nostart = λ[Orig ~> FreeAp[Extended, ?]] {
    case Coproduct(-\/(Machines.Start(_))) => FreeAp.pure(())
    case other                             => FreeAp.lift(Coproduct.rightc(other))
  }
~~~~~~~~

Saat ini, kita mempunyai dua program dan harus menggabungkan keduanya. Harap
diingat bahwa sintaks `*>` dari `Apply`

{lang="text"}
~~~~~~~~
  val patched = batch(gathered) *> freeap.foldMap(nostart)
~~~~~~~~

Dan menggabungkannya dalam sebuah metoda:

{lang="text"}
~~~~~~~~
  def optimise[A](orig: FreeAp[Orig, A]): FreeAp[Extended, A] =
    (batch(orig.analyze(gather)) *> orig.foldMap(nostart))
~~~~~~~~

Demikian! Kita meng-`.optimise` tiap kali kita memanggil `act` pada ikalan utama
kita yang hanya berupa pekerjaan pertukangan.


### `Coyoneda` (`Functor`)

Dinamai menggunakan nama dari matematikawan Nobuo Yoneda, kita dapat membuat
sebuah struktur data `Functor` untuk semua aljabar `S[_]`

{lang="text"}
~~~~~~~~
  sealed abstract class Coyoneda[S[_], A] {
    def run(implicit S: Functor[S]): S[A] = ...
    def trans[G[_]](f: F ~> G): Coyoneda[G, A] = ...
    ...
  }
  object Coyoneda {
    implicit def functor[S[_], A]: Functor[Coyoneda[S, A]] = ...
  
    private final case class Map[F[_], A, B](fa: F[A], f: A => B) extends Coyoneda[F, A]
    def apply[S[_], A, B](sa: S[A])(f: A => B) = Map[S, A, B](sa, f)
    def lift[S[_], A](sa: S[A]) = Map[S, A, A](sa, identity)
    ...
  }
~~~~~~~~

dan juga ada versi kontravariannya

{lang="text"}
~~~~~~~~
  sealed abstract class ContravariantCoyoneda[S[_], A] {
    def run(implicit S: Contravariant[S]): S[A] = ...
    def trans[G[_]](f: F ~> G): ContravariantCoyoneda[G, A] = ...
    ...
  }
  object ContravariantCoyoneda {
    implicit def contravariant[S[_], A]: Contravariant[ContravariantCoyoneda[S, A]] = ...
  
    private final case class Contramap[F[_], A, B](fa: F[A], f: B => A)
      extends ContravariantCoyoneda[F, A]
    def apply[S[_], A, B](sa: S[A])(f: B => A) = Contramap[S, A, B](sa, f)
    def lift[S[_], A](sa: S[A]) = Contramap[S, A, A](sa, identity)
    ...
  }
~~~~~~~~

A> Sebutan santai untuk `Coyoneda` adalah *koyo* dan `ContravariantCoyoneda`
A> adalah *kokoyo*. Semakin lama semakin meresap.

API dari koyo cenderung lebih sederhana dari `Free` dan `FreeAp`, dan memperkenankan
sebuah transformasi natural dengan `.trans` dan `.run` (yang menerima sebuah
`Functor` atau `Contravariant`) untuk lepas dari struktur *free*.

Koyo dan kokoyo berguna bila kita ingin menggunakan `.map` atau `.contramap`
kepada sebuah tipe dan kita tahu bahwa kita bisa mengkonversi menjadi sebuah
tipe data yang mempunyai instans `Functor`, namun kita tidak mau benar-benar
melakukannya terlalu dini. Sebagai contoh, kita membuat sebuah `Coyoneda[ISet, ?]`
(harap diingat bahwa `ISet` tidak mempunyai instans `Functor`) untuk menggunakan
metoda lain yang membutuhkan sebuah `Functor`, lalu mengkonversinya menjadi sebuah
`List` di lain waktu.

{lang="text"}
~~~~~~~~
  def liftCoyo[F[_]](implicit I: Ast :<: F) = new Machines[Coyoneda[F, ?]] {
    def getTime = Coyoneda.lift(I.inj(GetTime()))
    ...
  }
  def liftCocoyo[F[_]](implicit I: Ast :<: F) = new Machines[ContravariantCoyoneda[F, ?]] {
    def getTime = ContravariantCoyoneda.lift(I.inj(GetTime()))
    ...
  }
~~~~~~~~

Sebuah optimasi yang kita dapatkan dengan menggunakan `Coyoneda` adalah
*map fusion* (dan *contramap fusion*), yang memperkenankan kita untuk menulis
ulang

{lang="text"}
~~~~~~~~
  xs.map(a).map(b).map(c)
~~~~~~~~

menjadi

{lang="text"}
~~~~~~~~
  xs.map(x => c(b(a(x))))
~~~~~~~~

sehingga menghindari representasi sementara. Sebagai contoh, bila `xs` merupakan
sebuah `List` dengan seribu elemen, kita dapat menghemat dua ribu alokasi objek
karena kita hanya memetakan struktur data satu kali.

Namun, bisa dibilang jauh lebih mudah bila kita membuat perubahan semacam ini
pada fungsi awal secara manual atau menunggu proyek [`scalaz-plugin`](https://github.com/scalaz/scalaz-plugin)
dirilis dan secara otomatis melakukan optimasi semacam ini.


### Efek Elastis

Program sebenarnya hanya data saja: struktur bebas membantu memperjelas hal ini
dan memberikan kita kemampuan untuk mengatur ulang dan mengoptimasi data tersebut.

`Free` lebih istimewa daripada yang terlihat: struktur ini dapat mengurutkan
aljabar dan kelas tipe secara arbiter.

Sebagai contoh, sebuah struktur `free` untuk `MonadState` tersedia. `Ast` dan
`.liftF` lebih rumit daripada biasanya karena kita harus memperhitungkan 
parameter tipe `S` pada `MonadState` dan pewarisan dari `Monad`:

{lang="text"}
~~~~~~~~
  object MonadState {
    sealed abstract class Ast[S, A]
    final case class Get[S]()     extends Ast[S, S]
    final case class Put[S](s: S) extends Ast[S, Unit]
  
    def liftF[F[_], S](implicit I: Ast[S, ?] :<: F) =
      new MonadState[Free[F, ?], S] with BindRec[Free[F, ?]] {
        def get       = Free.liftF(I.inj(Get[S]()))
        def put(s: S) = Free.liftF(I.inj(Put[S](s)))
  
        val delegate         = Free.freeMonad[F]
        def point[A](a: =>A) = delegate.point(a)
        ...
      }
    ...
  }
~~~~~~~~

Hal ini merupakan kesempatan yang bisa digunakan untuk mengoptimasi interpreter.
Sebagai contoh, kita dapat menyimpan `S` pada bidang atomik, bukan pada trampolin
`StateT` berlapis.

Kita dapat membuat sebuah `Ast` dan `.liftF` untuk hampir semua aljabar ataupun
kelas tipe. Satu-satunya batasan adalah `F[_]` tidak muncul sebagai parameter
untuk instruksi apapun, misal, harus dimungkinkan agar aljabar mempunyai instans
`Functor`. Sayangnya, hal ini menghapus kemungkinan `MonadError` dan `Monoid`.

A> Alasan mengapa penyandian *free* tidak berfungsi untuk semua aljabar dan kelas
A> tipe cukup samar.
A>
A> Misalkan bila kita membuat sebuah Ast untuk `MonadError` dengan `F[_]` pada
A> posisi kontravarian, mis. sebagai sebuah parameter.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object MonadError {
A>     sealed abstract class Ast[F[_], E, A]
A>     final case class RaiseError[F[_], E, A](e: E) extends Ast[F, E, A]
A>     final case class HandleError[F[_], E, A](fa: F[A], f: E => F[A]) extends Ast[E, A]
A>   
A>     def liftF[F[_], E](implicit I: Ast[F, E, ?] :<: F): MonadError[F, E] = ...
A>     ...
A>   }
A> ~~~~~~~~
A>
A> Saat kita menginterpretasi sebuah program yang menggunakan `MonadError.Ast`,
A> kita harus membuat koproduk dari instruksi yang ada. Misal, kita memperluas
A> program `Drone`:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   type Ast[a] = Coproduct[MonadError.Ast[Ast, String, ?], Drone.Ast, a]
A> ~~~~~~~~
A>
A> Kode diatas akan gagal kompil karena `Ast` merujuk ke dirinya sendiri!
A>
A> Aljabar yang tidak sepenuhnya terbuat dari penanda kovarian fungtor, mis. `F[_]`
A> pada posisi kembalian, tidak mungkin diinterpretasi karena tipe hasil dari
A> program berupa hasil swa-rujuk. Dan memang, nama *aljabar* yang kita gunakan
A> sebagai akar pada [*F-Algebras*](https://en.wikipedia.org/wiki/F-algebra) dimana
A> F merupakan fungtor.
A>
A> *Terima kasih untuk Edmund Noble yang mengawali diskusi ini.*

Sebagaimana dengan PSA dari sebuah program *free* berkembang, performa mengalami
penurunan karena interpreter harus menyocokkan kepada set instruksi dengan biaya
`O(n)`. Alternatif dari `scalaz.Coproduct` adalah penyandian [iotaz](https://github.com/frees-io/iota)
yang menggunakan struktur data teroptimasi agar dapat bekerja pada `O(1)` dengan
pelepasan dinamis yang menggunakan integer untuk tiap koproduk yang ditetapkan
pada saat kompilasi.

Untuk alasan sejarah, sebuah PSA *free* untuk sebuah aljabar atau kelas tipe
disebut *Penyandian Awal*. Dan, implementasi langsung (misal, dengan `IO`) disebut
*Akhirnya Kosong*. Walau kita telah menjelajahi ide ide menarik dengan `Free`,
secara umum diterima bahwa tanpa-label lebih unggul. Namun, untuk menggunakan
gaya *akhirnya kosong* (tanpa label), kita membutuhkan tipe efek dengan performa
tinggi yang menyediakan semua kelas tipe monad yang kita bahas pada bab ini. Kita
juga harus mampu menjalankan kode `Applicative` kita secara paralel.
Persyaratan semacam ini akan kita bahas selanjutnya


## `Parallel`

Ada dua operasi dengan efek yang hampir selalu kita jalankan secara paralel:

1.  `.map` atas sebuah koleksi dengan efek, mengembalikan sebuah efek. Hal ini
    dapat dicapai dengan `.traverse` yang mendelegasikannya ke `.apply2` milik
    sistem efek tadi.
2.  menjalankan beberapa efek dengan jumlah tetap dengan *operator jerit* `|@|`,
    dan menggabungkan input efek-efek tadi, dan pada akhirnya mendelegasikan
    ke `.apply2`.

Namun, praktik di lapangan, kedua operasi tersebut tidak dijalankan secara paralel
secara default. Alasannya adalah, bila `F[_]` diimplementasikan dengan sebuah
`Monad, maka hukum kombinator turunan untuk `.apply2` harus dipenuhi, yang berisi

{lang="text"}
~~~~~~~~
  @typeclass trait Bind[F[_]] extends Apply[F] {
    ...
    override def apply2[A, B, C](fa: =>F[A], fb: =>F[B])(f: (A, B) => C): F[C] =
      bind(fa)(a => map(fb)(b => f(a, b)))
    ...
  }
~~~~~~~~

Dengan kata lain, **`Monad` dilarang menjalankan efek secara paralel.**

Namun, bila kita mempunyai sebuah `F[_]` yang tidak bersifat monadik, maka konteks
ini bisa saja mengimplementasikan `.apply2` secara paralel. Kita bisa menggunakan
`@@` mekanisme untuk membuat sebuah instans dari `Applicative` untuk `F[_] @@ Paralel`,
yang mempermudah menentukan instans ke alias tipe `Applicative.Par`

{lang="text"}
~~~~~~~~
  object Applicative {
    type Par[F[_]] = Applicative[λ[α => F[α] @@ Tags.Parallel]]
    ...
  }
~~~~~~~~

Program monadik dapat meminta `Par` implisit sebagai tambahan pada `Monad` mereka

{lang="text"}
~~~~~~~~
  def foo[F[_]: Monad: Applicative.Par]: F[Unit] = ...
~~~~~~~~

Sintaks `Traverse` dari Scalaz mendukung paralelisme:

{lang="text"}
~~~~~~~~
  implicit class TraverseSyntax[F[_], A](self: F[A]) {
    ...
    def parTraverse[G[_], B](f: A => G[B])(
      implicit F: Traverse[F], G: Applicative.Par[G]
    ): G[F[B]] = Tag.unwrap(F.traverse(self)(a => Tag(f(a))))
  }
~~~~~~~~

Bila `Applicative.Par[IO]` ada pada cakupan secara implisit, kita dapat memilih
pelangkahan secara berurutan maupun paralel:

{lang="text"}
~~~~~~~~
  val input: IList[String] = ...
  def network(in: String): IO[Int] = ...
  
  input.traverse(network): IO[IList[Int]] // one at a time
  input.parTraverse(network): IO[IList[Int]] // all in parallel
~~~~~~~~

Tidak berbeda jauh, kita dapat memanggil `.parApply` atau `.parTupled` setelah
menggunakan operator jerit

{lang="text"}
~~~~~~~~
  val fa: IO[String] = ...
  val fb: IO[String] = ...
  val fc: IO[String] = ...
  
  (fa |@| fb).parTupled: IO[(String, String)]
  
  (fa |@| fb |@| fc).parApply { case (a, b, c) => a + b + c }: IO[String]
~~~~~~~~

Harap diperhatikan bahwa saat kita mempunyai program `Applicative`, seperti

{lang="text"}
~~~~~~~~
  def foo[F[_]: Applicative]: F[Unit] = ...
~~~~~~~~

kita dapat menggunakan `F[A] @@ Parallel` sebagai konteks dari program kita dan
kita mendapatkan paralelisme sebagai perilaku bawaan untuk `.traverse` dan `|@|`.
Konversi antara operasi mentah dan `@@ Paralel` dari `F[_]` harus ditangani secara
manual pada kode bantuan yang bisa melelahkan. Sehingga, akan lebih mudah bila
langsung meminta bentuk `Applicative`

{lang="text"}
~~~~~~~~
  def foo[F[_]: Applicative: Applicative.Par]: F[Unit] = ...
~~~~~~~~


### Melanggar Hukum

Kita dapat mengambil pendekatan yang lebih berani terhadap paralelisme: dengan
tidak menaati hukum yang menyatakan bahwa `.apply2` harus berurutan untuk `Monad`.
Pendekatan ini sangat kontroversial, namun bekerja dengan sangat baik untuk
kebanyakan aplikasi di dunia nyata. Pertama, kita harus mengaudit basis kode
kita (termasuk ketergantungan pihak ketiga) untuk memastikan bahwa tidak ada
yang menggunakan hukum dari `.apply2`.

Kita bungkus `IO`

{lang="text"}
~~~~~~~~
  final class MyIO[A](val io: IO[A]) extends AnyVal
~~~~~~~~

dan sediakan implementasi buatan kita sendiri untuk `Monad` yang menjalakan
`.apply` secara paralel dengan mendelegasikan ke sebuah instans `@@ Parallel`

{lang="text"}
~~~~~~~~
  object MyIO {
    implicit val monad: Monad[MyIO] = new Monad[MyIO] {
      override def apply2[A, B, C](fa: MyIO[A], fb: MyIO[B])(f: (A, B) => C): MyIO[C] =
        Applicative[IO.Par].apply2(fa.io, fb.io)(f)
      ...
    }
  }
~~~~~~~~

Sekarang kita bisa menggunakan `MyIO` sebagai konteks aplikasi kita sebagai
pengganti `IO` dan **mendapatkan implementasi paralelisme secara default**.

A> Pelapisan sebuah tipe yang sudah ada dan menyediakan instans kelas tipe buatan
A> sendiri dikenal sebagai *newtyping*.
A>
A> `@@` dan *newtyping* bersifat komplementer: `@@` memperkenankan kita untuk
A> meminta varian kelas tipe spesifik pada model domain kita, dimana *newtyping*
A> memperkenankan kita untuk mendefinisikan instans pada implementasi. Keduanya
A> merupakan hal yang sama, namun memiliki titip sisip yang berbeda.
A>
A> Makro `@newtype` oleh [Carry Robbins](https://github.com/estatico/scala-newtype)
A> mempunyai representasi waktu jalan yang teroptimasi (lebih efisien bila dibandingkan
A> dengan `extends AnyVal`), yang mempermudah pendelegasian kelas tipe yang
A> tidak ingin kita utak atik. Sebagai contoh, kita dapat menyesuaikan `Monad`
A> namun mendelegasikan `Plus`:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   @newtype class MyIO[A](io: IO[A])
A>   object MyIO {
A>     implicit val monad: Monad[MyIO] = ...
A>     implicit val plus: Plus[MyIO] = derived
A>   }
A> ~~~~~~~~

Agar lebih lengkap: sebuah implementsai naif dan tidak efisien dari `Applicative.Par`
untuk `IO` sederhana kita dapat menggunakan `Future`:

{lang="text"}
~~~~~~~~
  object IO {
    ...
    type Par[a] = IO[a] @@ Parallel
    implicit val ParApplicative = new Applicative[Par] {
      override def apply2[A, B, C](fa: =>Par[A], fb: =>Par[B])(f: (A, B) => C): Par[C] =
        Tag(
          IO {
            val forked = Future { Tag.unwrap(fa).interpret() }
            val b      = Tag.unwrap(fb).interpret()
            val a      = Await.result(forked, Duration.Inf)
            f(a, b)
          }
        )
  }
~~~~~~~~

dan karena sebuah [kutu](https://github.com/scala/bug/issues/10954) pada kompilator
Scala yang memperlakukan semua instans `@@` sebagai objek yatim, kita harus
secara tersurat mengimpor yang tersirat:

{lang="text"}
~~~~~~~~
  import IO.ParApplicative
~~~~~~~~

Pada bagian akhir bab ini, kita akan melihat bagaimana `IO` Scalaz diimplementasikan
sebenar-benarnya.


## `IO`

`IO` Scalaz merupakan konstruk pemrograman asinkronus yang paling cepat pada
ekosistem Scala: hampir 50 kali lebih cepat bila dibandingkan dengan `Future`.
`IO` merupakan struktur data *free* yang khusus digunakan sebagai monad efek
umum.

{lang="text"}
~~~~~~~~
  sealed abstract class IO[E, A] { ... }
  object IO {
    private final class FlatMap         ... extends IO[E, A]
    private final class Point           ... extends IO[E, A]
    private final class Strict          ... extends IO[E, A]
    private final class SyncEffect      ... extends IO[E, A]
    private final class Fail            ... extends IO[E, A]
    private final class AsyncEffect     ... extends IO[E, A]
    ...
  }
~~~~~~~~

`IO` mempunyai **dua** parameter tipe: `IO` memiliki `Bifunctor` yang memperkenankan
tipe galat agar menjadi ADT spesifik aplikasi. Namun, karena kita berada pada JVM,
dan harus berinteraksi dengan pusaka warisan, sebuah tipe bantuan disediakan
agar dapat menggunakan tipe galat dari pengecualian:

{lang="text"}
~~~~~~~~
  type Task[A] = IO[Throwable, A]
~~~~~~~~


A> `scalaz.ioeffect.IO` merupakan `IO` dengan performa tinggi yang ditulis oleh
A> John de Goes. `IO` ini mempunyai siklus hidup yang terpisah dari pustaka standar
A> Scalaz dan harus ditambah secara manual ke `build.sbt` dengan
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   libraryDependencies += "org.scalaz" %% "scalaz-ioeffect" % "2.10.1"
A> ~~~~~~~~
A>
A> Jangan gunakan paket `scalaz-effect` dan `scalaz-concurrency` yang sudah
A> tidak digunakan lagi.
A>
A> Gunakan varian `scalaz.ioeffect` untuk semua kelas tipe dan tipe data.


### Pembuatan

Ada berapa cara untuk membuat `IO` yang meliputi varian blok kode lugas, lundung,
aman, dan tidak aman:

{lang="text"}
~~~~~~~~
  object IO {
    // eager evaluation of an existing value
    def now[E, A](a: A): IO[E, A] = ...
    // lazy evaluation of a pure calculation
    def point[E, A](a: =>A): IO[E, A] = ...
    // lazy evaluation of a side-effecting, yet Total, code block
    def sync[E, A](effect: =>A): IO[E, A] = ...
    // lazy evaluation of a side-effecting code block that may fail
    def syncThrowable[A](effect: =>A): IO[Throwable, A] = ...
  
    // create a failed IO
    def fail[E, A](error: E): IO[E, A] = ...
    // asynchronously sleeps for a specific period of time
    def sleep[E](duration: Duration): IO[E, Unit] = ...
    ...
  }
~~~~~~~~

dengan konstruktor pembantu `Task`:

{lang="text"}
~~~~~~~~
  object Task {
    def apply[A](effect: =>A): Task[A] = IO.syncThrowable(effect)
    def now[A](effect: A): Task[A] = IO.now(effect)
    def fail[A](error: Throwable): Task[A] = IO.fail(error)
    def fromFuture[E, A](io: Task[Future[A]])(ec: ExecutionContext): Task[A] = ...
  }
~~~~~~~~

Konstruktor yang paling jamak ditemui saat berurusan dengan kode warisan, sampai
saat ini, adalah `Task.apply` dan `Task.fromFuture`:

{lang="text"}
~~~~~~~~
  val fa: Task[Future[String]] = Task { ... impure code here ... }
  
  Task.fromFuture(fa)(ExecutionContext.global): Task[String]
~~~~~~~~

Kita tidak dapat mengumpankan `Future` mentah dengan leluasa karena struktur data
ini dievaluasi secara tegas. Sehingga, kita harus selalu dibuat dalam blok yang
aman.

Harap diperhatikan bahwa `ExecutionContext` **tidak** `implicit`. Dan juga harap
diingat bahwa kita mencadangkan kata kunci `implicit` untuk penurunan kelas tipe
untuk menyederhanakan bahasa: `ExecutionContext` merupakan konfigurasi yang harus
disediakan secara tersurat.


### Menjalankan

Interpreter `IO` disebut sebagai `RTS`, dari *runtime system* (sistem waktu-jalan).
Imlementasi interpreter ini diluar cakupan buku ini, kita akan fokus pada
fitur yang disediakan oleh `IO`.

`IO` hanya merupakan struktur data dan diinterpretasikan *pada akhir waktu*
dengan mengeksten `SafeApp` dan menerapkan `.run`

{lang="text"}
~~~~~~~~
  trait SafeApp extends RTS {
  
    sealed trait ExitStatus
    object ExitStatus {
      case class ExitNow(code: Int)                         extends ExitStatus
      case class ExitWhenDone(code: Int, timeout: Duration) extends ExitStatus
      case object DoNotExit                                 extends ExitStatus
    }
  
    def run(args: List[String]): IO[Void, ExitStatus]
  
    final def main(args0: Array[String]): Unit = ... calls run ...
  }
~~~~~~~~

A> `Void` merupakan tipe yang tidak mempunyai nilai, seperti `scala.Nothing`.
A> Namun, karena kompilator Scala akan menebak `Nothing` walaupun gagal menebak
A> parameter tipe, hal ini menyebabkan pesan galat yang membingungkan. Di sisi
A> lain, `Void` akan menggagalkan diri pada saat kompilasi sehingga tidak ada
A> menyebabkan pesan yang membingungkan.
A>
A> Sebuah tipe galat `Void` berarti bahwa efek yang dibuat **tidak bisa gagal**
A> yang berarti bahwa kita sudah menangani semua galat pada titik ini.

Bila kita mengintegrasikan dengan sebuah sistem warisan dan tidak berkuasa
atas titik awal aplikasi kita, kita dapat mengeksten `RTS` dan mendapatkan akses
pada metoda tak-aman untuk mengevaluasi `IO` pada titik awal agar dapat mengacu
ke kode kita yang berprinsip pada pemrograman fungsional.


### Fitur

`IO` menyediakan instans kelas tipe untuk `Bifunctor`, `MonadError[E, ?]`, `BindRec`,
`Plus`, `MonadPlus` (bila `E` membentuk sebuah `Monoid`), dan `Applicative[IO.Par[E, ?]]`.

Sebagai tambahan atas fungsionalitas dari kelas tipe, ada beberapa implementasi
metoda-metoda spesifik:

{lang="text"}
~~~~~~~~
  sealed abstract class IO[E, A] {
    // retries an action N times, until success
    def retryN(n: Int): IO[E, A] = ...
    // ... with exponential backoff
    def retryBackoff(n: Int, factor: Double, duration: Duration): IO[E, A] = ...
  
    // repeats an action with a pause between invocations, until it fails
    def repeat[B](interval: Duration): IO[E, B] = ...
  
    // cancel the action if it does not complete within the timeframe
    def timeout(duration: Duration): IO[E, Maybe[A]] = ...
  
    // runs `release` on success or failure.
    // Note that IO[Void, Unit] cannot fail.
    def bracket[B](release: A => IO[Void, Unit])(use: A => IO[E, B]): IO[E, B] = ...
    // alternative syntax for bracket
    def ensuring(finalizer: IO[Void, Unit]): IO[E, A] =
    // ignore failure and success, e.g. to ignore the result of a cleanup action
    def ignore: IO[Void, Unit] = ...
  
    // runs two effects in parallel
    def par[B](that: IO[E, B]): IO[E, (A, B)] = ...
    ...
~~~~~~~~

Adalah hal yang memungkinkan bila sebuah `IO` berada pada kondisi *terminated*
yang merepresentasikan tugas yang dimaksudkan untuk dibuang (bukan galat maupun
sukses). Perkakas yang berhubungan dengan terminasi adalah:

{lang="text"}
~~~~~~~~
  ...
    // terminate whatever actions are running with the given throwable.
    // bracket / ensuring is honoured.
    def terminate[E, A](t: Throwable): IO[E, A] = ...
  
    // runs two effects in parallel, return the winner and terminate the loser
    def race(that: IO[E, A]): IO[E, A] = ...
  
    // ignores terminations
    def uninterruptibly: IO[E, A] = ...
  ...
~~~~~~~~


### `Fiber`

Sebuah `IO` bisa saja membuat fiber, abstraksi ringan atas `Thread` JVM.
Kita dapat melakukan `.fork` kepada sebuah `IO` dan melakukan pengawasan
(`.supervise`) terhadap semua fiber yang belum lengkap untuk memastikan bahwa
fiber tersebut akan di-terminasi saat tindakan atas `IO` selesai

{lang="text"}
~~~~~~~~
  ...
    def fork[E2]: IO[E2, Fiber[E, A]] = ...
    def supervised(error: Throwable): IO[E, A] = ...
  ...
~~~~~~~~

Saat kita mempunyai sebuah `Fiber`, kita dapat menggabungkannya kembali ke `IO`
dengan `.join`, atau juga menghentikan dengan menggunakan `interrupt`.

{lang="text"}
~~~~~~~~
  trait Fiber[E, A] {
    def join: IO[E, A]
    def interrupt[E2](t: Throwable): IO[E2, Unit]
  }
~~~~~~~~

Kita dapat menggunakan fiber untuk mencapai bentuk kontrol konkuren optimistis.
Anggap sebuah situasi dimana kita mempunyai `data` yang harus kita analis namun
kita juga harus memvalidasinya. Kita dapat secara optimistis memulai analisis
dan membatalkan tugas bila gagal divalidasi. Dan semua ini dilakukan secara
paralel.

{lang="text"}
~~~~~~~~
  final class BadData(data: Data) extends Throwable with NoStackTrace
  
  for {
    fiber1   <- analysis(data).fork
    fiber2   <- validate(data).fork
    valid    <- fiber2.join
    _        <- if (!valid) fiber1.interrupt(BadData(data))
                else IO.unit
    result   <- fiber1.join
  } yield result
~~~~~~~~

Contoh penggunaan fiber lain adalah saat kita harus melakukan aksi *tembak dan
lupakan*. Sebagai conoth, pencatatan log prioritas rendah melalui jaringan.


### `Promise`

Sebuah *promise* merepresentasikan variable asinkronus yang dapat diatur tepat
satu kali (dengan `complete` atau `error`). Pendengar yang bisa mendapatkan nilai
variabel dengan `get` tidak dibatasi.

{lang="text"}
~~~~~~~~
  final class Promise[E, A] private (ref: AtomicReference[State[E, A]]) {
    def complete[E2](a: A): IO[E2, Boolean] = ...
    def error[E2](e: E): IO[E2, Boolean] = ...
    def get: IO[E, A] = ...
  
    // interrupts all listeners
    def interrupt[E2](t: Throwable): IO[E2, Boolean] = ...
  }
  object Promise {
    def make[E, A]: IO[E, Promise[E, A]] = ...
  }
~~~~~~~~

Secara umum, kita jaran menggunakan `Promise` pada kode aplikasi. `Promise`
merupakan blok bangun untuk *framework* konkurensi tingkat tinggi.

A> Bila sebuah operasi terjamin kesuksesannya, tipe galat `E` dibiarkan sebagai
A> sebuah parameter tipe bebas, sehingga pemanggil dapat menentukan pilihan
A> mereka sendiri.


### `IORef`

`IORef` merupakan ekuivalen dari `IO` untuk variabel atomik tidak tetap.

Kita dapat membaca variabel tersebut dan memiliki beberapa car untuk menulis
atau memutakhirkannya.

{lang="text"}
~~~~~~~~
  final class IORef[A] private (ref: AtomicReference[A]) {
    def read[E]: IO[E, A] = ...
  
    // write with immediate consistency guarantees
    def write[E](a: A): IO[E, Unit] = ...
    // write with eventual consistency guarantees
    def writeLater[E](a: A): IO[E, Unit] = ...
    // return true if an immediate write succeeded, false if not (and abort)
    def tryWrite[E](a: A): IO[E, Boolean] = ...
  
    // atomic primitives for updating the value
    def compareAndSet[E](prev: A, next: A): IO[E, Boolean] = ...
    def modify[E](f: A => A): IO[E, A] = ...
    def modifyFold[E, B](f: A => (B, A)): IO[E, B] = ...
  }
  object IORef {
    def apply[E, A](a: A): IO[E, IORef[A]] = ...
  }
~~~~~~~~

`IORef` merupakan blok bangun lain yang dapat digunakan untuk menyediakan `MonadState`
dengan performa tinggi. Sebagai contoh, buat sebuah *newtype* terspesialisasi
untuk `Task`

{lang="text"}
~~~~~~~~
  final class StateTask[A](val io: Task[A]) extends AnyVal
  object StateTask {
    def create[S](initial: S): Task[MonadState[StateTask, S]] =
      for {
        ref <- IORef(initial)
      } yield
        new MonadState[StateTask, S] {
          override def get       = new StateTask(ref.read)
          override def put(s: S) = new StateTask(ref.write(s))
          ...
        }
  }
~~~~~~~~

Kita dapat menggunakan implementasi teroptimasi `StateMonad` ini pada sebuah
`SafeApp` dimana `.program` kita bergantung pada kelas tipe Pustaka Transformator
Monad:

{lang="text"}
~~~~~~~~
  object FastState extends SafeApp {
    def program[F[_]](implicit F: MonadState[F, Int]): F[ExitStatus] = ...
  
    def run(@unused args: List[String]): IO[Void, ExitStatus] =
      for {
        stateMonad <- StateTask.create(10)
        output     <- program(stateMonad).io
      } yield output
  }
~~~~~~~~

Sebuah aplikasi yang realistis akan menerima beberapa aljabar dan kelas tipe sebagai
masukan.

A> `MonadState` teroptimasi ini dibangun sedemikian rupa sehingga mengaburkan
A> koherensi kelas tipe. Dua instans yang memiliki tipe yangsama bisa saja
A> mengatur keadaan yang berbeda. Adalah hal yang bijak untuk mengisolasi
A> konstruksi dari semua instans seperti itu pada titik-awal aplikasi.


#### `MonadIO`

`MonadIO` yang kita pelajari sebelumnya telah disederhanakan untuk menyembunyikan
parameter `E`. Kelas tipe yang sebenarnya adalah

{lang="text"}
~~~~~~~~
  trait MonadIO[M[_], E] {
    def liftIO[A](io: IO[E, A])(implicit M: Monad[M]): M[A]
  }
~~~~~~~~

dengan perubahan kecil di plat cetak pada pendamping aljabar kita, untuk
mengikutsertakan tambahan `E`: 

{lang="text"}
~~~~~~~~
  trait Lookup[F[_]] {
    def look: F[Int]
  }
  object Lookup {
    def liftIO[F[_]: Monad, E](io: Lookup[IO[E, ?]])(implicit M: MonadIO[F, E]) =
      new Lookup[F] {
        def look: F[Int] = M.liftIO(io.look)
      }
    ...
  }
~~~~~~~~


## Kesimpulan

1.  `Future` cacat, jangan digunakan.
2.  Mengatur keamanan susunan memori dengan `Trampoline`.
3.  Pustaka Transformator Monad (PTM) mengabstraksi efek-efek umum dengan kelas tipe.
4.  Transformator monad menyediakan implementasi default dari PTM.
5.  Struktur data `Free` memperkenankan kita untuk menganalisis, mengoptimasi, dan
    mengetes program kita.
6.  `IO` memberi kita jalan untuk mengimplementasi aljabar sebagai efek dari dunia luar.
7.  `IO` dapat menjalankan efek secara paralel dan merupakan tulang punggung dari
    aplikasi dengan performa tinggi.


# Derivasi Kelas Tipe

Kelas tipe menyediakan fungsionalitas polimorfis untuk aplikasi kita. Namun, untuk
menggunakan sebuah kelas tipe, kita butuh instans kelas tipe tersebut untuk objek
domain bisnis kita.

Pembuatan instans kelas tipe dari instans yang sudah ada dikenal dengan *derivasi
kelas tipe* dan menjadi topik pada bab ini.

Ada empat pendekatan atas derivasi kelas tipe:

1.  Instans manual untuk tiap objek domain. Pendekatan ini tidak mungkin dilakukan
    pada aplikasi nyata karena akan menghasilkan ratusan baris plat cetak untuk
    tiap baris `case class`. Namun, pendekatan ini berguna untuk tujuan pembelajaran
    dan optimasi performa.

2.  Abstrak atas kelas tipe dari kelas tipe Scalaz yang sudah ada. Merupakan pendekatan
    yang digunakan oloh `scalaz-deriving`, menyediakan tes terotomatisasi dan
    derivasi atas produk dan ko-produk.

3.  Makro. Namun, penulisan makro untuk tiap kelas tipe harus dilakukan oleh
    pengembang yang sangat berpengalaman. Untungnya, pustaka [Magnolia](https://github.com/propensive/magnolia)
    yang ditulis oleh Jon Pretty, mengabstraksi makro dengan APA yang sederhana
    dan memusatkan interaksi kompleks kepada kompilator.

4.  Menulis program generik dengan menggunakan pustaka [Shapeless](https://github.com/milessabin/shapeless).
    Mekanisme `implicit` merupakan sub-bahasa pada bahasa Scala dan dapat digunakan
    untuk menulis program pada tingkat tipe.

Pada bab ini, kita akan mempelajari kelas tipe yang semakin rumit dan derivasinya.
Kita akan memulai dengan `scalaz-deriving` sebagai mekanisme paling sesuai dengan
prinsip, mengulangi beberapa pelajaran pada bab 5 mengenai Kelas Tipe Scalaz,
dan Magnolia (paling mudah digunakan), dan diakhiri dengan Shapeless (paling
leluasa) untuk kelas tipe dengan logika derivasi kompleks.


## Contoh Berfungsi

Bab ini akan menunjukkan bagaimana cara mendefinisikan derivasi dari lima
kelas tipe spesifik. Tiap contoh menunjukkan fitur yang dapat digeneralisasi:

{lang="text"}
~~~~~~~~
  @typeclass trait Equal[A]  {
    // type parameter is in contravariant (parameter) position
    @op("===") def equal(a1: A, a2: A): Boolean
  }
  
  // for requesting default values of a type when testing
  @typeclass trait Default[A] {
    // type parameter is in covariant (return) position
    def default: String \/ A
  }
  
  @typeclass trait Semigroup[A] {
    // type parameter is in both covariant and contravariant position (invariant)
    @op("|+|") def append(x: A, y: =>A): A
  }
  
  @typeclass trait JsEncoder[T] {
    // type parameter is in contravariant position and needs access to field names
    def toJson(t: T): JsValue
  }
  
  @typeclass trait JsDecoder[T] {
    // type parameter is in covariant position and needs access to field names
    def fromJson(j: JsValue): String \/ T
  }
~~~~~~~~

A> Ada mahdzab yang berpendapat bahwa format serialisasi, seperti JSON dan XML,
A> **tidak** boleh mempunyai kelas tipe penyandi dan pembaca sandi karena akan
A> menghasilkan dekoherensi kelas tipe (mis., ada kemungkinan lebih dari satu
A> penyandi atau pembaca sandi untuk tipe yang sama). Alternatif yang disediakan
A> adalah dengan menggunakan aljabar dan menghindari penggunaan fitur bahasa
A> `implisit`.
A>
A> Walaupun mungkin untuk menerapkan teknik pada bab ini ke kelas tipe atau
A> derivasi aljabar, penerapan teknik pada derivasi aljabar akan melibatkan
A> plat cetak yang **jauh lebih banyak**. Maka dari itu, kita akan memilih
A> untuk membatasi pembelajaran ini pada penyandi dan pembaca sandi yang koheren.
A> Sebagaimana yang akan kita lihat selanjutnya pada bab ini, derivasi otomatis
A> dari Magnolia dan Shapeless, digabungkan dengan batasan pencarian `implicit`
A> dari kompilator Scala, biasanya berakhir pada dekoherensi kelas tipe.


## `scalaz-deriving`

Pustaka `scalaz-deriving` merupakan perpanjangan dari Scalaz dan dapat ditambahkan
ke `build.sbt` proyek dengan

{lang="text"}
~~~~~~~~
  val derivingVersion = "1.0.0"
  libraryDependencies += "org.scalaz" %% "scalaz-deriving" % derivingVersion
~~~~~~~~

menyediakan kelas tipe baru, yang ditunjukkan dibawah, yang berhubungan dengan
kelas tipe Scalaz 

{width=60%}
![](images/scalaz-deriving-base.png)

A> Pada Scalaz 7.3, `Applicative` dan `Divisible` akan mewarisi dari `InvariantApplicative`

Sebelum kita memulai, berikut merupakan rekap ulang dari kelas tipe utama Scalaz:

{lang="text"}
~~~~~~~~
  @typeclass trait InvariantFunctor[F[_]] {
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B]
  }
  
  @typeclass trait Contravariant[F[_]] extends InvariantFunctor[F] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = contramap(fa)(g)
  }
  
  @typeclass trait Divisible[F[_]] extends Contravariant[F] {
    def conquer[A]: F[A]
    def divide2[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C]
    ...
    def divide22[...] = ...
  }
  
  @typeclass trait Functor[F[_]] extends InvariantFunctor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] = map(fa)(f)
  }
  
  @typeclass trait Applicative[F[_]] extends Functor[F] {
    def point[A](a: =>A): F[A]
    def apply2[A,B,C](fa: =>F[A], fb: =>F[B])(f: (A, B) => C): F[C] = ...
    ...
    def apply12[...]
  }
  
  @typeclass trait Monad[F[_]] extends Functor[F] {
    @op(">>=") def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  @typeclass trait MonadError[F[_], E] extends Monad[F] {
    def raiseError[A](e: E): F[A]
    def emap[A, B](fa: F[A])(f: A => S \/ B): F[B] = ...
    ...
  }
~~~~~~~~


### Jangan Mengulang-Ulang

Cara paling sederhana untuk menderivasi sebuah kelas tipe adalah menggunakan
ulang derivasi yang sudah ada.

Kelas tipe `Equal` mempunyai instans `Contravariant[Equal]` yang menyediakan
`.contramap`:

{lang="text"}
~~~~~~~~
  object Equal {
    implicit val contravariant = new Contravariant[Equal] {
      def contramap[A, B](fa: Equal[A])(f: B => A): Equal[B] =
        (b1, b2) => fa.equal(f(b1), f(b2))
    }
    ...
  }
~~~~~~~~

Sebagai pengguna dari `Equal`, kita dapat menggunakan `.contramap` untuk tipe
data parameter tunggal kita. Harap diingat bahwa instans kelas tipe masuk pada
pendamping tipe data agar masuk pada cakupan implisit mereka:

{lang="text"}
~~~~~~~~
  final case class Foo(s: String)
  object Foo {
    implicit val equal: Equal[Foo] = Equal[String].contramap(_.s)
  }
  
  scala> Foo("hello") === Foo("world")
  false
~~~~~~~~

Namun, tidak semua kelas tipe mempunyai instans `Contravariant`. Terlebih lagi,
kelas tipe dengan parameter tipe pada posisi kovarian mungkin malah memiliki
instans `Functor`:

{lang="text"}
~~~~~~~~
  object Default {
    def instance[A](d: =>String \/ A) = new Default[A] { def default = d }
    implicit val string: Default[String] = instance("".right)
  
    implicit val functor: Functor[Default] = new Functor[Default] {
      def map[A, B](fa: Default[A])(f: A => B): Default[B] = instance(fa.default.map(f))
    }
    ...
  }
~~~~~~~~

Kita dapat menderivasi sebuah `Default[Foo]`

{lang="text"}
~~~~~~~~
  object Foo {
    implicit val default: Default[Foo] = Default[String].map(Foo(_))
    ...
  }
~~~~~~~~

Bila sebuah kelas tipe mempunyai parameter pada posisi kovarian dan kontravarian,
seperti halnya `Semigroup`, kelas tipe ini mungkin menyediakan sebuah instans
`IntravariantFunctor`

{lang="text"}
~~~~~~~~
  object Semigroup {
    implicit val invariant = new InvariantFunctor[Semigroup] {
      def xmap[A, B](ma: Semigroup[A], f: A => B, g: B => A) = new Semigroup[B] {
        def append(x: B, y: =>B): B = f(ma.append(g(x), g(y)))
      }
    }
    ...
  }
~~~~~~~~

dan kita akan memanggil `.xmap`

{lang="text"}
~~~~~~~~
  object Foo {
    implicit val semigroup: Semigroup[Foo] = Semigroup[String].xmap(Foo(_), _.s)
    ...
  }
~~~~~~~~

Secara umum, jauh lebih mudah untuk menggunakan `.xmap` bila dibandingkan dengan
menggunakan `.map` atau `.contramap`:

{lang="text"}
~~~~~~~~
  final case class Foo(s: String)
  object Foo {
    implicit val equal: Equal[Foo]         = Equal[String].xmap(Foo(_), _.s)
    implicit val default: Default[Foo]     = Default[String].xmap(Foo(_), _.s)
    implicit val semigroup: Semigroup[Foo] = Semigroup[String].xmap(Foo(_), _.s)
  }
~~~~~~~~

A> The `@xderiving` annotation automatically inserts `.xmap` boilerplate. Add the
A> following to `build.sbt`
A>
A> Anotasi `@xderiving` secara otomatis menyisipkan plat cetak `.xmap`. Tambahkan
A> potongan berikut pada `build.sbt`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   addCompilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion)
A>   libraryDependencies += "org.scalaz" %% "deriving-macro" % derivingVersion % "provided"
A> ~~~~~~~~
A> 
A> and use it as
A>
A> dan gunakan seperti
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   @xderiving(Equal, Default, Semigroup)
A>   final case class Foo(s: String)
A> ~~~~~~~~


### `MonadError`

Biasanya, sesuatu yang *menulis* dari sebuah nilai polimorfis mempunyai sebuah
`Contravariant`. Dan, sesuatu yang *membaca* ke sebuah nilai polimorfis mempunyai
sebuah `Functor`. Namun, sangat wajar bila pembacaan dapat gagal. Sebagai contoh,
bila kita mempunyai sebuah `String` default, bukan berarti kita tinggal menurunkan
`String Refined NonEmpty` darinya 

{lang="text"}
~~~~~~~~
  import eu.timepit.refined.refineV
  import eu.timepit.refined.api._
  import eu.timepit.refined.collection._
  
  implicit val nes: Default[String Refined NonEmpty] =
    Default[String].map(refineV[NonEmpty](_))
~~~~~~~~

yang gagal dikompilasi dengan galat

{lang="text"}
~~~~~~~~
  [error] default.scala:41:32: polymorphic expression cannot be instantiated to expected type;
  [error]  found   : Either[String, String Refined NonEmpty]
  [error]  required: String Refined NonEmpty
  [error]     Default[String].map(refineV[NonEmpty](_))
  [error]                                          ^
~~~~~~~~

Mohon diingat bahwa pada bab 4.1, `refineV` mengembalikan sebuah `Either`,
sesuai dengan apa yang telah kompilator peringatkan.

Sebaga penulis dari kelas tipe `Default`, kita dapat berbuat lebih daripada
`Functor` dan menyediakan sebuah `MonadError[Default, String]`:

{lang="text"}
~~~~~~~~
  implicit val monad = new MonadError[Default, String] {
    def point[A](a: =>A): Default[A] =
      instance(a.right)
    def bind[A, B](fa: Default[A])(f: A => Default[B]): Default[B] =
      instance((fa >>= f).default)
    def handleError[A](fa: Default[A])(f: String => Default[A]): Default[A] =
      instance(fa.default.handleError(e => f(e).default))
    def raiseError[A](e: String): Default[A] =
      instance(e.left)
  }
~~~~~~~~

Setelah mendapatkan akses ke sintaks `.emap` dan dapat menderivasi tipe *refined*

{lang="text"}
~~~~~~~~
  implicit val nes: Default[String Refined NonEmpty] =
    Default[String].emap(refineV[NonEmpty](_).disjunction)
~~~~~~~~

Nyatanya, kita dapat menyediakan aturan derivasi untuk semua tipe terrefinasi

{lang="text"}
~~~~~~~~
  implicit def refined[A: Default, P](
    implicit V: Validate[A, P]
  ): Default[A Refined P] = Default[A].emap(refineV[P](_).disjunction)
~~~~~~~~

dimana `Validate` berasal dari pustaka `refined` dan dibutuhkan oleh `refineV`.

A> Ekstensi `refined-scalaz` untuk pustaka `refined` menyediakan dukungan untuk
A> menderivasi semua kelas tipe untuk tipe terrefinasi dengan impor berikut
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   import eu.timepit.refined.scalaz._
A> ~~~~~~~~
A>
A> bila ada `Contravariant` atau `MonadError[?, String]` pada cakupan implisit.
A>
A> Namun, karena [batasan dari kompilator Scala](https://github.com/scala/bug/issues/10753), hal ini jarang
A> berfungsi di praktik pada lapangan dan kita harus menulis derivasi
A> tiap kelas tipe `implicit def refined`.

Kita juga dapat menggunakan `.emap` untuk menderivasi sebuah pembaca sandi
`Int` dari sebuah `Long` dengan perlindungan atas metoda non-total `.toInt`
dari pustaka standar.

{lang="text"}
~~~~~~~~
  implicit val long: Default[Long] = instance(0L.right)
  implicit val int: Default[Int] = Default[Long].emap {
    case n if (Int.MinValue <= n && n <= Int.MaxValue) => n.toInt.right
    case big => s"$big does not fit into 32 bits".left
  }
~~~~~~~~

Sebagai penulis dari kelas tipe `Default`, kita mungkin ingin mempertimbangkan
ulang desain APA kita sehingga tidak akan gagal, misalkan dengan menggunakan
penanda tipe berikut

{lang="text"}
~~~~~~~~
  @typeclass trait Default[A] {
    def default: A
  }
~~~~~~~~

Kita tidak akan dapat mendefinisikan sebuah `MonadError`, sehingga kita terpaksa
untuk menyediakan instans yang selalu sukses. Hal ini akan menghasilkan plat cetak
yang lebih banyak sebagai ganti atas keamanan tipe. Namun, kita akan tetap
menggunakan `String \/ A` sebagai nilai kembalian karena ini merupakan contoh
yang lebih umum.

### `.fromIso`

Semua kelas tipe di Scalaz mempunyai sebuah metoda pada objek pendampingnya
dengan sebuah penanda yang mirip sebagai berikut:

{lang="text"}
~~~~~~~~
  object Equal {
    def fromIso[F, G: Equal](D: F <=> G): Equal[F] = ...
    ...
  }
  
  object Monad {
    def fromIso[F[_], G[_]: Monad](D: F <~> G): Monad[F] = ...
    ...
  }
~~~~~~~~

Potongan diatas berarti bila kita mempunyai sebuah tipe `F` dan sebuah cara untuk
mengkonversinya menjadi sebuah `G` yang mempunyai sebuah instans, kita dapat
memanggil `Equal.fromIso` untuk mendapatkan instans dari `F`.

Sebagai contoh, sebagai pengguna kelas tipe, bila kita mempunyai tipe data `Bar`,
kita dapat mendefinisikan sebuah isomorfisme ke `(String, Int)`

{lang="text"}
~~~~~~~~
  import Isomorphism._
  
  final case class Bar(s: String, i: Int)
  object Bar {
    val iso: Bar <=> (String, Int) = IsoSet(b => (b.s, b.i), t => Bar(t._1, t._2))
  }
~~~~~~~~

dan menderivasi `Equal[Bar]` karena sudah ada `Equal` untuk semua tuple:

{lang="text"}
~~~~~~~~
  object Bar {
    ...
    implicit val equal: Equal[Bar] = Equal.fromIso(iso)
  }
~~~~~~~~

Mekanisme `.fromIso` juga dapat membantu kita sebagai penulis kelas tipe.
Sebagai contoh, `Default` yang mempunyai penanda tipe utama dengan bentuk `Unit => F[A]`.
Metoda `default` kita sebenarnya isomorfik terhadap `Kleisli[F, Unit, A]`,
atau transformator monad `ReaderT`.

Karena `Kleisli` sudah menyediakan sebuah `MonadError` (bila `F` sudah mempunyainya),
kita dapat menderivasi `MonadError[Default, String]` dengan membuat sebuah
isomorfisme antara `Default` dan `Kleisli`:

{lang="text"}
~~~~~~~~
  private type Sig[a] = Unit => String \/ a
  private val iso = Kleisli.iso(
    λ[Sig ~> Default](s => instance(s(()))),
    λ[Default ~> Sig](d => _ => d.default)
  )
  implicit val monad: MonadError[Default, String] = MonadError.fromIso(iso)
~~~~~~~~

memberikan kita `.map`, `.xmap`, dan `.emap` yang sudah kita gunakan selama ini.


### `Divisible` dan `Applicative`

Untuk menderivasi `Equal` pada kelas dengan dua parameter kita, kita akan
menggunakan ulang instans yang disediakan oleh Scalaz untuk tuple. Namun, dari
mana instans tuple itu berasal?

Kelas tipe yang lebih spesifik untuk `Contravariant` adalah `Divisible`. `Equal`
mempunyai sebuah instans:

{lang="text"}
~~~~~~~~
  implicit val divisible = new Divisible[Equal] {
    ...
    def divide[A1, A2, Z](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => (A1, A2)
    ): Equal[Z] = { (z1, z2) =>
      val (s1, s2) = f(z1)
      val (t1, t2) = f(z2)
      a1.equal(s1, t1) && a2.equal(s2, t2)
    }
    def conquer[A]: Equal[A] = (_, _) => true
  }
~~~~~~~~

A> Saat mengimplementasikan `Divisible`, kompilator akan meminta kita untuk
A> menyediakan `.contramap`, yang dapat kita penuhi dengan sebuah implmentasi
A> teroptimasi atau dengan kombinator terderivasi berikut:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
A>     divide2(conquer[Unit], fa)(c => ((), f(c)))
A> ~~~~~~~~
A>
A> Kombinator ini ditambahkan ke `Divisible` pada Scalaz 7.3.

Dan dari `divide2`, `Divisible` mampu membangun derivasi sampai ke `divide22`.
Kita dapat memanggil metoda ini langsung ke tipe data kita:

{lang="text"}
~~~~~~~~
  final case class Bar(s: String, i: Int)
  object Bar {
    implicit val equal: Equal[Bar] =
      Divisible[Equal].divide2(Equal[String], Equal[Int])(b => (b.s, b.i))
  }
~~~~~~~~

Ekuivalen untuk parameter tipe ini pada posisi kovarian adalah `Applicative`:

{lang="text"}
~~~~~~~~
  object Bar {
    ...
    implicit val default: Default[Bar] =
      Applicative[Default].apply2(Default[String], Default[Int])(Bar(_, _))
  }
~~~~~~~~

Namun, kita harus berhati hati agar kita tidak melanggar hukum kelas tipe
saat kita mengimplementasikan `Divisible` atau `Applicative`. Terlebih lagi,
sangat mudah untuk melanggar *hukum komposisi* yang menyatakan bahwwa kedua
alur-kode ini harus menghasilkan keluaran yang sama

-   `divide2(divide2(a1, a2)(dupe), a3)(dupe)`
-   `divide2(a1, divide2(a2, a3)(dupe))(dupe)`
-   untuk semua `dupe: A => (A, A)`

dengan hukum yang sama untuk `Applicative`.

Misalk, `JsEncoder` dan instans `Divisible` yang diajukan

{lang="text"}
~~~~~~~~
  new Divisible[JsEncoder] {
    ...
    def divide[A, B, C](fa: JsEncoder[A], fb: JsEncoder[B])(
      f: C => (A, B)
    ): JsEncoder[C] = { c =>
      val (a, b) = f(c)
      JsArray(IList(fa.toJson(a), fb.toJson(b)))
    }
  
    def conquer[A]: JsEncoder[A] = _ => JsNull
  }
~~~~~~~~

Pada satu sisi dari hukum komposisi, untuk sebuah input `String`, kita akan
mendapatkan

{lang="text"}
~~~~~~~~
  JsArray([JsArray([JsString(hello),JsString(hello)]),JsString(hello)])
~~~~~~~~

dan pada sisi lain

{lang="text"}
~~~~~~~~
  JsArray([JsString(hello),JsArray([JsString(hello),JsString(hello)])])
~~~~~~~~

yang berbeda. Kita dapat bereksperimen dengan implementasi `divide`, namun
tidak akan pernah memenuhi hukum komposisi untuk semua input.

Hal ini mengakibatkan kita tidak dapat menyediakan sebuah `Divisible[JsEncoder]
karena akan melanggar hukum matematika dan membatalkan semua asumsi yang digunakan
oleh pengguna `Divisible`.

Untuk membantu mengetes hukum, kelas tipe Scalaz berisi versi terkodifikasi
dari hukum hukum atas kelas tipe itu sendiri. Kita dapat menulis tes terotomatis,
memastikan bahwa hukum tersebut terlanggar, dan mengingatkan kita bahwa:

{lang="text"}
~~~~~~~~
  val D: Divisible[JsEncoder] = ...
  val S: JsEncoder[String] = JsEncoder[String]
  val E: Equal[JsEncoder[String]] = (p1, p2) => p1.toJson("hello") === p2.toJson("hello")
  assert(!D.divideLaw.composition(S, S, S)(E))
~~~~~~~~

Di sisi lain, sebuah tes `JsDecoder` memenuhi huku komposisi `Applicative`

{lang="text"}
~~~~~~~~
  final case class Comp(a: String, b: Int)
  object Comp {
    implicit val equal: Equal[Comp] = ...
    implicit val decoder: JsDecoder[Comp] = ...
  }
  
  def composeTest(j: JsValue) = {
    val A: Applicative[JsDecoder] = Applicative[JsDecoder]
    val fa: JsDecoder[Comp] = JsDecoder[Comp]
    val fab: JsDecoder[Comp => (String, Int)] = A.point(c => (c.a, c.b))
    val fbc: JsDecoder[((String, Int)) => (Int, String)] = A.point(_.swap)
    val E: Equal[JsDecoder[(Int, String)]] = (p1, p2) => p1.fromJson(j) === p2.fromJson(j)
    assert(A.applyLaw.composition(fbc, fab, fa)(E))
  }
~~~~~~~~

untuk beberapa data tes

{lang="text"}
~~~~~~~~
  composeTest(JsObject(IList("a" -> JsString("hello"), "b" -> JsInteger(1))))
  composeTest(JsNull)
  composeTest(JsObject(IList("a" -> JsString("hello"))))
  composeTest(JsObject(IList("b" -> JsInteger(1))))
~~~~~~~~

Sekarang, kita cukup yakin bathwa `MonadError` yang telah kita derivasi memenuhi
hukum hukum yang berlaku.

Namun, bukan berarti bila kita lulus tes untuk set data kecil, hukum tidak terpenuhi.
Kita harus menalar implementasi sampai tuntas agar kita yakin bahwa implementasi
ini **seharusnya** sudah memenuhi hukum yang berlaku, dan mencoba permasalahan
di luar batas normal yang bisa saja gagal.

Salah satu cara untuk menghasilkan data tes yang bervariasi adalah dengan menggunakan
pustaka [scalacheck](https://gihtub.com/rickynils/scalacheck) yang menyediakan
kelas tipe `Arbitrary` yang dapat terintegrasi ke kebanyakan kerangka testing
untuk mengulang sebuah test dengan data yang dihasilkan secara acak.

Pustaka `jsonformat` menyediakan sebuah `Arbitrary[JsValue]` (dan semua orang
harus menyediakan `Arbitrary` pada DTA mereka!) memperkenankan kita untuk menggunakan
fitur `forall` dari Scalatest:

{lang="text"}
~~~~~~~~
  forAll(SizeRange(10))((j: JsValue) => composeTest(j))
~~~~~~~~

Tes ini memberikan kita lebih percaya pada kelas tipe kita memenuhi hukum komposisi
`Applicative`. Dengan memeriksa seuma hukum pada `Divisible` dan `MonadError`
kita juga mendapat **banyak** tes secara cuma-cuma.

A> Kita harus membatasi `forAll` dengan `SizeRange` `10` yang membatasi
A> `JsObject` dan `JsArray` sampai maksimum 10 elemen saja. Hal ini untuk
A> menghindari lubernya karena jumlah yang lebih besar dapat menghasilkan
A> dokumen JSON yang sangat besar.


### `Decidable` dan `Alt`

Bila `Divisible` dan `Applicative` memberikan kita derivasi kelas tipe untuk
produk (dibangun dari tuple) `Decidable` dan `Alt` memberikan kita ko-prooduk
yang dibangun dari disjungsi berlapis:

{lang="text"}
~~~~~~~~
  @typeclass trait Alt[F[_]] extends Applicative[F] with InvariantAlt[F] {
    def alt[A](a1: =>F[A], a2: =>F[A]): F[A]
  
    def altly1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = ...
    def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] = ...
    def altly3 ...
    def altly4 ...
    ...
  }
  
  @typeclass trait Decidable[F[_]] extends Divisible[F] with InvariantAlt[F] {
    def choose1[Z, A1](a1: =>F[A1])(f: Z => A1): F[Z] = ...
    def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => A1 \/ A2): F[Z] = ...
    def choose3 ...
    def choose4 ...
    ...
  }
~~~~~~~~

Empat kelas tipe utama mempunyai penanda simetris:

| Typeclass     | method    | given          | signature         | returns |
|------------- |--------- |-------------- |----------------- |------- |
| `Applicative` | `apply2`  | `F[A1], F[A2]` | `(A1, A2) => Z`   | `F[Z]`  |
| `Alt`         | `altly2`  | `F[A1], F[A2]` | `(A1 \/ A2) => Z` | `F[Z]`  |
| `Divisible`   | `divide2` | `F[A1], F[A2]` | `Z => (A1, A2)`   | `F[Z]`  |
| `Decidable`   | `choose2` | `F[A1], F[A2]` | `Z => (A1 \/ A2)` | `F[Z]`  |

mendukung kovarian produk, kovarian koproduk, kontravarian produk, dan kontravarian
koproduk.

Kita dapat menulis sebuah instans `Decidable[Equal]` yang memperkenankan kita
untuk menderivasi `Equal` untuk semua TDA!

{lang="text"}
~~~~~~~~
  implicit val decidable = new Decidable[Equal] {
    ...
    def choose2[Z, A1, A2](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => A1 \/ A2
    ): Equal[Z] = { (z1, z2) =>
      (f(z1), f(z2)) match {
        case (-\/(s), -\/(t)) => a1.equal(s, t)
        case (\/-(s), \/-(t)) => a2.equal(s, t)
        case _ => false
      }
    }
  }
~~~~~~~~

Untuk TDA

{lang="text"}
~~~~~~~~
  sealed abstract class Darth { def widen: Darth = this }
  final case class Vader(s: String, i: Int)  extends Darth
  final case class JarJar(i: Int, s: String) extends Darth
~~~~~~~~

dimana produk (`Vader` dan `JarJar`) mempunyai instans `Equal`

{lang="text"}
~~~~~~~~
  object Vader {
    private val g: Vader => (String, Int) = d => (d.s, d.i)
    implicit val equal: Equal[Vader] = Divisible[Equal].divide2(Equal[String], Equal[Int])(g)
  }
  object JarJar {
    private val g: JarJar => (Int, String) = d => (d.i, d.s)
    implicit val equal: Equal[JarJar] = Divisible[Equal].divide2(Equal[Int], Equal[String])(g)
  }
~~~~~~~~

kita dapat menderivasi persamaan untuk semua TDA

{lang="text"}
~~~~~~~~
  object Darth {
    private def g(t: Darth): Vader \/ JarJar = t match {
      case p @ Vader(_, _)  => -\/(p)
      case p @ JarJar(_, _) => \/-(p)
    }
    implicit val equal: Equal[Darth] = Decidable[Equal].choose2(Equal[Vader], Equal[JarJar])(g)
  }
  
  scala> Vader("hello", 1).widen === JarJar(1, "hello).widen
  false
~~~~~~~~

A> Scalaz 7.2 tidak menyediakan instans `Decidable[Equal]` secara otomatis karena
A> instans tersebut merupakan tambahan susulan.

Kelas tipe yang mempunyai `Applicative` berhak memiliki sebuah instans dari `Alt`.
Bila kita ingin menggunakan trik `Kleisli.iso`, kita dapat mengeksten `IsomorphismMonadError`
dan mencampurnya pada `Alt` dan meningkatkan `MonadError[Default, String]` agar
memililki `Alt[Default]`:

{lang="text"}
~~~~~~~~
  private type K[a] = Kleisli[String \/ ?, Unit, a]
  implicit val monad = new IsomorphismMonadError[Default, K, String] with Alt[Default] {
    override val G = MonadError[K, String]
    override val iso = ...
  
    def alt[A](a1: =>Default[A], a2: =>Default[A]): Default[A] = instance(a1.default)
  }
~~~~~~~~

A> Nilai primitif dari `Alt` adalah `alt`, sebagaimana primitif dari `Applicative`
A> yang berupa `ap`, namun seringkali lebih masuk akal untuk menggunakan `altly2`
A> dan `apply2` dan primitif dengan penimpaan berikut:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   override def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B] =
A>     apply2(fa, f)((a, abc) => abc(a))
A>   
A>   override def alt[A](a1: =>F[A], a2: =>F[A]): F[A] = altly2(a1, a2) {
A>     case -\/(a) => a
A>     case \/-(a) => a
A>   }
A> ~~~~~~~~
A>
A> Jangan lupa untuk mengimplementasikan `apply2` dan `altly2` atau akan ada
A> ikalan tak-hingga pada saat waktu-jalan.

Memperkenankan kita untuk menderivasi `Default[Darth]`

{lang="text"}
~~~~~~~~
  object Darth {
    ...
    private def f(e: Vader \/ JarJar): Darth = e.merge
    implicit val default: Default[Darth] =
      Alt[Default].altly2(Default[Vader], Default[JarJar])(f)
  }
  object Vader {
    ...
    private val f: (String, Int) => Vader = Vader(_, _)
    implicit val default: Default[Vader] =
      Alt[Default].apply2(Default[String], Default[Int])(f)
  }
  object JarJar {
    ...
    private val f: (Int, String) => JarJar = JarJar(_, _)
    implicit val default: Default[JarJar] =
      Alt[Default].apply2(Default[Int], Default[String])(f)
  }
  
  scala> Default[Darth].default
  \/-(Vader())
~~~~~~~~

Kembali ke kelas tipe `scalaz-deriving`, orangtua invarian dari `Alt` dan `Decidable`
adalah:

{lang="text"}
~~~~~~~~
  @typeclass trait InvariantApplicative[F[_]] extends InvariantFunctor[F] {
    def xproduct0[Z](f: =>Z): F[Z]
    def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = ...
    def xproduct2 ...
    def xproduct3 ...
    def xproduct4 ...
  }
  
  @typeclass trait InvariantAlt[F[_]] extends InvariantApplicative[F] {
    def xcoproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = ...
    def xcoproduct2 ...
    def xcoproduct3 ...
    def xcoproduct4 ...
  }
~~~~~~~~

mendukung kelas tipe dengan `InvarianFunctor` seperti `Monad` dan `Semigroup`


### Arity Arbiter dan `@deriving`

Ada dua masalah dengan `InvariantApplicative` dan `InvariantAlt`:

1.  keduanya hanya mendukung produk dari 4 bidang dan koproduk dari 4 catatan.
2.  ada **banyak** plat cetak pada tipe data pendamping.

Pada bagian ini, kita akan menyelesaikan kedua permasalahan tersebut dengan kelas
tipe tambahan yang diperkenalkan oleh `scalaz-deriving`

{width=75%}
![](images/scalaz-deriving.png)

Empat tipe kelas utama kita, `Applicative`, `Divisible`, `Alt`, dan `Decidable`,
diperluas menjadi *arity* arbiter menggunakan pustaka [iotaz](https://github.com/frees-io/iota),
maka dari itu mendapatkan akhiran `z`.

Pustaka iotaz mempunyai tiga tipe utama:

-   `TList` which describes arbitrary length chains of types
-   `Prod[A <: TList]` for products
-   `Cop[A <: TList]` for coproducts

-   `TList` yang mendeskripsikan panjang rantai tipe arbiter
-   `Prod[A <: TList]` untuk produk
-   `Cop[A <: TList]` untuk koproduk

Sebagai contoh, sebuah representasi `TList` dari `Darth` pada bagian sebelumnya
adalah

{lang="text"}
~~~~~~~~
  import iotaz._, TList._
  
  type DarthT  = Vader  :: JarJar :: TNil
  type VaderT  = String :: Int    :: TNil
  type JarJarT = Int    :: String :: TNil
~~~~~~~~

yang dapat diinstansiasi:

{lang="text"}
~~~~~~~~
  val vader: Prod[VaderT]    = Prod("hello", 1)
  val jarjar: Prod[JarJarT]  = Prod(1, "hello")
  
  val VaderI = Cop.Inject[Vader, Cop[DarthT]]
  val darth: Cop[DarthT] = VaderI.inj(Vader("hello", 1))
~~~~~~~~

Agar dapat menggunakan APA `scalaz-deriving`, kita membutuhkan `Isomorphism`
antara TDA kita dengan representasi generik `iotaz`. Akan sangat banyak plat cetak
yang terjadi:

{lang="text"}
~~~~~~~~
  object Darth {
    private type Repr   = Vader :: JarJar :: TNil
    private val VaderI  = Cop.Inject[Vader, Cop[Repr]]
    private val JarJarI = Cop.Inject[JarJar, Cop[Repr]]
    private val iso     = IsoSet(
      {
        case d: Vader  => VaderI.inj(d)
        case d: JarJar => JarJarI.inj(d)
      }, {
        case VaderI(d)  => d
        case JarJarI(d) => d
      }
    )
    ...
  }
  
  object Vader {
    private type Repr = String :: Int :: TNil
    private val iso   = IsoSet(
      d => Prod(d.s, d.i),
      p => Vader(p.head, p.tail.head)
    )
    ...
  }
  
  object JarJar {
    private type Repr = Int :: String :: TNil
    private val iso   = IsoSet(
      d => Prod(d.i, d.s),
      p => JarJar(p.head, p.tail.head)
    )
    ...
  }
~~~~~~~~

Setelah menulis plat cetak diatas, kita dapat memanggil APA `Deriving` untuk `Equal`.
Hal ini mungkin terjadi karena `scalaz-deriving` menyediakan instans teroptimasi
untuk `Deriving[Equal]`

{lang="text"}
~~~~~~~~
  object Darth {
    ...
    implicit val equal: Equal[Darth] = Deriving[Equal].xcoproductz(
      Prod(Need(Equal[Vader]), Need(Equal[JarJar])))(iso.to, iso.from)
  }
  object Vader {
    ...
    implicit val equal: Equal[Vader] = Deriving[Equal].xproductz(
      Prod(Need(Equal[String]), Need(Equal[Int])))(iso.to, iso.from)
  }
  object JarJar {
    ...
    implicit val equal: Equal[JarJar] = Deriving[Equal].xproductz(
      Prod(Need(Equal[Int]), Need(Equal[String])))(iso.to, iso.from)
  }
~~~~~~~~

A> Kelas tipe pada APA `Deriving` terlapisi oleh `Need` (harap ingat `Name` pada
A> bab 6) yang memperkenankan konstruksi luntung sehingga menghindari tugas
A> yang tak perlu bila kelas tipe tidak dibutuhkan. Selain itu, *stack overflow*
A> dapat dihindari untuk GADT rekursi.

Agar kelas tipe `Default` dapat diperlakukan sama, kita harus menyediakan sebuah
instans `Deriving[Default]`. Untuk hal ini, kita tinggal melapisi `Alt` dengan
objek pembantu:

{lang="text"}
~~~~~~~~
  object Default {
    ...
    implicit val deriving: Deriving[Default] = ExtendedInvariantAlt(monad)
  }
~~~~~~~~

{lang="text"}
~~~~~~~~
  object Darth {
    ...
    implicit val default: Default[Darth] = Deriving[Default].xcoproductz(
      Prod(Need(Default[Vader]), Need(Default[JarJar])))(iso.to, iso.from)
  }
  object Vader {
    ...
    implicit val default: Default[Vader] = Deriving[Default].xproductz(
      Prod(Need(Default[String]), Need(Default[Int])))(iso.to, iso.from)
  }
  object JarJar {
    ...
    implicit val default: Default[JarJar] = Deriving[Default].xproductz(
      Prod(Need(Default[Int]), Need(Default[String])))(iso.to, iso.from)
  }
~~~~~~~~
Kita telah menyelesaikan masalah *arity* arbiter, namun kita juga menambah plat
cetak jauh lebih banyak.

Dan yang paling menjengkelkan, anotasi `@deriving` yang disediakan oleh `deriving-plugin`,
membuat semua plat cetak ini secara manual dan hanya perlu diterapkan pada bagian
atas sebuah TDA:

{lang="text"}
~~~~~~~~
  @deriving(Equal, Default)
  sealed abstract class Darth { def widen: Darth = this }
  final case class Vader(s: String, i: Int)  extends Darth
  final case class JarJar(i: Int, s: String) extends Darth
~~~~~~~~

Yang juga diikut-sertakan pada `scalaz-deriving` adalah instans dari `Order`,
`Semigroup`, dan `Monoid`. Instans dari `Show` dan `Arbitrary` tersedia dengan
memasang `scalaz-deriving-magnolia` dan `scalaz-deriving-scalacheck`.


### Contoh

Kita akan menutup pembelajaran kita mengenai `scalaz-deriving` dengan implementasi
dari contoh kelas tipe yang bekerja seutuhnya. Sebelum kita melakukannya, kita
harus tahu tentang tipe data baru: `/~\` yang juga dikenal dengan *uler kasur*,
yang berisi dua jenis struktur lebih tinggi yang berbagi tipe parameter yang sama:

{lang="text"}
~~~~~~~~
  sealed abstract class /~\[A[_], B[_]] {
    type T
    def a: A[T]
    def b: B[T]
  }
  object /~\ {
    type APair[A[_], B[_]]  = A /~\ B
    def unapply[A[_], B[_]](p: A /~\ B): Some[(A[p.T], B[p.T])] = ...
    def apply[A[_], B[_], Z](az: =>A[Z], bz: =>B[Z]): A /~\ B = ...
  }
~~~~~~~~

Biasanya, kita menggunakan uler-kasur pada konteks `Id /~\ TC` dimana `TC` merupakan
kelas tipe, yang berarti kita mempunyai sebuah nilai dan sebuah instans dari
sebuah kelas tipe untuk nilai tersebut tanpa harus tahu apapun mengenai nilai
tadi.

Sebagai tambahan, semua metoda pada APA `Deriving` mempunyai bukti tersirat
dengan bentuk `A PairedWith FA`, memperkenankan pustaka `iotaz` agar dapat melaksanakan
metoda `.zip`, `.traverse`, dan operasi lainnya pada `Prod` dan `Cop`. Kita dapat
mengabaikan parameter ini karena kita tidak menggunakannya secara langsung.


#### `Equal`

Sebagaimana dengan `Default`, kita dapat mendefinisikan `Decidable` biasa yang
memiliki *arity* tetap dan melapisinya dengan `ExtendedInvariantAlt` (pendekatan
paling sederhana), namun kita memilih untuk mengimplementasikan `Decidablez`
secara langsung dengan alasa performa yang lebih baik. Kita juga menambah dua
optimasi tambahan:

1.  melakukan persamaan instans `.eq` sebelum menerapkan `Equal.equal`, memperkenankan
    persamaan antar nilai-nilai identik.
2.  `Foldable.all` memperkenankan untuk kelar awal saat hasil salah satu perbandingan
    bernilai `false`. Misalkan, bila bidang pertama tidak cocok satu sama lain,
    maka kita perlu memeriksa persamaan pada bidang-bidang lainnya.

{lang="text"}
~~~~~~~~
  new Decidablez[Equal] {
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])
  
    def dividez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(g: Z => Prod[A])(
      implicit ev: A PairedWith FA
    ): Equal[Z] = (z1, z2) => (g(z1), g(z2)).zip(tcs).all {
      case (a1, a2) /~\ fa => quick(a1, a2) || fa.value.equal(a1, a2)
    }
  
    def choosez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(g: Z => Cop[A])(
      implicit ev: A PairedWith FA
    ): Equal[Z] = (z1, z2) => (g(z1), g(z2)).zip(tcs) match {
      case -\/(_)               => false
      case \/-((a1, a2) /~\ fa) => quick(a1, a2) || fa.value.equal(a1, a2)
    }
  }
~~~~~~~~


#### `Default`

Sayangnya, APA `iotaz` untuk `.traverse` (dan analognya, `.coptraverse`) meminta
kita untuk mendefinisikan transformasi natural, yang mempunyai sintaks kikuk,
bahkan dengan tambahan kompilator `kind-projector`.

{lang="text"}
~~~~~~~~
  private type K[a] = Kleisli[String \/ ?, Unit, a]
  new IsomorphismMonadError[Default, K, String] with Altz[Default] {
    type Sig[a] = Unit => String \/ a
    override val G = MonadError[K, String]
    override val iso = Kleisli.iso(
      λ[Sig ~> Default](s => instance(s(()))),
      λ[Default ~> Sig](d => _ => d.default)
    )
  
    val extract = λ[NameF ~> (String \/ ?)](a => a.value.default)
    def applyz[Z, A <: TList, FA <: TList](tcs: Prod[FA])(f: Prod[A] => Z)(
      implicit ev: A PairedWith FA
    ): Default[Z] = instance(tcs.traverse(extract).map(f))
  
    val always = λ[NameF ~> Maybe](a => a.value.default.toMaybe)
    def altlyz[Z, A <: TList, FA <: TList](tcs: Prod[FA])(f: Cop[A] => Z)(
      implicit ev: A PairedWith FA
    ): Default[Z] = instance {
      tcs.coptraverse[A, NameF, Id](always).map(f).headMaybe \/> "not found"
    }
  }
~~~~~~~~


#### `Semigroup`

Pendefinisian `Semigroup` untuk koproduk umum tidak mungkin didefinisikan, namun
masih memungkinkan bila mendefinisikannya untuk produk umum. Kita dapat menggunakan
*arity* arbiter `InvariantApplicative`:

{lang="text"}
~~~~~~~~
  new InvariantApplicativez[Semigroup] {
    type L[a] = ((a, a), NameF[a])
    val appender = λ[L ~> Id] { case ((a1, a2), fa) => fa.value.append(a1, a2) }
  
    def xproductz[Z, A <: TList, FA <: TList](tcs: Prod[FA])
                                             (f: Prod[A] => Z, g: Z => Prod[A])
                                             (implicit ev: A PairedWith FA) =
      new Semigroup[Z] {
        def append(z1: Z, z2: =>Z): Z = f(tcs.ziptraverse2(g(z1), g(z2), appender))
      }
  }
~~~~~~~~


#### `JsEncoder` and `JsDecoder`

`scalaz-deriving` tidak menyediakan akses ke nama bidang. Jadi tidak memungkinkan
untuk menulis penyandi dan pembaca sandi JSON.

A> Versi awal dari `scalaz-deriving` mendukung pembacaan nama bidang, namun tetap
A> saja tidak ada keuntungan yang didapat bila dibandingkan bila menggunakan Magnolia.
A> Jadi, dukungan dihapus agar tetap fokus pada kelas tipe `Alt` dan `Decidable`
A> yang taat hukum.


## Magnolia

Pustaka makro Magnolia menyediakan APA yang rapi untuk menulis derivasi kelas
tipe. Pemasangan Magnolia dapat dilakukan dengan menambah potongan berikut
pada `build.sbt`

{lang="text"}
~~~~~~~~
  libraryDependencies += "com.propensive" %% "magnolia" % "0.10.1"
~~~~~~~~

Seorang penulis kelas tipe mengimplementasikan anggota-anggota berikut:

{lang="text"}
~~~~~~~~
  import magnolia._
  
  object MyDerivation {
    type Typeclass[A]
  
    def combine[A](ctx: CaseClass[Typeclass, A]): Typeclass[A]
    def dispatch[A](ctx: SealedTrait[Typeclass, A]): Typeclass[A]
  
    def gen[A]: Typeclass[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Sedangkan APA Magnolia:

{lang="text"}
~~~~~~~~
  class CaseClass[TC[_], A] {
    def typeName: TypeName
    def construct[B](f: Param[TC, A] => B): A
    def constructMonadic[F[_]: Monadic, B](f: Param[TC, A] => F[B]): F[A]
    def parameters: Seq[Param[TC, A]]
    def annotations: Seq[Any]
  }
  
  class SealedTrait[TC[_], A] {
    def typeName: TypeName
    def subtypes: Seq[Subtype[TC, A]]
    def dispatch[B](value: A)(handle: Subtype[TC, A] => B): B
    def annotations: Seq[Any]
  }
~~~~~~~~

dengan pembantu

{lang="text"}
~~~~~~~~
  final case class TypeName(short: String, full: String)
  
  class Param[TC[_], A] {
    type PType
    def label: String
    def index: Int
    def typeclass: TC[PType]
    def dereference(param: A): PType
    def default: Option[PType]
    def annotations: Seq[Any]
  }
  
  class Subtype[TC[_], A] {
    type SType <: A
    def typeName: TypeName
    def index: Int
    def typeclass: TC[SType]
    def cast(a: A): SType
    def annotations: Seq[Any]
  }
~~~~~~~~

Kelas tipe `Monadic`, yang digunakan pada `constructMonadic`, dibuat secara
otomatis bila tipe data kita mempunyai metoda `.map` dan `.flatMap` saat kita
mengimpor `mercator._`.

Sebenarnya, tidak masuk akal bila kita menggunakan Magnolia untuk kelas tipe
yang dapat diabstraksi dengan `Divisible`, `Decidable`, `Applicative`, atau `Alt`
karena abstraksi tersebut menyediakan struktur dan tes tambahan secara otomatis.
Namun, Magnolia menawarkan fitur yang tidak dapat diberikan oleh `scalaz-deriving`:
akses ke nama bidang, nama tipe, anotasi, dan nilai default.


### Contoh: JSON

Kita mempunyai beberapa pilihan desain mengenai serialisasi JSON yang harus dipilih:

1.  Haruskah kita mengikut-sertakan bidang dengan nilai `null`?
2.  Haruskah pembacaan sandi memperlakukan nilai yang hilang dan `null` secara berbeda?
3.  Bagaimana kita menyandikan nama dari sebuah koproduk?
4.  Bagaimana kita memperlakukan koproduk yang bukan berupa `JsObject`?

Kita akan memilih beberapa pengaturan default

-   tidak mengikut sertakan bidang bila nilai bidang tersebut berupa `JsNull`.
-   menangani bidang yang hilang sama dengan nilai `null`.
-   menggunakan bidang khusus `"type"` untuk membedakan koproduk yang menggunakan nama tipe.
-   menempatkan nilai primitif pada bidang khusus `"xvalue"`.

dan memperkenankan pengguna untuk menambahkan anotasi ke bidang koproduk dan produk
agar dapat mengubah format sesuai keinginan mereka:

{lang="text"}
~~~~~~~~
  sealed class json extends Annotation
  object json {
    final case class nulls()          extends json
    final case class field(f: String) extends json
    final case class hint(f: String)  extends json
  }
~~~~~~~~

A> Magnolia tidak terbatas pada satu keluarga anotasi saja. Penyandian ini
A> dimaksudkan agar kita dapat melakukan perbandingan semacam Shapeless pada
A> bab selanjutnya.

Sebagai contoh

{lang="text"}
~~~~~~~~
  @json.field("TYPE")
  sealed abstract class Cost
  final case class Time(s: String) extends Cost
  final case class Money(@json.field("integer") i: Int) extends Cost
~~~~~~~~

Dimulai dengan `JsDecoder` yang hanya menangani pengaturan default kita:

{lang="text"}
~~~~~~~~
  object JsMagnoliaEncoder {
    type Typeclass[A] = JsEncoder[A]
  
    def combine[A](ctx: CaseClass[JsEncoder, A]): JsEncoder[A] = { a =>
      val empty = IList.empty[(String, JsValue)]
      val fields = ctx.parameters.foldRight(right) { (p, acc) =>
        p.typeclass.toJson(p.dereference(a)) match {
          case JsNull => acc
          case value  => (p.label -> value) :: acc
        }
      }
      JsObject(fields)
    }
  
    def dispatch[A](ctx: SealedTrait[JsEncoder, A]): JsEncoder[A] = a =>
      ctx.dispatch(a) { sub =>
        val hint = "type" -> JsString(sub.typeName.short)
        sub.typeclass.toJson(sub.cast(a)) match {
          case JsObject(fields) => JsObject(hint :: fields)
          case other            => JsObject(IList(hint, "xvalue" -> other))
        }
      }
  
    def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Kita dapat melihat bagaimana APA Magnolia mempermudah pengaksesan nama bidang
dan kelas tipe untuk tiap parameter.

Sekarang, kita akan menambah anotasi untuk menangani prarasa pengguna. Untuk
menghindari mengingat-ingat anotasi pada tiap penyandian, kita akan menyimpannya
pada tembolok dalam bentuk larik. Walaupun akses bidang pada sebuah larik tidak
total, sebagai gantinya, kita mendapat jaminan bahwa indeks akan selalu selaras.
Yang menjadi korban pada tarik-ulur antara spesialisasi dan generalisasi semacam
ini adalah performa.

{lang="text"}
~~~~~~~~
  object JsMagnoliaEncoder {
    type Typeclass[A] = JsEncoder[A]
  
    def combine[A](ctx: CaseClass[JsEncoder, A]): JsEncoder[A] =
      new JsEncoder[A] {
        private val anns = ctx.parameters.map { p =>
          val nulls = p.annotations.collectFirst {
            case json.nulls() => true
          }.getOrElse(false)
          val field = p.annotations.collectFirst {
            case json.field(name) => name
          }.getOrElse(p.label)
          (nulls, field)
        }.toArray
  
        def toJson(a: A): JsValue = {
          val empty = IList.empty[(String, JsValue)]
          val fields = ctx.parameters.foldRight(empty) { (p, acc) =>
            val (nulls, field) = anns(p.index)
            p.typeclass.toJson(p.dereference(a)) match {
              case JsNull if !nulls => acc
              case value            => (field -> value) :: acc
            }
          }
          JsObject(fields)
        }
      }
  
    def dispatch[A](ctx: SealedTrait[JsEncoder, A]): JsEncoder[A] =
      new JsEncoder[A] {
        private val field = ctx.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse("type")
        private val anns = ctx.subtypes.map { s =>
          val hint = s.annotations.collectFirst {
            case json.hint(name) => field -> JsString(name)
          }.getOrElse(field -> JsString(s.typeName.short))
          val xvalue = s.annotations.collectFirst {
            case json.field(name) => name
          }.getOrElse("xvalue")
          (hint, xvalue)
        }.toArray
  
        def toJson(a: A): JsValue = ctx.dispatch(a) { sub =>
          val (hint, xvalue) = anns(sub.index)
          sub.typeclass.toJson(sub.cast(a)) match {
            case JsObject(fields) => JsObject(hint :: fields)
            case other            => JsObject(hint :: (xvalue -> other) :: IList.empty)
          }
        }
      }
  
    def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Untuk pembaca sandi, kita menggunakan `.constructMonadic` yang mempunyai penanda
tipe mirip dengan `.traverse`

{lang="text"}
~~~~~~~~
  object JsMagnoliaDecoder {
    type Typeclass[A] = JsDecoder[A]
  
    def combine[A](ctx: CaseClass[JsDecoder, A]): JsDecoder[A] = {
      case obj @ JsObject(_) =>
        ctx.constructMonadic(
          p => p.typeclass.fromJson(obj.get(p.label).getOrElse(JsNull))
        )
      case other => fail("JsObject", other)
    }
  
    def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] = {
      case obj @ JsObject(_) =>
        obj.get("type") match {
          case \/-(JsString(hint)) =>
            ctx.subtypes.find(_.typeName.short == hint) match {
              case None => fail(s"a valid '$hint'", obj)
              case Some(sub) =>
                val value = obj.get("xvalue").getOrElse(obj)
                sub.typeclass.fromJson(value)
            }
          case _ => fail("JsObject with type", obj)
        }
      case other => fail("JsObject", other)
    }
  
    def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Hal yang sama, penambahan dukungan untuk prarasa pengguna dan nilai bidang default,
dan juga bebarapa optimasi:

{lang="text"}
~~~~~~~~
  object JsMagnoliaDecoder {
    type Typeclass[A] = JsDecoder[A]
  
    def combine[A](ctx: CaseClass[JsDecoder, A]): JsDecoder[A] =
      new JsDecoder[A] {
        private val nulls = ctx.parameters.map { p =>
          p.annotations.collectFirst {
            case json.nulls() => true
          }.getOrElse(false)
        }.toArray
  
        private val fieldnames = ctx.parameters.map { p =>
          p.annotations.collectFirst {
            case json.field(name) => name
          }.getOrElse(p.label)
        }.toArray
  
        def fromJson(j: JsValue): String \/ A = j match {
          case obj @ JsObject(_) =>
            import mercator._
            val lookup = obj.fields.toMap
            ctx.constructMonadic { p =>
              val field = fieldnames(p.index)
              lookup
                .get(field)
                .into {
                  case Maybe.Just(value) => p.typeclass.fromJson(value)
                  case _ =>
                    p.default match {
                      case Some(default) => \/-(default)
                      case None if nulls(p.index) =>
                        s"missing field '$field'".left
                      case None => p.typeclass.fromJson(JsNull)
                    }
                }
            }
          case other => fail("JsObject", other)
        }
      }
  
    def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] =
      new JsDecoder[A] {
        private val subtype = ctx.subtypes.map { s =>
          s.annotations.collectFirst {
            case json.hint(name) => name
          }.getOrElse(s.typeName.short) -> s
        }.toMap
        private val typehint = ctx.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse("type")
        private val xvalues = ctx.subtypes.map { sub =>
          sub.annotations.collectFirst {
            case json.field(name) => name
          }.getOrElse("xvalue")
        }.toArray
  
        def fromJson(j: JsValue): String \/ A = j match {
          case obj @ JsObject(_) =>
            obj.get(typehint) match {
              case \/-(JsString(h)) =>
                subtype.get(h) match {
                  case None => fail(s"a valid '$h'", obj)
                  case Some(sub) =>
                    val xvalue = xvalues(sub.index)
                    val value  = obj.get(xvalue).getOrElse(obj)
                    sub.typeclass.fromJson(value)
                }
              case _ => fail(s"JsObject with '$typehint' field", obj)
            }
          case other => fail("JsObject", other)
        }
      }
  
    def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Kita memanggil metoda `JsMagnoliaEncoder.gen` atau `JsMagnoliaDecoder.gen` dari
objek pendamping tipe data kita. Sebagai contoh, APA Google Maps

{lang="text"}
~~~~~~~~
  final case class Value(text: String, value: Int)
  final case class Elements(distance: Value, duration: Value, status: String)
  final case class Rows(elements: List[Elements])
  final case class DistanceMatrix(
    destination_addresses: List[String],
    origin_addresses: List[String],
    rows: List[Rows],
    status: String
  )
  
  object Value {
    implicit val encoder: JsEncoder[Value] = JsMagnoliaEncoder.gen
    implicit val decoder: JsDecoder[Value] = JsMagnoliaDecoder.gen
  }
  object Elements {
    implicit val encoder: JsEncoder[Elements] = JsMagnoliaEncoder.gen
    implicit val decoder: JsDecoder[Elements] = JsMagnoliaDecoder.gen
  }
  object Rows {
    implicit val encoder: JsEncoder[Rows] = JsMagnoliaEncoder.gen
    implicit val decoder: JsDecoder[Rows] = JsMagnoliaDecoder.gen
  }
  object DistanceMatrix {
    implicit val encoder: JsEncoder[DistanceMatrix] = JsMagnoliaEncoder.gen
    implicit val decoder: JsDecoder[DistanceMatrix] = JsMagnoliaDecoder.gen
  }
~~~~~~~~

Untungnya, anotasi `@deriving` mendukung Magnolia. Bila penulis kelas tipe menyediakan
berkas `deriving.conf` bersamaan dengan berkas jar mereka yang berisi teks berikut

{lang="text"}
~~~~~~~~
  jsonformat.JsEncoder=jsonformat.JsMagnoliaEncoder.gen
  jsonformat.JsDecoder=jsonformat.JsMagnoliaDecoder.gen
~~~~~~~~

`deriving-macro` akan memanggil metoda yang disediakan oleh pengguna:

{lang="text"}
~~~~~~~~
  @deriving(JsEncoder, JsDecoder)
  final case class Value(text: String, value: Int)
  @deriving(JsEncoder, JsDecoder)
  final case class Elements(distance: Value, duration: Value, status: String)
  @deriving(JsEncoder, JsDecoder)
  final case class Rows(elements: List[Elements])
  @deriving(JsEncoder, JsDecoder)
  final case class DistanceMatrix(
    destination_addresses: List[String],
    origin_addresses: List[String],
    rows: List[Rows],
    status: String
  )
~~~~~~~~


### Derivasi Otomatis

Penghasilan instans `implicit` pada objek pendamping tipe data, secara historis,
dikenal sebagai derivasi *semi-otomatis*. Berbeda dengan derivasi otomatis dimana
`.gen` dibuat implisit

{lang="text"}
~~~~~~~~
  object JsMagnoliaEncoder {
    ...
    implicit def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
  }
  object JsMagnoliaDecoder {
    ...
    implicit def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
  }
~~~~~~~~

Penggguna dapat mengimpor metoda ini ke cakupan kode mereka dan mendapatkan
derivasi otomatis pada saat penggunaan

{lang="text"}
~~~~~~~~
  scala> final case class Value(text: String, value: Int)
  scala> import JsMagnoliaEncoder.gen
  scala> Value("hello", 1).toJson
  res = JsObject([("text","hello"),("value",1)])
~~~~~~~~

Mungkin terlihat menggiurkan, karena pengguna tidak perlu repot menulis kode,
namun ada dua kerugian penting:

1.  makro diselawat pada setiap penggunaan, misal tiap kali kita memanggil `.toJson`.
    Hal semacam ini memperlambat kompilasi dan juga menghasilkan objek lebih banyak
    pada saat waktu-jalan, yang secara tidak langsung berdampak pada performa waktu-jalan.
2.  kemungkinan derivasi tak terduga.

Kerugian pertama cukup jelas, namun derivasi yang tak terduga akan terejawantah
sebagai kutu yang hampir tidak kasat mata. Anggap contoh berikut

{lang="text"}
~~~~~~~~
  @deriving(JsEncoder)
  final case class Foo(s: Option[String])
~~~~~~~~

bila kita lupa menyediakan derivasi implisit untuk `Option`. Mungkin kita berharap
`Foo(Some("hello"))` akan menjadi

{lang="text"}
~~~~~~~~
  {
    "s":"hello"
  }
~~~~~~~~

Namun yang muncul adalah

{lang="text"}
~~~~~~~~
  {
    "s": {
      "type":"Some",
      "get":"hello"
    }
  }
~~~~~~~~

karena Magnolia menderivasikan penyandi `Option` untuk kita.

Hal semacam ini sangat membingungkan. Kita lebih memilih agar kompilator memberi-tahu
kita bila kita lupa sesuatu. Maka dari itu, penderivasian otomatis tidak
direkomendasikan.


## Shapeless

Pustaka [Shapeles](https://github.com/milessabin/shapeless) dikenal sebagai pustaka
paling rumit pada ekosistem Scala. Alasannya, pustaka ini menggunakan fitur bahasa
`implicit` dengan sangat mendalam dengan membuat semacam *bahasa pemrograman generik*
pada tingkat tipe.

Hal semacam ini tidak sepenuhnya asing: pada Scalaz, kita membatasi penggunaan
fitur bahasa `implicit` hanya pada kelas tipe. Namun, kadang kita meminta kompilator
menyediakan kita *bukti* yang behubungan dengan tipe. Sebagai contoh, hubungan
Liskov atau Leibniz (`<~<` dan `===`) dan saat melakukan `Inject` ke sebuah
aljabar `scalaz.Coproduct` dengan sebuah aljabar *free*.

A> Bukan hal yang mutlak untuk memahami Shapeless bila ingin menjadi pemrogram
A> fungsional. Bila bab ini terlalu berat, silakan untuk melewati sampai ke bagian
A> berikutnya.

Untuk memasang Shapeless, tambahkan potongan kode berikut ke `build.sbt`

{lang="text"}
~~~~~~~~
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
~~~~~~~~

Inti dari Shapeless` adalah tipedata `HList` dan `Coproduct`

{lang="text"}
~~~~~~~~
  package shapeless
  
  sealed trait HList
  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
  sealed trait NNil extends HList
  case object HNil extends HNil {
    def ::[H](h: H): H :: HNil = ::(h, this)
  }
  
  sealed trait Coproduct
  sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
  final case class Inl[+H, +T <: Coproduct](head: H) extends :+:[H, T]
  final case class Inr[+H, +T <: Coproduct](tail: T) extends :+:[H, T]
  sealed trait CNil extends Coproduct // no implementations
~~~~~~~~

yang merupakan representasi *generik* dari produk dan koproduk, sedangkan
`sealed trait HNil` digunakan sebagai pembantu agar kita tidak perlu menulis
`HNil.type`

Shapeless juga mempunyai salinan tipe data `IsoSet` yang disebut sebagai `Generic`
yang memperkenankan kita untuk berpindah antara sebuah TDA dan representasi
generiknya:

{lang="text"}
~~~~~~~~
  trait Generic[T] {
    type Repr
    def to(t: T): Repr
    def from(r: Repr): T
  }
  object Generic {
    type Aux[T, R] = Generic[T] { type Repr = R }
    def apply[T](implicit G: Generic[T]): Aux[T, G.Repr] = G
    implicit def materialize[T, R]: Aux[T, R] = macro ...
  }
~~~~~~~~

Banyak dari tipe Shapeless mempunyai tipe anggot (`Repr`) dan alias tipe `.Aux`
(bantuan, *auxiliary*) pada objek pendamping yang membuat tipe kedua muncul
terlihat. Hal ini memperkenankan kita untuk meminta `Generic[Foo]` untuk tipe
`Foo` tanpa harus menyediakan representasi generiknya karena sudah dibuat oleh
sebuah makro.

{lang="text"}
~~~~~~~~
  scala> import shapeless._
  scala> final case class Foo(a: String, b: Long)
         Generic[Foo].to(Foo("hello", 13L))
  res: String :: Long :: HNil = hello :: 13 :: HNil
  
  scala> Generic[Foo].from("hello" :: 13L :: HNil)
  res: Foo = Foo(hello,13)
  
  scala> sealed abstract class Bar
         case object Irish extends Bar
         case object English extends Bar
  
  scala> Generic[Bar].to(Irish)
  res: English.type :+: Irish.type :+: CNil.type = Inl(Irish)
  
  scala> Generic[Bar].from(Inl(Irish))
  res: Bar = Irish
~~~~~~~~

Ada juga komplementer `LabelledGeneric` yang mengikutsertakan nama bidang

{lang="text"}
~~~~~~~~
  scala> import shapeless._, labelled._
  scala> final case class Foo(a: String, b: Long)
  
  scala> LabelledGeneric[Foo].to(Foo("hello", 13L))
  res: String with KeyTag[Symbol with Tagged[String("a")], String] ::
       Long   with KeyTag[Symbol with Tagged[String("b")],   Long] ::
       HNil =
       hello :: 13 :: HNil
  
  scala> sealed abstract class Bar
         case object Irish extends Bar
         case object English extends Bar
  
  scala> LabelledGeneric[Bar].to(Irish)
  res: Irish.type   with KeyTag[Symbol with Tagged[String("Irish")],     Irish.type] :+:
       English.type with KeyTag[Symbol with Tagged[String("English")], English.type] :+:
       CNil.type =
       Inl(Irish)
~~~~~~~~

Harap diperhatikan bahwa **nilai** dari sebuah representasi `LabelledGeneric`
sama dengan representasi `Generic`. Nama bidang hanya ada pada tipe dan dihapus
pada waktu-jalan.

Kita tidak perlu untuk menulis `KeyTag` secara manual karena kita dapat menggunakan
tipe alias:

{lang="text"}
~~~~~~~~
  type FieldType[K, +V] = V with KeyTag[K, V]
~~~~~~~~

Bila kita ingin mengakses nama bidang dari sebuah `FieldType[K, A]`, kita dapat
meminta bukti implisit `Witness.Aux[K]` yang memperkenankan kita untuk mengakses
nilai dari `K` pada waktu-jalan.

Secara sekilas, ini semua yang harus kita tahu mengenai Shapeless agar dapat
menderivasi sebuah kelas tipe. Namun, karena semua hal semakin rumit, misalkan
jawaban kapan kawin, punya anak, dan pensiun, kita akan melanjutkan pembahasan
dengan contoh yang juga semakin kompleks.


### Contoh: *Equal*

Pola yang umum digunakan adalah mengeksten kelas tipe yang ingin kita derivasi
dan menempatkan kode Shapeless pada objek pendampingnya. Pola ini memberikan
kita cakupan implisit yang dapat dicari oleh kompilator tanpa harus melakukan
impor yang rumit.

{lang="text"}
~~~~~~~~
  trait DerivedEqual[A] extends Equal[A]
  object DerivedEqual {
    ...
  }
~~~~~~~~

Titik mulai dari derivasi Shapeless adalah metoda `gen` yang meminta dua parameter
tipe: `A` sebagai yang kita derivasikan dan `R` sebagai representasi generiknya.
Lalu kita akan meminta `Generic.Aux[A, R]`, menghubungkan `A` ke `R`, dan sebuah
instans dari kelas tipe `Derived` untuk `R`. Kita memulai dengan penanda dan
implementasi sederhana berikut:

{lang="text"}
~~~~~~~~
  import shapeless._
  
  object DerivedEqual {
    def gen[A, R: DerivedEqual](implicit G: Generic.Aux[A, R]): Equal[A] =
      (a1, a2) => Equal[R].equal(G.to(a1), G.to(a2))
  }
~~~~~~~~

Kita telah mereduksi permasalahan atas penyediaan sebuah `Equal[R]` implisit
untuk `R` yang merupakan representasi generik dari `A`. Pertam, perhatikan produk
yang berupa `R <: HList`. Penanda inilah yang kita inginkan untuk diimplementasikan:

{lang="text"}
~~~~~~~~
  implicit def hcons[H: Equal, T <: HList: DerivedEqual]: DerivedEqual[H :: T]
~~~~~~~~

karena bila kita dapat mengimplementasikannya untuk *head* dan *tail*, komplire
akan dapat mengulang metoda ini sampai pada akhir daftar. Hal ini membawa kita
pada keharusan untuk menyediakan sebuah instans untuk `HNil` kosong

{lang="text"}
~~~~~~~~
  implicit def hnil: DerivedEqual[HNil]
~~~~~~~~

Kita akan mengimplementasikan metoda berikut

{lang="text"}
~~~~~~~~
  implicit def hcons[H: Equal, T <: HList: DerivedEqual]: DerivedEqual[H :: T] =
    (h1, h2) => Equal[H].equal(h1.head, h2.head) && Equal[T].equal(h1.tail, h2.tail)
  
  implicit val hnil: DerivedEqual[HNil] = (_, _) => true
~~~~~~~~

dan untuk kooproduk, kita ingin mengimplementasikan penanda berikut

{lang="text"}
~~~~~~~~
  implicit def ccons[H: Equal, T <: Coproduct: DerivedEqual]: DerivedEqual[H :+: T]
  implicit def cnil: DerivedEqual[CNil]
~~~~~~~~

A> Scalaz dan Shapeless berbagi banyak nama tipe. Saat menggunakan secara bersamaan,
A> sering kali kita harus mengecualikan beberapa elemen dari impor. Mis,
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   import scalaz.{ Coproduct => _, :+: => _, _ }, Scalaz._
A>   import shapeless._
A> ~~~~~~~~

`.cnil` tidak akan pernah dipanggil untuk kelas tipe dengan parameter tipe yang
hanya ada pada posisi kontravarian, seperti `Equal`, namun koompiler tidak tahu
mengenai hal tersebut. Jadi, kita akan menyediakan potongan kode berikut:

{lang="text"}
~~~~~~~~
  implicit val cnil: DerivedEqual[CNil] = (_, _) => sys.error("impossible")
~~~~~~~~

Untuk koproduk, kita hanya bisa membandingkan dua hal bila mereka selaras. Atau
keduanya `Inl` atau `Inr`

{lang="text"}
~~~~~~~~
  implicit def ccons[H: Equal, T <: Coproduct: DerivedEqual]: DerivedEqual[H :+: T] = {
    case (Inl(c1), Inl(c2)) => Equal[H].equal(c1, c2)
    case (Inr(c1), Inr(c2)) => Equal[T].equal(c1, c2)
    case _                  => false
  }
~~~~~~~~

Hal yang patut dicatat adalah metoda kita selaras dengan konsep `conquer` (`hnil`),
`divide2` (`hlist`), dan `alt2` (`coproduct`). Namun, kita tidak mendapat
keuntungan apapun seperti pengimplementasian `Decidable`. Hal ini berarti kita
harus memulai dari awal bila kita menulis tes untuk kode ini.

Mari kita tes kode berikut dengan TDA sederhana

{lang="text"}
~~~~~~~~
  sealed abstract class Foo
  final case class Bar(s: String)          extends Foo
  final case class Faz(b: Boolean, i: Int) extends Foo
  final case object Baz                    extends Foo
~~~~~~~~

Kita harus menyediakan instans pada objek pendamping:

{lang="text"}
~~~~~~~~
  object Foo {
    implicit val equal: Equal[Foo] = DerivedEqual.gen
  }
  object Bar {
    implicit val equal: Equal[Bar] = DerivedEqual.gen
  }
  object Faz {
    implicit val equal: Equal[Faz] = DerivedEqual.gen
  }
  final case object Baz extends Foo {
    implicit val equal: Equal[Baz.type] = DerivedEqual.gen
  }
~~~~~~~~

Namun, kode tersebut tidak dapat dikompilasi

{lang="text"}
~~~~~~~~
  [error] shapeless.scala:41:38: ambiguous implicit values:
  [error]  both value hnil in object DerivedEqual of type => DerivedEqual[HNil]
  [error]  and value cnil in object DerivedEqual of type => DerivedEqual[CNil]
  [error]  match expected type DerivedEqual[R]
  [error]     : Equal[Baz.type] = DerivedEqual.gen
  [error]                                      ^
~~~~~~~~

Nah, galat kompilasi Shapeless terlihat seperti ini.

Masalah ini, yang sama sekali tidak jelas terlihat dari pesan galat, terjadi
karena kompilator tidak dapat menentukan `R` dan mengira `R` sebagai tipe lainnya.
Kita harus menyediakan parameter tipe eksplisit saat memanggil `gen`, mis.

{lang="text"}
~~~~~~~~
  implicit val equal: Equal[Baz.type] = DerivedEqual.gen[Baz.type, HNil]
~~~~~~~~

atau kita dapat menggunakan makro `Generic` agar kompilator dapat menebak representasi
generiknya

{lang="text"}
~~~~~~~~
  final case object Baz extends Foo {
    implicit val generic                = Generic[Baz.type]
    implicit val equal: Equal[Baz.type] = DerivedEqual.gen[Baz.type, generic.Repr]
  }
  ...
~~~~~~~~

A> Sampai disini, abaikan semua coretan merah dan hanya percaya pada kompilator.
A> Disinilah dimana Shapeless tidak didukung oleh IDE.

Penanda tipe-lah yang menyelesaikan masalah tersebut

{lang="text"}
~~~~~~~~
  def gen[A, R: DerivedEqual](implicit G: Generic.Aux[A, R]): Equal[A]
~~~~~~~~

yang dijabarkan menjadi

{lang="text"}
~~~~~~~~
  def gen[A, R](implicit R: DerivedEqual[R], G: Generic.Aux[A, R]): Equal[A]
~~~~~~~~

Kompiler Scala menyelesaikan batasan tipe dari kiri ke kanan. Jadi kompilator
akan mencari banyak solusi untuk `DerivedEqual[R]` sebelum membatasinya menjadi
`Generic.Aux[A, R]`. Cara lain untuk menyelesaikan masalah ini adalah dengan
tidak menggunakan batasan konteks.

A> Kita merasa penunjukkan gagalnya kompilasi kode, bukan versi yang benar-benar
A> berjalan dengan baik, adalah karena memang keseharian nyata dari Shapeless
A> begitu. Hal lain yang mungkin bisa saja kita lakukan disini adalah menggunakan
A> `sealed` pada *trait* `DerivedEqual` sehingga hanya versi terderivasi saja
A> yang valid. Namun, `sealed trait` tidak kompatibel dengan tipe SAM.

Berbekal pengetahuan ini, kita tidak perlu lagi `implicit val generic` atau tipe
parameter eksplisit pada panggilan `.gen`. Kita dapat menggunakan `@deriving`
dan menambahkan sebuah catatan pada `deriving.conf` (dengan asumsi kita ingin
menimpa implementasi `scalaz-deriving`)

{lang="text"}
~~~~~~~~
  scalaz.Equal=fommil.DerivedEqual.gen
~~~~~~~~

dan menulis

{lang="text"}
~~~~~~~~
  @deriving(Equal) sealed abstract class Foo
  @deriving(Equal) final case class Bar(s: String)          extends Foo
  @deriving(Equal) final case class Faz(b: Boolean, i: Int) extends Foo
  @deriving(Equal) final case object Baz
~~~~~~~~

Namun, mengganti versi `scalaz-deriving` juga berarti waktu kompilasi akan semakin
panjang. Hal ini disebabkan karena kompilator menyelesaikan pencarian implisit `N`
untuk tiap produk bidang `N` atau koproduk dari produk `N`. Hal semacam ini
tidak terjadi pada `scalaz-deriving` dan Magnolia.

Harap dicatat saat menggunakan `scalaz-deriving` atau Magnolia, kita dapat
menuliskan `@deriving` pada bagian atas anggota dari sebuah TDA. Shapeless
meminta perlakuan yang berbeda dengan mengharuskan kita untuk menambahkannya pada
semua bagian.

Namun, implementasi ini masih memiliki kutu: kegagalan pada tipe rekursif **saat waktu jalan**, mis.

{lang="text"}
~~~~~~~~
  @deriving(Equal) sealed trait ATree
  @deriving(Equal) final case class Leaf(value: String)               extends ATree
  @deriving(Equal) final case class Branch(left: ATree, right: ATree) extends ATree
~~~~~~~~

{lang="text"}
~~~~~~~~
  scala> val leaf1: Leaf    = Leaf("hello")
         val leaf2: Leaf    = Leaf("goodbye")
         val branch: Branch = Branch(leaf1, leaf2)
         val tree1: ATree   = Branch(leaf1, branch)
         val tree2: ATree   = Branch(leaf2, branch)
  
  scala> assert(tree1 /== tree2)
  [error] java.lang.NullPointerException
  [error] at DerivedEqual$.shapes$DerivedEqual$$$anonfun$hcons$1(shapeless.scala:16)
          ...
~~~~~~~~

Alasan hal ini terjadi adalah `Equal[Tree]` bergantung pada `Equal[Branch]` yang
bergantung pada `Equal[Tree]`. Dan terjadilah rekursi. Maka dari itu, implementasi
ini harus dipanggil secara lantung.

`scalaz-deriving` dan Magnolia secara otomatis melakukan evaluasi lantung dan
lagi-lagi Shapeless mengambil penekatan yang berbeda karena menyerahkan sepenuhnya
ke penulis kelas tipe.

Tipe makro `Cached`, `Strict`, dan `Lazy` mengubah perilaku inferensi kompilator
dan memperkenankan kita untuk mendapatkan kelantungan yang kita butuhkan. Pola
yang harus diikut adalah dengan menggunakan `Cached[Strict[_]]` pada titik masuk
dan `Lazy[_]` pada instans `H`.

Akah jauh lebih baik untuk tidak lagi menggunakan batasan konteks dan tipe
SAM pada titik ini:

{lang="text"}
~~~~~~~~
  sealed trait DerivedEqual[A] extends Equal[A]
  object DerivedEqual {
    def gen[A, R](
      implicit G: Generic.Aux[A, R],
      R: Cached[Strict[DerivedEqual[R]]]
    ): Equal[A] = new Equal[A] {
      def equal(a1: A, a2: A) =
        quick(a1, a2) || R.value.value.equal(G.to(a1), G.to(a2))
    }
  
    implicit def hcons[H, T <: HList](
      implicit H: Lazy[Equal[H]],
      T: DerivedEqual[T]
    ): DerivedEqual[H :: T] = new DerivedEqual[H :: T] {
      def equal(ht1: H :: T, ht2: H :: T) =
        (quick(ht1.head, ht2.head) || H.value.equal(ht1.head, ht2.head)) &&
          T.equal(ht1.tail, ht2.tail)
    }
  
    implicit val hnil: DerivedEqual[HNil] = new DerivedEqual[HNil] {
      def equal(@unused h1: HNil, @unused h2: HNil) = true
    }
  
    implicit def ccons[H, T <: Coproduct](
      implicit H: Lazy[Equal[H]],
      T: DerivedEqual[T]
    ): DerivedEqual[H :+: T] = new DerivedEqual[H :+: T] {
      def equal(ht1: H :+: T, ht2: H :+: T) = (ht1, ht2) match {
        case (Inl(c1), Inl(c2)) => quick(c1, c2) || H.value.equal(c1, c2)
        case (Inr(c1), Inr(c2)) => T.equal(c1, c2)
        case _                  => false
      }
    }
  
    implicit val cnil: DerivedEqual[CNil] = new DerivedEqual[CNil] {
      def equal(@unused c1: CNil, @unused c2: CNil) = sys.error("impossible")
    }
  
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])
  }
~~~~~~~~

Sembari melepas batasan konteks, kita juga mengoptimasi dengan menggunakan
jalan pintas `quick` dari `scalaz-deriving`.

Sekarang, kita dapat memanggil

{lang="text"}
~~~~~~~~
  assert(tree1 /== tree2)
~~~~~~~~

tanpa mendapatkan pengecualian waktu-jalan.


### Contoh: `Default`

Tidak ada jebakan baru pada implementasi dari kelas tipe dengan parameter tipe
di posisi kovarian. Disini, kita membuat nilai `HList` dan `Coproduct` dan harus
menyediakan nilai untuk `CNil` karena nilai ini berhubungan dengan permasalah
dimana tidak ada koproduk yang mampu menyediakan nilai tersebut.

{lang="text"}
~~~~~~~~
  sealed trait DerivedDefault[A] extends Default[A]
  object DerivedDefault {
    def gen[A, R](
      implicit G: Generic.Aux[A, R],
      R: Cached[Strict[DerivedDefault[R]]]
    ): Default[A] = new Default[A] {
      def default = R.value.value.default.map(G.from)
    }
  
    implicit def hcons[H, T <: HList](
      implicit H: Lazy[Default[H]],
      T: DerivedDefault[T]
    ): DerivedDefault[H :: T] = new DerivedDefault[H :: T] {
      def default =
        for {
          head <- H.value.default
          tail <- T.default
        } yield head :: tail
    }
  
    implicit val hnil: DerivedDefault[HNil] = new DerivedDefault[HNil] {
      def default = HNil.right
    }
  
    implicit def ccons[H, T <: Coproduct](
      implicit H: Lazy[Default[H]],
      T: DerivedDefault[T]
    ): DerivedDefault[H :+: T] = new DerivedDefault[H :+: T] {
      def default = H.value.default.map(Inl(_)).orElse(T.default.map(Inr(_)))
    }
  
    implicit val cnil: DerivedDefault[CNil] = new DerivedDefault[CNil] {
      def default = "not a valid coproduct".left
    }
  }
~~~~~~~~

Seperti analogi yang dapat kita tarik antara `Equal` dan `Decidable`, kita dapat
melihat hubungan antara `Alt` pada `.point` (`hnil`), `.apply2` (`.hcons`),
dan `.altly2` (`.ccons`).

Tidak banyak yang bisa dipelajari dari contoh seperti `Semigroup`, jadi kita
akan melewati penyandian dan pembacaan sandi.


### Contoh: `JsEncoder`

Agar dapat mereproduksi penyandi JSON Magnolia kita, kita harus mampu mengakses

1.  nama bidang dan nama kelas
2.  anotasi untuk prarasa pengguna
3.  nilai default pada sebuah `case class`

Kita akan memulai dengan membuat sebuah penyandi yang hanya menangani pengaturan
yang masuk akal.

Untuk mendapatkan nama bidang, kita menggunakan `LabelledGeneric`, bukan `Generic`,
dan pada saat mendefiniskan tipe dari elemen awal, kita menggunakan `FieldType[K, H]`,
bukan hanya `H`. Sebuah `Witness.Aux[K]` menyediakan nilai dari nama bidang pada
saat waktu-jalan.

Semua metooda kita akan mengembalikan `JsObject`, bukan `JsValue`, agar kita
dapat mengkhususkan dan membuat `DerivedJsEncoder` yang mempunyai penanda tipe
berbeda dengan `JsEncoder`.

{lang="text"}
~~~~~~~~
  import shapeless._, labelled._
  
  sealed trait DerivedJsEncoder[R] {
    def toJsFields(r: R): IList[(String, JsValue)]
  }
  object DerivedJsEncoder {
    def gen[A, R](
      implicit G: LabelledGeneric.Aux[A, R],
      R: Cached[Strict[DerivedJsEncoder[R]]]
    ): JsEncoder[A] = new JsEncoder[A] {
      def toJson(a: A) = JsObject(R.value.value.toJsFields(G.to(a)))
    }
  
    implicit def hcons[K <: Symbol, H, T <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[T]
    ): DerivedJsEncoder[FieldType[K, H] :: T] =
      new DerivedJsEncoder[A, FieldType[K, H] :: T] {
        private val field = K.value.name
        def toJsFields(ht: FieldType[K, H] :: T) =
          ht match {
            case head :: tail =>
              val rest = T.toJsFields(tail)
              H.value.toJson(head) match {
                case JsNull => rest
                case value  => (field -> value) :: rest
              }
          }
      }
  
    implicit val hnil: DerivedJsEncoder[HNil] =
      new DerivedJsEncoder[HNil] {
        def toJsFields(h: HNil) = IList.empty
      }
  
    implicit def ccons[K <: Symbol, H, T <: Coproduct](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[T]
    ): DerivedJsEncoder[FieldType[K, H] :+: T] =
      new DerivedJsEncoder[FieldType[K, H] :+: T] {
        private val hint = ("type" -> JsString(K.value.name))
        def toJsFields(ht: FieldType[K, H] :+: T) = ht match {
          case Inl(head) =>
            H.value.toJson(head) match {
              case JsObject(fields) => hint :: fields
              case v                => IList.single("xvalue" -> v)
            }
  
          case Inr(tail) => T.toJsFields(tail)
        }
      }
  
    implicit val cnil: DerivedJsEncoder[CNil] =
      new DerivedJsEncoder[CNil] {
        def toJsFields(c: CNil) = sys.error("impossible")
      }
  
  }
~~~~~~~~

A> Sebuah pola yang muncul pada banya pustaka derivasi Shapeless yang memperkenalkan
A> "petunjuk" dengan `implicit` default
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   trait ProductHint[A] {
A>     def nulls(field: String): Boolean
A>     def fieldname(field: String): String
A>   }
A>   object ProductHint {
A>     implicit def default[A]: ProductHint[A] = new ProductHint[A] {
A>       def nulls(field: String)     = false
A>       def fieldname(field: String) = field
A>     }
A>   }
A> ~~~~~~~~
A>
A> Pengguna dimaksudkan untuk menyediakan sebuah instans khusus dari `ProductHint`
A> sendiri pada objek pendamping atau objek paket mereka sendiri. Hal semacam ini
A> sangat buruk karena bergantung pada pengurutan implisit yang rapuh dan merupakan
A> sumber dekoherensi kelas tipe: bila kita menderivasi sebuah `JsEncoder[Foo]`,
A> kita akan mendapatkan hasil yang berbeda yang tergantung pada cakupan mana
A> `ProductHint[Foo]` berada. Sangat dianjurkan untuk menghindari hal semacam ini.

Shapeless menentukan jalur kode pada saat kompilasi berdasarkan pada ada atau tidaknya
anotasi yang dapat memberikan potensi kode teroptimasi, walaupun dengan beban
repetisi kode. Hal semacam ini juga berarti bahwa jumlah anotasi yang kita urus,
termasuk sub-tipenya, harus bisa diatur atau bisa saja kita menulis 10 kali jumlah
kode. Kita dapat mengganti tiga anotasi kita menjadi satu anotasi yang berisi
semua parameter kustomasi:

{lang="text"}
~~~~~~~~
  case class json(
    nulls: Boolean,
    field: Option[String],
    hint: Option[String]
  ) extends Annotation
~~~~~~~~

Semua pengguna anotasi harus menyediakan tiga nilai default dan metoda pembantu
tidak tersedia untuk konstruktor anotasi. Kita dapat menulis pengekstrak kustom
sehingga kita tidak harus mengganti kode Magnolia kita.

{lang="text"}
~~~~~~~~
  object json {
    object nulls {
      def unapply(j: json): Boolean = j.nulls
    }
    object field {
      def unapply(j: json): Option[String] = j.field
    }
    object hint {
      def unapply(j: json): Option[String] = j.hint
    }
  }
~~~~~~~~

Kita dapat meminta `Annotation[json, A]` untuk `case class` atau `sealed trait`
agar mendapatkan akses ke anotasi. Namun, kita harus menulis `hcons` dan `ccons`
untuk menangani kedua kasus tersebut karena bukti tidak akan dibuat bila anotasi
tidak ada. Maka dari itu, kita memperkenalkan cakupan implisit dengan prioritas
yang lebih rendah dan meletakkan bukti "tanpa anotasi" disana.

Kita juga dapat meminta bukti `Annotations.Aux[json, A, J]` untuk mendapatkan
`HList` dari anotasi `json` untuk tipe `A`. Hal yang sama, kita harus menyediakan
`hcons` dan `ccoons` untuk menangani kejadian ada-atau-tidaknya sebuah anotasi.

Untuk mendukung anotasi satu ini, kita harus menulis kode empat kali lebih banyak.

Dimulai dengan menulis ulang `JsEncoder` yang hanya menangani kode pengguna yang
tidak mempunyai anotasi apapun. Sekarang, setiap kode yang menggunakan `@json`
akan gagal dikompilasi. Hal ini merupakan jaring pengaman yang cukup baik.

Kita harus menambah sebuah tipe `A` dan `J` ke `DerivedJsEncoder` dan melangkahi
anotasi pada metoda `.toJsObject`-nya. Bukti `.hcons` dan `.ccons` sudah menyediakan
instans untuk `DerivedJsEncoder` dengan anotasi `None.type` dan kita akan memindahkan
mereka ke prioritas yang lebih rendah sehingga kita dapat menangani `Annotation[json, A]`
di prioritas yang lebih tinggi.

Harap perhatikan bahwa bukti untuk `J` sudah diberikan sebelum `R`. Hal ini
sangat penting karena kompilator harus menyelesaikan `J` sebelum dapat menyelesaikan
`R`.

{lang="text"}
~~~~~~~~
  sealed trait DerivedJsEncoder[A, R, J <: HList] {
    def toJsFields(r: R, anns: J): IList[(String, JsValue)]
  }
  object DerivedJsEncoder extends DerivedJsEncoder1 {
    def gen[A, R, J <: HList](
      implicit
      G: LabelledGeneric.Aux[A, R],
      J: Annotations.Aux[json, A, J],
      R: Cached[Strict[DerivedJsEncoder[A, R, J]]]
    ): JsEncoder[A] = new JsEncoder[A] {
      def toJson(a: A) = JsObject(R.value.value.toJsFields(G.to(a), J()))
    }
  
    implicit def hnil[A]: DerivedJsEncoder[A, HNil, HNil] =
      new DerivedJsEncoder[A, HNil, HNil] {
        def toJsFields(h: HNil, a: HNil) = IList.empty
      }
  
    implicit def cnil[A]: DerivedJsEncoder[A, CNil, HNil] =
      new DerivedJsEncoder[A, CNil, HNil] {
        def toJsFields(c: CNil, a: HNil) = sys.error("impossible")
      }
  }
  private[jsonformat] trait DerivedJsEncoder1 {
    implicit def hcons[A, K <: Symbol, H, T <: HList, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :: T, None.type :: J] =
      new DerivedJsEncoder[A, FieldType[K, H] :: T, None.type :: J] {
        private val field = K.value.name
        def toJsFields(ht: FieldType[K, H] :: T, anns: None.type :: J) =
          ht match {
            case head :: tail =>
              val rest = T.toJsFields(tail, anns.tail)
              H.value.toJson(head) match {
                case JsNull => rest
                case value  => (field -> value) :: rest
              }
          }
      }
  
    implicit def ccons[A, K <: Symbol, H, T <: Coproduct, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] =
      new DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] {
        private val hint = ("type" -> JsString(K.value.name))
        def toJsFields(ht: FieldType[K, H] :+: T, anns: None.type :: J) =
          ht match {
            case Inl(head) =>
              H.value.toJson(head) match {
                case JsObject(fields) => hint :: fields
                case v                => IList.single("xvalue" -> v)
              }
            case Inr(tail) => T.toJsFields(tail, anns.tail)
          }
      }
  }
~~~~~~~~

Sekarang kita dapat menambah penanda tipe untuk enam metoda baru tersebut, dan
memenuhi semua kemungkinan dimana anotasi mungkin berada. Harap juga perhatikan
bahwa kita hanya mendukung satu anotasi pada setiap posis. Bila pengguna menyediakan
beberapa anotasi, semua anotasi lain setelah anotasi pertama akan diabaikan.

Saat ini, kita sudah kehabisan nama untuk banyak hal. Maka dari itu, kita akan
menyebutnya, secara arbiter, sebagai `Annotated` bila `A` sudah memiliki anotasi
dan `Custom` bila ada sebuah anotasi pada sebuah bidang:

{lang="text"}
~~~~~~~~
  object DerivedJsEncoder extends DerivedJsEncoder1 {
    ...
    implicit def hconsAnnotated[A, K <: Symbol, H, T <: HList, J <: HList](
      implicit
      A: Annotation[json, A],
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :: T, None.type :: J]
  
    implicit def cconsAnnotated[A, K <: Symbol, H, T <: Coproduct, J <: HList](
      implicit
      A: Annotation[json, A],
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J]
  
    implicit def hconsAnnotatedCustom[A, K <: Symbol, H, T <: HList, J <: HList](
      implicit
      A: Annotation[json, A],
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :: T, Some[json] :: J]
  
    implicit def cconsAnnotatedCustom[A, K <: Symbol, H, T <: Coproduct, J <: HList](
      implicit
      A: Annotation[json, A],
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J]
  }
  private[jsonformat] trait DerivedJsEncoder1 {
    ...
    implicit def hconsCustom[A, K <: Symbol, H, T <: HList, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :: T, Some[json] :: J] = ???
  
    implicit def cconsCustom[A, K <: Symbol, H, T <: Coproduct, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J]
  }
~~~~~~~~

Kita tidak benar-benar butuh `.hconsAnnotated` atau `.hconsAnnotatedCustom`, karena
anotasi pada sebuah `case class` tidak berarti apapun pada penyandian produk tersebut.
kedua metoda tersebut hanya dipakai pada `.cconsAnnotated*`. Maka dari itu, kita
dapat menghapus kedua metoda tersebut.

`.cconsAnnotated` dan `.cconsAnnotatedCustom` dapat didefinisikan sebagai

{lang="text"}
~~~~~~~~
  new DerivedJsEncoder[A, FieldType[K, H] :+: T, None.type :: J] {
    private val hint = A().field.getOrElse("type") -> JsString(K.value.name)
    def toJsFields(ht: FieldType[K, H] :+: T, anns: None.type :: J) = ht match {
      case Inl(head) =>
        H.value.toJson(head) match {
          case JsObject(fields) => hint :: fields
          case v                => IList.single("xvalue" -> v)
        }
      case Inr(tail) => T.toJsFields(tail, anns.tail)
    }
  }
~~~~~~~~

dan

{lang="text"}
~~~~~~~~
  new DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
    private val hintfield = A().field.getOrElse("type")
    def toJsFields(ht: FieldType[K, H] :+: T, anns: Some[json] :: J) = ht match {
      case Inl(head) =>
        val ann = anns.head.get
        H.value.toJson(head) match {
          case JsObject(fields) =>
            val hint = (hintfield -> JsString(ann.hint.getOrElse(K.value.name)))
            hint :: fields
          case v =>
            val xvalue = ann.field.getOrElse("xvalue")
            IList.single(xvalue -> v)
        }
      case Inr(tail) => T.toJsFields(tail, anns.tail)
    }
  }
~~~~~~~~

Guna `.head` dan `.get` mungkin meragukan, namun harap diingat bahwa tipe disini
berupa `::` dan `Some` yang berarti metoda ini total dan aman digunakan.

`.hconsCustom` dan `.cconsCustom` ditulis

{lang="text"}
~~~~~~~~
  new DerivedJsEncoder[A, FieldType[K, H] :: T, Some[json] :: J] {
    def toJsFields(ht: FieldType[K, H] :: T, anns: Some[json] :: J) = ht match {
      case head :: tail =>
        val ann  = anns.head.get
        val next = T.toJsFields(tail, anns.tail)
        H.value.toJson(head) match {
          case JsNull if !ann.nulls => next
          case value =>
            val field = ann.field.getOrElse(K.value.name)
            (field -> value) :: next
        }
    }
  }
~~~~~~~~

dan

{lang="text"}
~~~~~~~~
  new DerivedJsEncoder[A, FieldType[K, H] :+: T, Some[json] :: J] {
    def toJsFields(ht: FieldType[K, H] :+: T, anns: Some[json] :: J) = ht match {
      case Inl(head) =>
        val ann = anns.head.get
        H.value.toJson(head) match {
          case JsObject(fields) =>
            val hint = ("type" -> JsString(ann.hint.getOrElse(K.value.name)))
            hint :: fields
          case v =>
            val xvalue = ann.field.getOrElse("xvalue")
            IList.single(xvalue -> v)
        }
      case Inr(tail) => T.toJsFields(tail, anns.tail)
    }
  }
~~~~~~~~

Terang saja, ada sangat banyak plat cetak, namun bila diperhatikan lebih seksama,
kita dapat melihat bahwa tiap metoda diimplementasika se-efisien mungkin dengan
informasi yang tersedia: alur kode dipilih saaat waktu kompilasi, bukan pada
saat waktu jalan.

Bagi para pengguna yang sangat menginginkan performa bisa saja melakukan faktorisasi
ulang pada kode ini, sehingga informasi anotasi tersedia lebih awal, bukan disisipkan
pada metoda `.toJsFields` dengan lapisan lain. Untuk performa puncak, kita
dapat memperlakukan tiap kustomasi sebagai anotasi terpisah, namun hal tersebut
menambah lagi jumlah kode yang kita tulis, dengan tambahan beban waktu kompilasi
pada pengguna hilir. Optimasi semacam itu berapa diluar cakupan buku ini, namun
hal tersebut bisa saja dilakukan dan praktik di lapangan memang demikian adanya:
pemindahan beban kerja dari waktu-jalan ke waktu-kompilasi merupakan hal yang
paling menarik dari pemrograman generik.

Satu lagi kekurangan yang harus kita sadari: [`LabelledGeneric` tidak kompatibel dengan `scalaz.@@`](https://github.com/milessabin/shapeless/issue/309),
namun, tentu saja ada penyiasatannya. Misalkan kita ingin mengabaikan label, kita
dapat menambah aturan derivasi berikut pada objek pendamping dari penyandi dan
pembaca sandi

{lang="text"}
~~~~~~~~
  object JsEncoder {
    ...
    implicit def tagged[A: JsEncoder, Z]: JsEncoder[A @@ Z] =
      JsEncoder[A].contramap(Tag.unwrap)
  }
  object JsDecoder {
    ...
    implicit def tagged[A: JsDecoder, Z]: JsDecoder[A @@ Z] =
      JsDecoder[A].map(Tag(_))
  }
~~~~~~~~

Lalu kita dapat men-derivasi sebuah `JsDecoder` untuk `TradeTemplate` kita dari
bab 5

{lang="text"}
~~~~~~~~
  final case class TradeTemplate(
    otc: Option[Boolean] @@ Tags.Last
  )
  object TradeTemplate {
    implicit val encoder: JsEncoder[TradeTemplate] = DerivedJsEncoder.gen
  }
~~~~~~~~

Namun, kita malah mendapat sebuah galat kompilator

{lang="text"}
~~~~~~~~
  [error] could not find implicit value for parameter G: LabelledGeneric.Aux[A,R]
  [error]   implicit val encoder: JsEncoder[TradeTemplate] = DerivedJsEncoder.gen
  [error]                                                                     ^
~~~~~~~~

Penyiasatan masalah ini adalah dengan memperkenalkan bukti untuk `H @@ Z` pada
cakupan impliist yang lebih rendah dan memanggil kode tersebut, sehingga kompilator
dapat menemukannya:

{lang="text"}
~~~~~~~~
  object DerivedJsEncoder extends DerivedJsEncoder1 with DerivedJsEncoder2 {
    ...
  }
  private[jsonformat] trait DerivedJsEncoder2 {
    this: DerivedJsEncoder.type =>
  
    // WORKAROUND https://github.com/milessabin/shapeless/issues/309
    implicit def hconsTagged[A, K <: Symbol, H, Z, T <: HList, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H @@ Z]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H @@ Z] :: T, None.type :: J] = hcons(K, H, T)
  
    implicit def hconsCustomTagged[A, K <: Symbol, H, Z, T <: HList, J <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsEncoder[H @@ Z]],
      T: DerivedJsEncoder[A, T, J]
    ): DerivedJsEncoder[A, FieldType[K, H @@ Z] :: T, Some[json] :: J] = hconsCustom(K, H, T)
  }
~~~~~~~~

Dan untungnya, kita hanya perlu memikirkan tentang produk, karena koproduk tidak
dapat dilabeli.


### `JsDecoder`

Bagian pembacaan sandi kurang lebih sama dengan contoh sebelumnya. Kita dapat
menyusun sebuah instans dari `Field[K, H]` dengan metoda bantuan `field[K](h: D]`.
Kita hanya menulis default yang masuk akal saja:

{lang="text"}
~~~~~~~~
  sealed trait DerivedJsDecoder[A] {
    def fromJsObject(j: JsObject): String \/ A
  }
  object DerivedJsDecoder {
    def gen[A, R](
      implicit G: LabelledGeneric.Aux[A, R],
      R: Cached[Strict[DerivedJsDecoder[R]]]
    ): JsDecoder[A] = new JsDecoder[A] {
      def fromJson(j: JsValue) = j match {
        case o @ JsObject(_) => R.value.value.fromJsObject(o).map(G.from)
        case other           => fail("JsObject", other)
      }
    }
  
    implicit def hcons[K <: Symbol, H, T <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsDecoder[H]],
      T: DerivedJsDecoder[T]
    ): DerivedJsDecoder[FieldType[K, H] :: T] =
      new DerivedJsDecoder[FieldType[K, H] :: T] {
        private val fieldname = K.value.name
        def fromJsObject(j: JsObject) = {
          val value = j.get(fieldname).getOrElse(JsNull)
          for {
            head  <- H.value.fromJson(value)
            tail  <- T.fromJsObject(j)
          } yield field[K](head) :: tail
        }
      }
  
    implicit val hnil: DerivedJsDecoder[HNil] = new DerivedJsDecoder[HNil] {
      private val nil               = HNil.right[String]
      def fromJsObject(j: JsObject) = nil
    }
  
    implicit def ccons[K <: Symbol, H, T <: Coproduct](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsDecoder[H]],
      T: DerivedJsDecoder[T]
    ): DerivedJsDecoder[FieldType[K, H] :+: T] =
      new DerivedJsDecoder[FieldType[K, H] :+: T] {
        private val hint = ("type" -> JsString(K.value.name))
        def fromJsObject(j: JsObject) =
          if (j.fields.element(hint)) {
            j.get("xvalue")
              .into {
                case \/-(xvalue) => H.value.fromJson(xvalue)
                case -\/(_)      => H.value.fromJson(j)
              }
              .map(h => Inl(field[K](h)))
          } else
            T.fromJsObject(j).map(Inr(_))
      }
  
    implicit val cnil: DerivedJsDecoder[CNil] = new DerivedJsDecoder[CNil] {
      def fromJsObject(j: JsObject) = fail(s"JsObject with 'type' field", j)
    }
  }
~~~~~~~~

Menambahkan prarasa pengguna dengan cara anotasi, sama halnya dengan `DerivedJsonEncoder`
dan terasa kaku. Jadi, kita akan memperlakukannya sebagai latihan bagi pembaca.

Satu hal penting yang tertinggal: nilai default `case class`. Kita dapat meminta
bukti namun yang menjadi masalah adalah kita tidak dapat lagi menggunakan mekanisme
derivasi yang sama untuk produk dan koproduk: bukti tidak akan pernah dibuat untuk
koproduk.

Solusi yang dipakai cukup drastis, kita harus memisah `DerivedJsDecoder` menjadi
`DerivedCoproductJsDecoder` dan `DerivedProductJsDecder`. Kita akan memfokuskan
perhatian kita pada `DerivedProductJsDecoder` dan menggunakan `Map` untuk pencarian
bidang yang lebih singkat:

{lang="text"}
~~~~~~~~
  sealed trait DerivedProductJsDecoder[A, R, J <: HList, D <: HList] {
    private[jsonformat] def fromJsObject(
      j: Map[String, JsValue],
      anns: J,
      defaults: D
    ): String \/ R
  }
~~~~~~~~

Kita dapat meminta bukti nilai default dengan `Default.Aux[A, D]` dan menduplikasi
semua metooda agar menangani kasus yang tergantung dari ketersediaan nilai default.
Namun, Shapeless berbaik-hati (untuk kali ini) dan menyediakan `Default.AsOoptions.Aux[A, D]`
yang memperkenankan kita untuk menangani nilai default pada saat waktu-jalan.

{lang="text"}
~~~~~~~~
  object DerivedProductJsDecoder {
    def gen[A, R, J <: HList, D <: HList](
      implicit G: LabelledGeneric.Aux[A, R],
      J: Annotations.Aux[json, A, J],
      D: Default.AsOptions.Aux[A, D],
      R: Cached[Strict[DerivedProductJsDecoder[A, R, J, D]]]
    ): JsDecoder[A] = new JsDecoder[A] {
      def fromJson(j: JsValue) = j match {
        case o @ JsObject(_) =>
          R.value.value.fromJsObject(o.fields.toMap, J(), D()).map(G.from)
        case other => fail("JsObject", other)
      }
    }
    ...
  }
~~~~~~~~

Kita harus memindahkan metoda `.hcons` dan `.hnil` ke objek pasangan dari kelas
tipe tertutup, yang dapat menangani nilai default

{lang="text"}
~~~~~~~~
  object DerivedProductJsDecoder {
    ...
      implicit def hnil[A]: DerivedProductJsDecoder[A, HNil, HNil, HNil] =
      new DerivedProductJsDecoder[A, HNil, HNil, HNil] {
        private val nil = HNil.right[String]
        def fromJsObject(j: StringyMap[JsValue], a: HNil, defaults: HNil) = nil
      }
  
    implicit def hcons[A, K <: Symbol, H, T <: HList, J <: HList, D <: HList](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsDecoder[H]],
      T: DerivedProductJsDecoder[A, T, J, D]
    ): DerivedProductJsDecoder[A, FieldType[K, H] :: T, None.type :: J, Option[H] :: D] =
      new DerivedProductJsDecoder[A, FieldType[K, H] :: T, None.type :: J, Option[H] :: D] {
        private val fieldname = K.value.name
        def fromJsObject(
          j: StringyMap[JsValue],
          anns: None.type :: J,
          defaults: Option[H] :: D
        ) =
          for {
            head <- j.get(fieldname) match {
                     case Maybe.Just(v) => H.value.fromJson(v)
                     case _ =>
                       defaults.head match {
                         case Some(default) => \/-(default)
                         case None          => H.value.fromJson(JsNull)
                       }
                   }
            tail <- T.fromJsObject(j, anns.tail, defaults.tail)
          } yield field[K](head) :: tail
      }
    ...
  }
~~~~~~~~

We can no longer use `@deriving` for products and coproducts: there can only be
one entry in the `deriving.conf` file.

Kita tidak dapat lagi menggunakan `@deriving` untuk produk dan koproduk: hanya
boleh ada satu *entry* pada berkas `deriving.conf`.

Dan jangan lupa untuk menambahkan dukungan `@@`

{lang="text"}
~~~~~~~~
  object DerivedProductJsDecoder extends DerivedProductJsDecoder1 {
    ...
  }
  private[jsonformat] trait DerivedProductJsDecoder2 {
    this: DerivedProductJsDecoder.type =>
  
    implicit def hconsTagged[
      A, K <: Symbol, H, Z, T <: HList, J <: HList, D <: HList
    ](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsDecoder[H @@ Z]],
      T: DerivedProductJsDecoder[A, T, J, D]
    ): DerivedProductJsDecoder[
      A,
      FieldType[K, H @@ Z] :: T,
      None.type :: J,
      Option[H @@ Z] :: D
    ] = hcons(K, H, T)
  
    implicit def hconsCustomTagged[
      A, K <: Symbol, H, Z, T <: HList, J <: HList, D <: HList
    ](
      implicit
      K: Witness.Aux[K],
      H: Lazy[JsDecoder[H @@ Z]],
      T: DerivedProductJsDecoder[A, T, J, D]
    ): DerivedProductJsDecoder[
      A,
      FieldType[K, H @@ Z] :: T,
      Some[json] :: J,
      Option[H @@ Z] :: D
    ] = hconsCustomTagged(K, H, T)
  }
~~~~~~~~


### Derivasi Rumit

Shapeless memperkenankan lebih banyak jenis derivasi bila dibandingkan dengan
`scalaz-deriving` atau Magnolia. Sebagai contoh, sebuah penyandi / pembaca sandi
yang tidak mungkin bisa dilakukan dengan Magnolia. Sebagai contoh, model XML dari
[`xmlformat`](https://github.com/scalaz/scalaz-deriving/tree/master/example/xmlformat)

{lang="text"}
~~~~~~~~
  @deriving(Equal, Show, Arbitrary)
  sealed abstract class XNode
  
  @deriving(Equal, Show, Arbitrary)
  final case class XTag(
    name: String,
    attrs: IList[XAttr],
    children: IList[XTag],
    body: Maybe[XString]
  )
  
  @deriving(Equal, Show, Arbitrary)
  final case class XAttr(name: String, value: XString)
  
  @deriving(Show)
  @xderiving(Equal, Monoid, Arbitrary)
  final case class XChildren(tree: IList[XTag]) extends XNode
  
  @deriving(Show)
  @xderiving(Equal, Semigroup, Arbitrary)
  final case class XString(text: String) extends XNode
~~~~~~~~

Dikarenakan sifat dari XML, akan masuk akal bila kita memiliki pasangan penyandi /
pembaca sandi untuk konten `XChildren` dan `XString`. Kita dapat menyediakan
sebuah derivasi untuk `XChildren` dengan Shapeless, namun kita ingin sebuah
bidang khusus untuk jenis kelas tipe yang dimilikinya dan juga untuk bidang `Option`.
Kita harus mewajibkan bidang-bidang dianotasi dengan nama tersandi. Sebagai
tambahan, saat membaca penyandian, akan lebih baik bila kita memiliki strategi
yang berbeda untuk menangani elemen dari XML, yang mungkin berupa banyak bagian,
tergantung bila tipe kita mempunyai `Semigroup`, `Monoid`, ataupun tidak sama sekali.

A> Banyak pengembang yang percaya XML hanya bentuk lebih lantung dari JSON dengan
A> kurung sudut sebagai pengganti kurung kurawal. Namun, hanya dengan sebuah percobaan
A> penulisan pengubah bolak balik antara `XNode` dan `JsValue` bisa meyakinkan
A> bahwa XML dan JSON merupakan hal yang berbeda, dengan konversi yang hanya mungkin
A> pada kasus-per-kasus.


### Contoh: `UrlQueryWriter`

Sama halnya dengan `xmlformat`, aplikasi `drone-dynamic-agents` dapat diuntungkan
dari derivasi kelas tipe dari kelas tipe `UrqQueryWriter` yang dibangun dengan
instans `UrlEncodedWriter` untuk tiap *entry* bidang. Kelas tipe ini tidak mendukung
koproduk:

{lang="text"}
~~~~~~~~
  @typeclass trait UrlQueryWriter[A] {
    def toUrlQuery(a: A): UrlQuery
  }
  trait DerivedUrlQueryWriter[T] extends UrlQueryWriter[T]
  object DerivedUrlQueryWriter {
    def gen[T, Repr](
      implicit
      G: LabelledGeneric.Aux[T, Repr],
      CR: Cached[Strict[DerivedUrlQueryWriter[Repr]]]
    ): UrlQueryWriter[T] = { t =>
      CR.value.value.toUrlQuery(G.to(t))
    }
  
    implicit val hnil: DerivedUrlQueryWriter[HNil] = { _ =>
      UrlQuery(IList.empty)
    }
    implicit def hcons[Key <: Symbol, A, Remaining <: HList](
      implicit Key: Witness.Aux[Key],
      LV: Lazy[UrlEncodedWriter[A]],
      DR: DerivedUrlQueryWriter[Remaining]
    ): DerivedUrlQueryWriter[FieldType[Key, A] :: Remaining] = {
      case head :: tail =>
        val first =
          Key.value.name -> URLDecoder.decode(LV.value.toUrlEncoded(head).value, "UTF-8")
        val rest = DR.toUrlQuery(tail)
        UrlQuery(first :: rest.params)
    }
  }
~~~~~~~~

Cukup masuk akal bila kita bertanya apakan 30 baris kode ini memang peningkatan
dari 8 baris untuk 2 instans manual yang dibutuhkan oleh aplikasi kita: pilihan
yang ditentukan kasus-per-kasus.

Agar lebih lengkap, derivasi `UrlEncodedWriter` dapat ditulis dengan Magnolia

{lang="text"}
~~~~~~~~
  object UrlEncodedWriterMagnolia {
    type Typeclass[a] = UrlEncodedWriter[a]
    def combine[A](ctx: CaseClass[UrlEncodedWriter, A]) = a =>
      Refined.unsafeApply(ctx.parameters.map { p =>
        p.label + "=" + p.typeclass.toUrlEncoded(p.dereference(a))
      }.toList.intercalate("&"))
    def gen[A]: UrlEncodedWriter[A] = macro Magnolia.gen[A]
  }
~~~~~~~~


### Sisi Gelap Derivasi

> "Beware fully automatic derivation. Anger, fear, aggression; the dark side of
> the derivation are they. Easily they flow, quick to join you in a fight. If once
> you start down the dark path, forever will it dominate your compiler, consume
> you it will."
> 
> ― an ancient Shapeless master

Selain semua peringatan mengenai derivasi otomatis yang telah ditulis untuk
Magnolia, Shapeless jauh lebih mengkuatirkan. Tak hanya derivasi otomatis
Shapeless [sering kali penyebab lambannya kompilasi](https://www.scala-lang.org/blog/2018/06/04/scalac-profiling.html),
Shapeless juga merupakan sumber dari kutu koherensi kelas tipe.

Derivasi otomatis adalah ketika `def gen` bersifat `implicit` yang mana sebuah
panggilan akan melakukan rekursi untuk semua *entry* pada TDA. Karena cara kerja
cakupan implisit, sebuah `implicit def` akan memiliki prioritas lebih tinggi
dibandingkan dengan instans kustom pada objek pendamping dan menjadi sumber
dekoherensi kelas tipe. Sebagai contoh, perhatikan kode berikut bila `.gen` kita
`implicit`

{lang="text"}
~~~~~~~~
  import DerivedJsEncoder._
  
  @xderiving(JsEncoder)
  final case class Foo(s: String)
  final case class Bar(foo: Foo)
~~~~~~~~

Kita mungkin berharap bentuk tersandi otomatis dari `Bar("hello")` terlihat
seperti

{lang="text"}
~~~~~~~~
  {
    "foo":"hello"
  }
~~~~~~~~

karena kita menggunakan `xderiving` untuk `Foo`. Namun, hasil yang diberikan
adalah sebagai berikut

{lang="text"}
~~~~~~~~
  {
    "foo": {
      "s":"hello"
    }
  }
~~~~~~~~

Yang lebih parah adalah saat metoda implisit ditambahkan pada objek pendamping
dari kelas tipe. Hal ini berarti kelas tipe selalu diderivasi pada saat penggunaan
dan pengguna tidak dapat memilih untuk tidak menggunakannya.

Secara mendasar, saat menulis program generik, `implicit` dapat diabaikan oleh
kompilator, bergantung pada cakupan. Hal ini berarti kita kehilangan keamanan pada
waktu-kompilasi yang menjadi motivasi utama atas pemrograman pada tingkat tipe.

Semua akan menjadi lebih mudah pada sisi baik, dimana `implicit` hanya digunakan
untuk kelas tipe yang koheren dan unik secara global. Ketakutan atas plat cetak
merupakan pemicu ke sisi gelap. Ketakutan akan berubah menjadi amarah. Amarah
akan menyebabkan benci. Dan, benci akan menyebabkan penderitaan.

## Performa

Tidak ada sesuatu yang namanya Busur Gandiwa bila kita berbicara mengenai derivasi
kelas tipe. Salah satu hal yang harus dipertimbangkan adalah performa: baik pada
saat kompilasi maupun waktu-jalan.


#### Waktu Kompilasi

Bila kita berbicara mengenai waktu kompilasi, Shapeless merupakan pencilan. Bukan
hal yang luar biasa bila kita mendapati sebuah proyek kecil menderita penggelembungan
waktu kompilasi dari satu detik menjadi satu menit. Untuk mengusut masalah kompilasi,
kita dapat melakukan *profiling* (lol, help) terhadap aplikasi kita dengan menggunakan
plugin `scalac-profiling`

{lang="text"}
~~~~~~~~
  addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0")
  scalacOptions ++= Seq("-Ystatistics:typer", "-P:scalac-profiling:no-profiledb")
~~~~~~~~

Potongan diatas akan menghasilkan keluaran yang dapat menghasilkan sebuah graf
api.

Untuk derivasi Shapeless yang jamak digunakan, kita akan mendapat grafik yang menarik

{width=90%}
![](images/implicit-flamegraph-jsonformat-jmh.png)

hampir semua waktu kompilasi dihabiskan untuk resolusi `implicit`. Harap diperhatikan,
grafik ini juga mengikutsertakan kompilasi `scalaz-deriving`, Magnolia, dan instans
manual. Namun, komputasi dari Shapeless mendominasi grafik tersebut.

Dan, ini bila kita berhasil melakukan *profiling*. Bila ada masalah dengan
derivasi shapeless, kompilator dapat tersangkut pada sebuah ikalan tak hingga dan
harus dibunuh.


#### Performa Waktu-Jalan

Bila kita berbicara mengenai performa waktu-jalan, tentu jawabannya selalu *tergantung*.

Bila kita mengasumsikan logika derivasi sudah ditulis secara efisien, kita dapat
mengetahui mana yang lebih cepat dengan menggunakan eksperimentasi.

Pustaka `jsonformat` menggunakan [Java Microbenchmark Harness (JMH)](http://openjdk.java.net/projects/code-tools/jmh/)
pada model yang memetakan ke GeoJSON, GoogleMaps, dan Twitter. Pustaka ini dikontribusikan
oleh Andriy Plokhotnyuk. Ada tiga tes untuk tiap model:

-   penyandian TDA ke `JsValue`
-   pembacaan sandi yang berhasil dari `JsValue` dari poin pertama kembali ke TDA
-   pembacaan sandi yang gagal dari `JsValue` dengan data galat

diterapkan pada implementasi berikut:

-   Magnolia
-   Shapeless
-   manual

dengan optimisasi yang setara untuk semua implementasi. Hasil berupa operasi
per detik (lebih tinggi lebih baik), pada sebuah komputer *desktop* bertenaga,
menggunakan satu utas:

{lang="text"}
~~~~~~~~
  > jsonformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*encode*
  Benchmark                                 Mode  Cnt       Score      Error  Units
  
  GeoJSONBenchmarks.encodeMagnolia         thrpt    5   70527.223 ±  546.991  ops/s
  GeoJSONBenchmarks.encodeShapeless        thrpt    5   65925.215 ±  309.623  ops/s
  GeoJSONBenchmarks.encodeManual           thrpt    5   96435.691 ±  334.652  ops/s
  
  GoogleMapsAPIBenchmarks.encodeMagnolia   thrpt    5   73107.747 ±  439.803  ops/s
  GoogleMapsAPIBenchmarks.encodeShapeless  thrpt    5   53867.845 ±  510.888  ops/s
  GoogleMapsAPIBenchmarks.encodeManual     thrpt    5  127608.402 ± 1584.038  ops/s
  
  TwitterAPIBenchmarks.encodeMagnolia      thrpt    5  133425.164 ± 1281.331  ops/s
  TwitterAPIBenchmarks.encodeShapeless     thrpt    5   84233.065 ±  352.611  ops/s
  TwitterAPIBenchmarks.encodeManual        thrpt    5  281606.574 ± 1975.873  ops/s
~~~~~~~~

Sebagaimana yang kita lihat, implementasi manual memimpin dan diikuti oleh Magnolia.
Implementasi dengan Shapeless memiliki performa 30 - 70% lebih buruk bila dibandingkan
dengan instans manual. Sekarang untuk pembacaan sandi

{lang="text"}
~~~~~~~~
  > jsonformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*decode.*Success
  Benchmark                                        Mode  Cnt       Score      Error  Units
  
  GeoJSONBenchmarks.decodeMagnoliaSuccess         thrpt    5   40850.270 ±  201.457  ops/s
  GeoJSONBenchmarks.decodeShapelessSuccess        thrpt    5   41173.199 ±  373.048  ops/s
  GeoJSONBenchmarks.decodeManualSuccess           thrpt    5  110961.246 ±  468.384  ops/s
  
  GoogleMapsAPIBenchmarks.decodeMagnoliaSuccess   thrpt    5   44577.796 ±  457.861  ops/s
  GoogleMapsAPIBenchmarks.decodeShapelessSuccess  thrpt    5   31649.792 ±  861.169  ops/s
  GoogleMapsAPIBenchmarks.decodeManualSuccess     thrpt    5   56250.913 ±  394.105  ops/s
  
  TwitterAPIBenchmarks.decodeMagnoliaSuccess      thrpt    5   55868.832 ± 1106.543  ops/s
  TwitterAPIBenchmarks.decodeShapelessSuccess     thrpt    5   47711.161 ±  356.911  ops/s
  TwitterAPIBenchmarks.decodeManualSuccess        thrpt    5   71962.394 ±  465.752  ops/s
~~~~~~~~

Pacuan kali ini lebih ketat untuk tempat kedua, dengan Shapeless dan Magnolia
menorehkan hasil yang mirip. Dan pada akhirnya, pembacaan sandi dari sebuah
`JsValue` yang berisi data tidak valid (pada posisi yang memang disengaja agak
kikuk)

{lang="text"}
~~~~~~~~
  > jsonformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*decode.*Error
  Benchmark                                      Mode  Cnt        Score       Error  Units
  
  GeoJSONBenchmarks.decodeMagnoliaError         thrpt    5   981094.831 ± 11051.370  ops/s
  GeoJSONBenchmarks.decodeShapelessError        thrpt    5   816704.635 ±  9781.467  ops/s
  GeoJSONBenchmarks.decodeManualError           thrpt    5   586733.762 ±  6389.296  ops/s
  
  GoogleMapsAPIBenchmarks.decodeMagnoliaError   thrpt    5  1288888.446 ± 11091.080  ops/s
  GoogleMapsAPIBenchmarks.decodeShapelessError  thrpt    5  1010145.363 ±  9448.110  ops/s
  GoogleMapsAPIBenchmarks.decodeManualError     thrpt    5  1417662.720 ±  1197.283  ops/s
  
  TwitterAPIBenchmarks.decodeMagnoliaError      thrpt    5   128704.299 ±   832.122  ops/s
  TwitterAPIBenchmarks.decodeShapelessError     thrpt    5   109715.865 ±   826.488  ops/s
  TwitterAPIBenchmarks.decodeManualError        thrpt    5   148814.730 ±  1105.316  ops/s
~~~~~~~~

Saat kita mengira kita menemukan sebuah pola, Magnolia dan Shapeless menang pacuan
tersebut saat membaca penyandian tidak valid dari data GeoJSON. Namun, instans
manual memenangkan pacuan Google Maps dan Twitter.

Kita ingin mengikut-sertakan `scalaz-deriving` pada perbandingan, jadi kita akan
membandingan implementasi setara dari `Equal` yang dites pada dua nilai yang
berisi nilai yang sama (`True`) dan dua nilai yang memiliki isi yang sedikit berbeda
(`False`)

{lang="text"}
~~~~~~~~
  > jsonformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*equal*
  Benchmark                                     Mode  Cnt        Score       Error  Units
  
  GeoJSONBenchmarks.equalScalazTrue            thrpt    5   276851.493 ±  1776.428  ops/s
  GeoJSONBenchmarks.equalMagnoliaTrue          thrpt    5    93106.945 ±  1051.062  ops/s
  GeoJSONBenchmarks.equalShapelessTrue         thrpt    5   266633.522 ±  4972.167  ops/s
  GeoJSONBenchmarks.equalManualTrue            thrpt    5   599219.169 ±  8331.308  ops/s
  
  GoogleMapsAPIBenchmarks.equalScalazTrue      thrpt    5    35442.577 ±   281.597  ops/s
  GoogleMapsAPIBenchmarks.equalMagnoliaTrue    thrpt    5    91016.557 ±   688.308  ops/s
  GoogleMapsAPIBenchmarks.equalShapelessTrue   thrpt    5   107245.505 ±   468.427  ops/s
  GoogleMapsAPIBenchmarks.equalManualTrue      thrpt    5   302247.760 ±  1927.858  ops/s
  
  TwitterAPIBenchmarks.equalScalazTrue         thrpt    5    99066.013 ±  1125.422  ops/s
  TwitterAPIBenchmarks.equalMagnoliaTrue       thrpt    5   236289.706 ±  3182.664  ops/s
  TwitterAPIBenchmarks.equalShapelessTrue      thrpt    5   251578.931 ±  2430.738  ops/s
  TwitterAPIBenchmarks.equalManualTrue         thrpt    5   865845.158 ±  6339.379  ops/s
~~~~~~~~

Sesuai dengan perkiraan kita, instans manual jauh lebih cepat bila dibandingkan
dengan yang lainnya. Disusul dengan Shapeless dengan derivasi otomatis. `scalaz-deriving`
bekerja keras untuk GeoJSON namun kalah mengenaskan pada pacuan Google Maps dan
Twitter. Tes tentang `False` kurang lebih memiliki hasil yang sama:

{lang="text"}
~~~~~~~~
  > jsonformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*equal*
  Benchmark                                     Mode  Cnt        Score       Error  Units
  
  GeoJSONBenchmarks.equalScalazFalse           thrpt    5    89552.875 ±   821.791  ops/s
  GeoJSONBenchmarks.equalMagnoliaFalse         thrpt    5    86044.021 ±  7790.350  ops/s
  GeoJSONBenchmarks.equalShapelessFalse        thrpt    5   262979.062 ±  3310.750  ops/s
  GeoJSONBenchmarks.equalManualFalse           thrpt    5   599989.203 ± 23727.672  ops/s
  
  GoogleMapsAPIBenchmarks.equalScalazFalse     thrpt    5    35970.818 ±   288.609  ops/s
  GoogleMapsAPIBenchmarks.equalMagnoliaFalse   thrpt    5    82381.975 ±   625.407  ops/s
  GoogleMapsAPIBenchmarks.equalShapelessFalse  thrpt    5   110721.122 ±   579.331  ops/s
  GoogleMapsAPIBenchmarks.equalManualFalse     thrpt    5   303588.815 ±  2562.747  ops/s
  
  TwitterAPIBenchmarks.equalScalazFalse        thrpt    5   193930.568 ±  1176.421  ops/s
  TwitterAPIBenchmarks.equalMagnoliaFalse      thrpt    5   429764.654 ± 11944.057  ops/s
  TwitterAPIBenchmarks.equalShapelessFalse     thrpt    5   494510.588 ±  1455.647  ops/s
  TwitterAPIBenchmarks.equalManualFalse        thrpt    5  1631964.531 ± 13110.291  ops/s
~~~~~~~~

Performa waktu-jalan dari `scalaz-deriving`, Magnolia, dan Shapeless biasanya
cukup baik. Tentu kita juga harus realistis: kita tidak menulis aplikasi yang
harus mampu menyandikan 130.000 nilai ke JSON tiap detiknya, pada satu *core*,
di JVM. Bila hal semacam itu menjadi masalah, silakan berpaling ke C++.

Agak tidak mungkin instans terderivasi menjadi penyebab macetnya performa aplikasi.
Bahkan bila memang demikian adanya, ada perahu penyelamat dengan penulisan ulang,
yang jauh lebih leluasa dan berbahaya: lebih mudah terjadi salah ketik, pengenalan
kutu, dan kemunduran performa tanpa sengaja saat menulis instans manual.

Kesimpulannya: derivasi tipu-tipu dan makro jaman baheula bukan tandingan untuk
instans yang ditulis secara manual, tong.

A> Kita dapat mengabiskan sisa hidup kita dengan [`async-profiler`](https://github.com/jvm-profiling-tools/async-profiler)
A> untuk menyidik graf api CPU dan alokasi objek untuk membuat implementasi diatas
A> menjadi lebih cepat. Sebagai contoh, ada beberapa optimasi pada basis kode
A> `jsonformat` tidak direproduksi disini seperti pencarian bidang `JsObject` yang
A> lebih teroptimasi, dan pengikutsertaan `.xmap`, `.map`, dan `.contramap` pada
A> kelas tipe yang relevan. Namun, kita harus mengakui bahwa basis kode tersebut
A> memiliki fokus utama keterbacaan, bukan optimasi, dan masih mencapai performa
A> yang luar biasa.


## Kesimpulan

Saat menentukan teknologi yang akan digunakan untuk menderivasi kelas tipe,
bagan fitur ini mungkin membantu:

| Fitur           | Scalaz | Magnolia | Shapeless    | Manual         |
|--------------  |------ |-------- |------------ |------------ |
| `@deriving`     | ya     | ya       | ya            |               |
| Hukum           | ya     |          |               |               |
| Kompilasi copat | ya     | ya       |               | yes           |
| Nama bidang     |        | ya       | ya            |               |
| Anotasi         |        | ya       | sebagian      |               |
| Nilai default   |        | ya       | dengan kurang |               |
| Rumit           |        |          | memedihkan    |               |
| Performa        |        |          |               | masuk pak eko |

Pilih `scalaz-deriving` bila memungkinkan, gunakan Magnolia untuk penyandian /
pembacaan sandi atau bila performa agak penting, dan gunakan Shapeless untuk
derivasi yang rumit bila waktu kompilasi tidak menjadi masalah.

Instans manual selalu menjadi pelampung untuk kasus khusus dan untuk mencapai
performa paling akhir. Hindari kutu karena salah ketik pada instans manual
dengan menggunakan alat penghasil kode.


# Merangkai Aplikasi

Untuk menutup buku ini, kita akan menerapkan apa yang telah kita pelajari dengan
menulis contoh aplikasi dan mengimplementasikan sebuah klien dan peladen HTTP
menggunakan pustaka pemrogaram fungsional murni (lol, help) [http4s](https://http4s.org/).

Kode sumber dari aplikasi `drone-dynamic-agents` tersedia bersama dengan sumber
kode buku pada `https://github.com/fommil/fpmortals` pada direktori `example`.
Untuk membaca bab ini, tidak perlu berada di depan komputer, namun kebanyakan
pembaca mungkin memilih untuk melihat-lihat basis-kode sebagai tambahan tulisan
ini.

Beberapa bagian dari aplikasi sengaja belum diimplementasikan dan digunakan
sebagai latihan bagi pembaca. Silakan lihat `README` untuk instruksi lebih lanjut.


## Ikhtisar

Aplikasi utama kita hanya membutuhkan sebuah implementasi untuk aljabar `DynAgents`.

{lang="text"}
~~~~~~~~
  trait DynAgents[F[_]] {
    def initial: F[WorldView]
    def update(old: WorldView): F[WorldView]
    def act(world: WorldView): F[WorldView]
  }
~~~~~~~~

Kita sudah memiliki sebuah implementasi, `DynAgentsModule`, yang membutuhkan
implementasi dari aljabar `Drone` dan `Machines`, yang juga membutuhkan sebuah
aljabar `JsonClient`, `LocalClock`, OAuth2, dan lain lain.

Gambaran utuh dari semua aljabar, modul, dan interpreter sangat berguna. Berikut
tata letak kode sumber:

{lang="text"}
~~~~~~~~
  ├── dda
  │   ├── algebra.scala
  │   ├── DynAgents.scala
  │   ├── main.scala
  │   └── interpreters
  │       ├── DroneModule.scala
  │       └── GoogleMachinesModule.scala
  ├── http
  │   ├── JsonClient.scala
  │   ├── OAuth2JsonClient.scala
  │   ├── encoding
  │   │   ├── UrlEncoded.scala
  │   │   ├── UrlEncodedWriter.scala
  │   │   ├── UrlQuery.scala
  │   │   └── UrlQueryWriter.scala
  │   ├── oauth2
  │   │   ├── Access.scala
  │   │   ├── Auth.scala
  │   │   ├── Refresh.scala
  │   │   └── interpreters
  │   │       └── BlazeUserInteraction.scala
  │   └── interpreters
  │       └── BlazeJsonClient.scala
  ├── os
  │   └── Browser.scala
  └── time
      ├── Epoch.scala
      ├── LocalClock.scala
      └── Sleep.scala
~~~~~~~~

Penanda dari semua aljabar dapat diikhtisarkan sebagai

{lang="text"}
~~~~~~~~
  trait Sleep[F[_]] {
    def sleep(time: FiniteDuration): F[Unit]
  }
  
  trait LocalClock[F[_]] {
    def now: F[Epoch]
  }
  
  trait JsonClient[F[_]] {
    def get[A: JsDecoder](
      uri: String Refined Url,
      headers: IList[(String, String)]
    ): F[A]
  
    def post[P: UrlEncodedWriter, A: JsDecoder](
      uri: String Refined Url,
      payload: P,
      headers: IList[(String, String)]
    ): F[A]
  }
  
  trait Auth[F[_]] {
    def authenticate: F[CodeToken]
  }
  trait Access[F[_]] {
    def access(code: CodeToken): F[(RefreshToken, BearerToken)]
  }
  trait Refresh[F[_]] {
    def bearer(refresh: RefreshToken): F[BearerToken]
  }
  trait OAuth2JsonClient[F[_]] {
    // same methods as JsonClient, but doing OAuth2 transparently
  }
  
  trait UserInteraction[F[_]] {
    def start: F[String Refined Url]
    def open(uri: String Refined Url): F[Unit]
    def stop: F[CodeToken]
  }
  
  trait Drone[F[_]] {
    def getBacklog: F[Int]
    def getAgents: F[Int]
  }
  
  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[MachineNode ==>> Epoch]
    def start(node: MachineNode): F[Unit]
    def stop(node: MachineNode): F[Unit]
  }
~~~~~~~~

Harap diperhatikan bahwa beberapa penanda dari bab sebelumnya sudah difaktorisasi
ulang agar menggunakan tipe data Scalaz karena kita tahu bahwa tipe data tersebut
lebih unggul bila dibandingkan dengan pustaka standar.

Tipe data tersebut adalah:

{lang="text"}
~~~~~~~~
  @xderiving(Order, Arbitrary)
  final case class Epoch(millis: Long) extends AnyVal
  
  @deriving(Order, Show)
  final case class MachineNode(id: String)
  
  @deriving(Equal, Show)
  final case class CodeToken(token: String, redirect_uri: String Refined Url)
  
  @xderiving(Equal, Show, ConfigReader)
  final case class RefreshToken(token: String) extends AnyVal
  
  @deriving(Equal, Show, ConfigReader)
  final case class BearerToken(token: String, expires: Epoch)
  
  @deriving(ConfigReader)
  final case class OAuth2Config(token: RefreshToken, server: ServerConfig)
  
  @deriving(ConfigReader)
  final case class AppConfig(drone: BearerToken, machines: OAuth2Config)
  
  @xderiving(UrlEncodedWriter)
  final case class UrlQuery(params: IList[(String, String)]) extends AnyVal
~~~~~~~~

dan kelas tipe yang digunakan adalah:

{lang="text"}
~~~~~~~~
  @typeclass trait UrlEncodedWriter[A] {
    def toUrlEncoded(a: A): String Refined UrlEncoded
  }
  @typeclass trait UrlQueryWriter[A] {
    def toUrlQuery(a: A): UrlQuery
  }
~~~~~~~~

Kita menderivasi kelas tipe yang berguna menggunakan `scalaz-deriving` dan Magnolia.
Kelas tipe `ConfigReader` berasal dari pustaka `pureconfig` dan digunakan untuk
membaca konfigurasi waktu-jalan dari berkas properti HOCON.

Dan tanpa membahas detail bagaimana mengimplementasikan aljabar, kita harus tahu
graf ketergantungan dari `DynAgentsModule`.

{lang="text"}
~~~~~~~~
  final class DynAgentsModule[F[_]: Applicative](
    D: Drone[F],
    M: Machines[F]
  ) extends DynAgents[F] { ... }
  
  final class DroneModule[F[_]](
    H: OAuth2JsonClient[F]
  ) extends Drone[F] { ... }
  
  final class GoogleMachinesModule[F[_]](
    H: OAuth2JsonClient[F]
  ) extends Machines[F] { ... }
~~~~~~~~

Ada dua modul yang mengimplementasikan `OAuth2JsonClient`, satu yang digunakan untuk
aljabar OAuth2 `Refresh` (untuk Google) dan satunya yang menggunakan ulang `BearerToken`
tanpa kadaluarsa (untuk Drone).

{lang="text"}
~~~~~~~~
  final class OAuth2JsonClientModule[F[_]](
    token: RefreshToken
  )(
    H: JsonClient[F],
    T: LocalClock[F],
    A: Refresh[F]
  )(
    implicit F: MonadState[F, BearerToken]
  ) extends OAuth2JsonClient[F] { ... }
  
  final class BearerJsonClientModule[F[_]: Monad](
    bearer: BearerToken
  )(
    H: JsonClient[F]
  ) extends OAuth2JsonClient[F] { ... }
~~~~~~~~

Sampai disini, kita sudah melihat persyaratan untuk `F` agar mempunyai `Applicative[F]`,
`Monad[F]`, dan `MonadState[F, BearerToken]`. Semua persyaratan ini dapat dipenuhi
dengan menggunakan `StateT[Task, BearerToken, ?]` sebagai konteks aplikasi kita.

Walaupun demikian, beberapa aljabar kita hanya mempunyai satu interpreter,
menggunakan `Task`

{lang="text"}
~~~~~~~~
  final class LocalClockTask extends LocalClock[Task] { ... }
  final class SleepTask extends Sleep[Task] { ... }
~~~~~~~~

Namun harap diingat bahwa aljabar kita dapat menyediakan sebuah `liftM` pada objek
pendampingnya, lihat pada bab 7.4 pada bagian Pustaka Transformator Monad, dan
memperkenankan kita untuk mengangkat `LocalClock[Task]` pada konteks `StateT[Task, BearerToken, ?]`
dan pada akhirnya semuanya konsisten.

Sayangnya, cerita tidak berhenti disini. Beberapa hal menjadi semakin kompleks
saat kita beralih pada lapisan selanjutya. `JsonClient` kita mempunyai sebuah
*interpreter* yang memiliki konteks yang berbeda

{lang="text"}
~~~~~~~~
  final class BlazeJsonClient[F[_]](H: Client[Task])(
    implicit
    F: MonadError[F, JsonClient.Error],
    I: MonadIO[F, Throwable]
  ) extends JsonClient[F] { ... }
  object BlazeJsonClient {
    def apply[F[_]](
      implicit
      F: MonadError[F, JsonClient.Error],
      I: MonadIO[F, Throwable]
    ): Task[JsonClient[F]] = ...
  }
~~~~~~~~

Harap perhatikan bahwa konstruktor `BlazeJsonClient` mengembalikan sebuah
`Task[JsonClient[F]]` dan bukan `JsonClient[F]`. Hal ini disebabkan karena
pembuatan klien tersebut memiliki efek: kumpulan koneksi tak tetap dibuat dan
diatur secara internal oleh http4s.

A> `OAuth2JsonClientModuel` membutuhkan sebuah `MonadState` dan `BlazeJsonClient`
A> membutuhkan `MonadError` dan `MonadIO`. Konteks aplikasi kita kemungkinan besar
A> merupkan kombinasi keduanya.
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   StateT[EitherT[Task, JsonClient.Error, ?], BearerToken, ?]
A> ~~~~~~~~
A>
A> Susunan monad. Susunan monad secara otomatis menyediakan instans dari `MonadState`
A> dan `MonadError` saat dilapiskan, jadi kita tidak perlu memikirkan tentang hal ini.
A> Bila kita sudah mempermanenkan implementasi pada *interpreter*, dan mengembalikan
A> sebuah `EitherT[Task, Error, ?]` dari `BlazeJsonClient`, maka instansiasi akan
A> menjadi jauh lebih sulit.

Kita juga tidak boleh lupa bahwa kita harus menyediakan sebuah `RefreshTokon` untuk
`GoogleMachinesModule`. Kita dapat meminta pengguna untuk repot, namun karena kita
baik hati dan menyediakan aplikasi sekali pakai yang menggunakan aljabar `Auth`
dan `Access`. Implementasi `AuthModule` dan `AccessModule` membawa ketergantungan
tambahan. Namun, tidak ada perubahan pada konteks aplikasi `F[_]`.

{lang="text"}
~~~~~~~~
  final class AuthModule[F[_]: Monad](
    config: ServerConfig
  )(
    I: UserInteraction[F]
  ) extends Auth[F] { ... }
  
  final class AccessModule[F[_]: Monad](
    config: ServerConfig
  )(
    H: JsonClient[F],
    T: LocalClock[F]
  ) extends Access[F] { ... }
  
  final class BlazeUserInteraction private (
    pserver: Promise[Void, Server[Task]],
    ptoken: Promise[Void, String]
  ) extends UserInteraction[Task] { ... }
  object BlazeUserInteraction {
    def apply(): Task[BlazeUserInteraction] = ...
  }
~~~~~~~~

*Interpreter* untuk `UserInteraction` merupakan bagian paling kompleks dari
basis kode kita: bagian ini memulai peladen HTTP, mengirim pengguna untuk mengunjungi
sebuah laman web pada peramban mereka, menangkap panggilan balik pada peladen, dan
mengembalikan hasil sembari mematikan peladen web secara aman.

Kita tidak menggunakan `StateT` untuk mengatur keadaan ini, namun kita menggunakan
primitif `Promise` (dari `ioeffect`). Kita harus selalu menggunakan `Promise`
atau `IORef`, bukan `StateT` bila kita menulis *interpreter* `IO`. Tidak saja
`StateT` memiliki dampak performa pada aplikasi utama, namun juga membocorkan
manajemen keadaan internal ke aplikasi utama, dan pada akhirnya harus bertanggung
jawab untuk menyediakan nilai awal. Kita juga tidak dapat menggunakan `StateT`
pada skenario ini karena kita membutuhkan semantik "menanti" yang hanya disediakan
oleh `Promise`.


## `Main`

Bagian paling buruk dari PF adalah memastikan bahwa semua monad selaras dan hal
semacam ini biasa terjadi pada titik mulai `Main`.

Ikalan utama kita adalah

{lang="text"}
~~~~~~~~
  state = initial()
  while True:
    state = update(state)
    state = act(state)
~~~~~~~~

dan kabar baiknya, kode yang asli terlihat seperti

{lang="text"}
~~~~~~~~
  for {
    old     <- F.get
    updated <- A.update(old)
    changed <- A.act(updated)
    _       <- F.put(changed)
    _       <- S.sleep(10.seconds)
  } yield ()
~~~~~~~~

dimana `F` menyimpan keadaan keseluruhan pada sebuah `MonadState[F, WorldView]`.
Kita dapat menempatkannya pada sebuah metoda dengan nama `.step` dan mengulang
selamanya dengan memanggil `.step[F].forever[Unit]`.

Ada dua pendekatan yang dapat kita ambil, dan kita akan mempelajari keduanya.
Yang pertama, dan paling sederhana, adalah membangun sebuah susunan monad yang
sesuai dengan semua aljabar. Semua mendapatkan sebuah metoda `.liftM` agar dapat
diangkat ke susunan yang lebih tinggi.

Kode yang ingin kita tulis untuk mode otentikasi sekali pakai adalah

{lang="text"}
~~~~~~~~
  def auth(name: String): Task[Unit] = {
    for {
      config    <- readConfig[ServerConfig](name + ".server")
      ui        <- BlazeUserInteraction()
      auth      = new AuthModule(config)(ui)
      codetoken <- auth.authenticate
      client    <- BlazeJsonClient
      clock     = new LocalClockTask
      access    = new AccessModule(config)(client, clock)
      token     <- access.access(codetoken)
      _         <- putStrLn(s"got token: $token")
    } yield ()
  }.run
~~~~~~~~

dimana `.readConfig` dan `.putStrLn` merupakan panggilan pustaka. Kita dapat
menganggap mereka sebagai *interpreter* `Task` untuk aljabar yang membaca konfigurasi
waktu-jalan dari aplikasi dan mencetak sebuah *string* ke layar.

Namun, kode ini tidak dapat dikompilasi karena dua alasan. Pertama, kita harus
mempertimbangkan bagaimana bentuk susunan monad kita. Konstruktor `BlazeJsonClient`
mengembalikan `Task` namun metoda milik `JsonClient` membutuhkan sebuah
`MonadError[..., JsonClient.Error]`. Dan monad tersebut dapat disediakan oleh
`EitherT`. Maka dari itu, kita dapat membangun susunan monad umum untuk semua
*for comprehension* (lol, help) sebagai

{lang="text"}
~~~~~~~~
  type H[a] = EitherT[Task, JsonClient.Error, a]
~~~~~~~~

Sayangnya, hal ini juga berarti kita harus mengangkat semua yang mengembalikan
`Task` dengan `.liftM`. Hal semacam ini menambah plat cetak cukup banyak.
Sayangnya, metoda `.liftM` tidak menerima tipe dengan bentuk `H[_]`. `.liftM`
menerima tipe dengan bentuk `H[_[_], _]` sehingga kita harus membuat sebuah
alias tipe untuk membantu kompilator:

{lang="text"}
~~~~~~~~
  type HT[f[_], a] = EitherT[f, JsonClient.Error, a]
  type H[a]        = HT[Task, a]
~~~~~~~~

sekarang kita dapat memanggil `.liftM[HT]` saat kita menerima sebuah `Task`

{lang="text"}
~~~~~~~~
  for {
    config    <- readConfig[ServerConfig](name + ".server").liftM[HT]
    ui        <- BlazeUserInteraction().liftM[HT]
    auth      = new AuthModule(config)(ui)
    codetoken <- auth.authenticate.liftM[HT]
    client    <- BlazeJsonClient[H].liftM[HT]
    clock     = new LocalClockTask
    access    = new AccessModule(config)(client, clock)
    token     <- access.access(codetoken)
    _         <- putStrLn(s"got token: $token").liftM[HT]
  } yield ()
~~~~~~~~

Namun, kode diatas masih belum dapat dikompilasi karena `clock` berupa `LocalClock[Task]`
dan `AccessModule` membutuhkan sebuah `LocalClock[H]`. Kita tinggal menambahkan
plat cetak `.liftM` yang dibutuhkan pada objek pendamping dari `LocalClock` dan
pada akhirnya dapat mengangkat semua aljabar

{lang="text"}
~~~~~~~~
  clock     = LocalClock.liftM[Task, HT](new LocalClockTask)
~~~~~~~~

dan semuanya berhasil dikompilasi.

Pendekatan kedua adalah dengan membuat sebuah aplikasi yang lebih kompleks,
namun dibutuhkan bila terjadi konflik pada susunan monad, seperti yang kita
butuhkan pada ikalan utama kita. Bila kita melakukan analisis, kita akan menemukan
bahwa monad berikutlah yang kita butuhkan:

-   `MonadError[F, JsonClient.Error]` untuk penggunaan `JsonClient`
-   `MonadState[F, BearerToken]` untuk penggunaan`OAuth2JsonClient`
-   `MonadState[F, WorldView]` untuk ikalan utama kita

Sayangnya, persyaratan dua `MonadState` menyebabkan konflik. Kita dapat membuat
sebuah tipe data yang menangkap semua keadaan program. Namun, hal tersebut merupakan
abstraksi yang penuh kebocoran. Maka dari itu, kita akan melapiskan komprehensi
*for* (lol, help) kita dan menyediakan keadaan saat dibutuhkan.

Sekarang kita harus berpikir mengenai tiga lapisan, yang kita sebut `F`, `G`, dan `H`

{lang="text"}
~~~~~~~~
  type HT[f[_], a] = EitherT[f, JsonClient.Error, a]
  type GT[f[_], a] = StateT[f, BearerToken, a]
  type FT[f[_], a] = StateT[f, WorldView, a]
  
  type H[a]        = HT[Task, a]
  type G[a]        = GT[H, a]
  type F[a]        = FT[G, a]
~~~~~~~~

Dan sekarang saatnya berita buruk mengenai `.liftM`. Metoda ini hanya berlaku
pada satu lapisan pada satu waktu. Bila kita mempunyai sebuah `Task[A]` dan kita
ingin sebuah `F[A]`, kita harus melangkahi semua lapisan dan menulis
`ta.liftM[HT].liftM[GT].liftM[FT]`. Hal yang sama saat kita mengangkat aljabar,
kita harus memanggil `liftM` berulang kali. Untuk mendapatkan `Sleep[F]`, kita
harus menulis

{lang="text"}
~~~~~~~~
  val S: Sleep[F] = {
    import Sleep.liftM
    liftM(liftM(liftM(new SleepTask)))
  }
~~~~~~~~

dan untuk mendapatkan `LocalClock[G]` kita harus melakukan dua kali pengangkatan

{lang="text"}
~~~~~~~~
  val T: LocalClock[G] = {
    import LocalClock.liftM
    liftM(liftM(new LocalClockTask))
  }
~~~~~~~~

Dan aplikasi utama menjadi

{lang="text"}
~~~~~~~~
  def agents(bearer: BearerToken): Task[Unit] = {
    ...
    for {
      config <- readConfig[AppConfig]
      blaze  <- BlazeJsonClient[G]
      _ <- {
        val bearerClient = new BearerJsonClientModule(bearer)(blaze)
        val drone        = new DroneModule(bearerClient)
        val refresh      = new RefreshModule(config.machines.server)(blaze, T)
        val oauthClient =
          new OAuth2JsonClientModule(config.machines.token)(blaze, T, refresh)
        val machines = new GoogleMachinesModule(oauthClient)
        val agents   = new DynAgentsModule(drone, machines)
        for {
          start <- agents.initial
          _ <- {
            val fagents = DynAgents.liftM[G, FT](agents)
            step(fagents, S).forever[Unit]
          }.run(start)
        } yield ()
      }.eval(bearer).run
    } yield ()
  }
~~~~~~~~

dimana ikalan bagian luar menggunakan `Task`, ikalan tengah menggunakan `G`,
dan ikalan dalam menggunakan `F`.

Panggilan ke `.run(start)` dan `.eval(bearer)` adalah dimana kita menyediakan
keadan awal untuk bagian `StateT` aplikasi kita. `.run` digunakan untuk menyingkap
galat `EitherT`.

Kita dapat memanggil dua titik awal aplikasi ini dari `SafeApp` kita

{lang="text"}
~~~~~~~~
  object Main extends SafeApp {
    def run(args: List[String]): IO[Void, ExitStatus] = {
      if (args.contains("--machines")) auth("machines")
      else agents(BearerToken("<invalid>", Epoch(0)))
    }.attempt[Void].map {
      case \/-(_)   => ExitStatus.ExitNow(0)
      case -\/(err) => ExitStatus.ExitNow(1)
    }
  }
~~~~~~~~

dan menjalankannya.

{lang="text"}
~~~~~~~~
  > runMain fommil.dda.Main --machines
  [info] Running (fork) fommil.dda.Main --machines
  ...
  [info] Service bound to address /127.0.0.1:46687
  ...
  [info] Created new window in existing browser session.
  ...
  [info] Headers(Host: localhost:46687, Connection: keep-alive, User-Agent: Mozilla/5.0 ...)
  ...
  [info] POST https://www.googleapis.com/oauth2/v4/token
  ...
  [info] got token: "<elided>"
~~~~~~~~

Hore!


## Blaze

Kita mengimplementasikan klien dan peladen HTTP dengan pustaka pihak ketiga `http4s`.
*Interpreter* untuk aljabar klien dan peladen disebut *Blaze*.

Kita butuh ketergantungan sebagai berikut

{lang="text"}
~~~~~~~~
  val http4sVersion = "0.18.16"
  libraryDependencies ++= Seq(
    "org.http4s"            %% "http4s-dsl"          % http4sVersion,
    "org.http4s"            %% "http4s-blaze-server" % http4sVersion,
    "org.http4s"            %% "http4s-blaze-client" % http4sVersion
  )
~~~~~~~~


### `BlazeJsonClient`

Sekarang kita butuh beberapa impor

{lang="text"}
~~~~~~~~
  import org.http4s
  import org.http4s.{ EntityEncoder, MediaType }
  import org.http4s.headers.`Content-Type`
  import org.http4s.client.Client
  import org.http4s.client.blaze.{ BlazeClientConfig, Http1Client }
~~~~~~~~

Modul `Client` dapat diringkas menjadi

{lang="text"}
~~~~~~~~
  final class Client[F[_]](
    val shutdown: F[Unit]
  )(implicit F: MonadError[F, Throwable]) {
    def fetch[A](req: Request[F])(f: Response[F] => F[A]): F[A] = ...
    ...
  }
~~~~~~~~

dimana `Request` dan `Response` merupakan tipe data:

{lang="text"}
~~~~~~~~
  final case class Request[F[_]](
    method: Method
    uri: Uri,
    headers: Headers,
    body: EntityBody[F]
  ) {
    def withBody[A](a: A)
                   (implicit F: Monad[F], A: EntityEncoder[F, A]): F[Request[F]] = ...
    ...
  }
  
  final case class Response[F[_]](
    status: Status,
    headers: Headers,
    body: EntityBody[F]
  )
~~~~~~~~

yang terdiri dari

{lang="text"}
~~~~~~~~
  final case class Headers(headers: List[Header])
  final case class Header(name: String, value: String)
  
  final case class Uri( ... )
  object Uri {
    // not total, only use if `s` is guaranteed to be a URL
    def unsafeFromString(s: String): Uri = ...
    ...
  }
  
  final case class Status(code: Int) {
    def isSuccess: Boolean = ...
    ...
  }
  
  type EntityBody[F[_]] = fs2.Stream[F, Byte]
~~~~~~~~

Tipe `EntityBody` merupakan alias untuk `Stream` dari pustaka [`fs2`](https://github.com/functional-streams-for-scala/fs2).
Tipe data `Stream` dapat dianggap sebagai aliran data dengan efek yang ditarik secara
luntung. Tipe data ini diimplementasikan sebagai monad `Free` dengan penangkapan
pengecualian dan interupsi. `Stream` menerima dua parameter tipe: sebuah tipe
dengan efek dan sebuah tipe konten, dan memiliki representasi efisien internal
untuk mengelompokkan data. Sebagai contooh, walaupun kita menggunakan `Stream[F, Byte]`
sebenarnya monad ini membungkus `Array[Byte]` yang tiba melalu jaringan.

Kita dapat mengkonversi *header* (lol, help) dan representasi URL kita menjadi
versi yang dibutuhkan oleh http4s:

{lang="text"}
~~~~~~~~
  def convert(headers: IList[(String, String)]): http4s.Headers =
    http4s.Headers(
      headers.foldRight(List[http4s.Header]()) {
        case ((key, value), acc) => http4s.Header(key, value) :: acc
      }
    )
  
  def convert(uri: String Refined Url): http4s.Uri =
    http4s.Uri.unsafeFromString(uri.value) // we already validated our String
~~~~~~~~

Metoda `.get` dan `.post` keduanya membutuhkan sebuah konversi dari tipe `Response`
http4s menjadi `A`. Kita dapat memisahkannya menjadi sebuah fungsi,

{lang="text"}
~~~~~~~~
  import JsonClient.Error
  
  final class BlazeJsonClient[F[_]] private (H: Client[Task])(
    implicit
    F: MonadError[F, Error],
    I: MonadIO[F, Throwable]
  ) extends JsonClient[F] {
    ...
    def handler[A: JsDecoder](resp: http4s.Response[Task]): Task[Error \/ A] = {
      if (!resp.status.isSuccess)
        Task.now(JsonClient.ServerError(resp.status.code).left)
      else
        for {
          text <- resp.body.through(fs2.text.utf8Decode).compile.foldMonoid
          res = JsParser(text)
            .flatMap(_.as[A])
            .leftMap(JsonClient.DecodingError(_))
        } yield res
    }
  }
~~~~~~~~

`.through(fs2.text.utf8Decode)` digunakan untuk mengkonversi `Stream[Task, Byte]`
menjadi `Stream[Task, String]` dengan `.compile.foldMonoid` menginterpretasinya
dengan `Task`, dan pada akhirnya, menggabungkan semua bagian menggunakan `Monoid[String]`.
Hasilnya adalah `Task[String]`.

Lalu kita mengurai *string* tersebut sebagai JSON dan menggunakan `JsDecoder[A]`
untuk membuat keluaran yang dibutuhkan.

Berikut implementasi kita dari `.get`

{lang="text"}
~~~~~~~~
  def get[A: JsDecoder](
    uri: String Refined Url,
    headers: IList[(String, String)]
  ): F[A] =
    I.liftIO(
        H.fetch(
          http4s.Request[Task](
            uri = convert(uri),
            headers = convert(headers)
          )
        )(handler[A])
      )
      .emap(identity)
~~~~~~~~

`.get` hanyalah berupa saluran (lol, help): kita mengkonversi tipe masukan
menjadi `http4s.Request` lalu memanggil `.fetch` pada `Client` dengan `handler`
kita. `handler` mengembalikan sebuah `Task[Error \/ A]`, namun kita membutuhkan
sebuah `F[A]`. Maka dari itu, kita menggunakan `MonadIO.liftIO` untuk membuat
`F[Error \/ A]` dan melakukan pemetaan menggunakan `.emap` untuk mendorong
galat ke `F`.

Sayangnya, bila kita mencoba mengkompilasi kode ini, akan terjadi kegagalan.
Galat akan terlihat seperti

{lang="text"}
~~~~~~~~
  [error] BlazeJsonClient.scala:95:64: could not find implicit value for parameter
  [error]  F: cats.effect.Sync[scalaz.ioeffect.Task]
~~~~~~~~

Pada dasarnya, ada kucing yang hilang.

Alasan kegagalan ini adalah http4s menggunakan pustaka PF utama lain, bukan Scalaz.
Untungnya, `scalaz-ioeffect` menyediakan lapisan kompatibilitas dan [*shims*](https://github.com/djspiewak/shims) (lol, help)
yang menyediakan konversi implisit tanpa batas. Kita dapat mengkompilasi kode kita
dengan ketergantungan sebagai berikut:

{lang="text"}
~~~~~~~~
  libraryDependencies ++= Seq(
    "com.codecommit" %% "shims"                % "1.4.0",
    "org.scalaz"     %% "scalaz-ioeffect-cats" % "2.10.1"
  )
~~~~~~~~

dan mengimpor

{lang="text"}
~~~~~~~~
  import shims._
  import scalaz.ioeffect.catz._
~~~~~~~~

Implementasi `.post` kurang lebih sama. Namun, kita juga harus menyediakan
instans dari

{lang="text"}
~~~~~~~~
  EntityEncoder[Task, String Refined UrlEncoded]
~~~~~~~~

Untungnya, kelas tipe `EntityEncoder` menyediakan metoda bantuan agar dapat
memperkenankan kita untuk menderivasi dari penyandi `String` yang sudah ada

{lang="text"}
~~~~~~~~
  implicit val encoder: EntityEncoder[Task, String Refined UrlEncoded] =
    EntityEncoder[Task, String]
      .contramap[String Refined UrlEncoded](_.value)
      .withContentType(
        `Content-Type`(MediaType.`application/x-www-form-urlencoded`)
      )
~~~~~~~~

Satu-satunya pembeda antara `.get` dan `.post` adalah cara kita membangun `http4s.Request`

{lang="text"}
~~~~~~~~
  http4s.Request[Task](
    method = http4s.Method.POST,
    uri = convert(uri),
    headers = convert(headers)
  )
  .withBody(payload.toUrlEncoded)
~~~~~~~~

dan bagian utama adalah pembangun, yang hanya berupa pemanggilan `Http1Client`
dengan objek konfigurasi

{lang="text"}
~~~~~~~~
  object BlazeJsonClient {
    def apply[F[_]](
      implicit
      F: MonadError[F, JsonClient.Error],
      I: MonadIO[F, Throwable]
    ): Task[JsonClient[F]] =
      Http1Client(BlazeClientConfig.defaultConfig).map(new BlazeJsonClient(_))
  }
~~~~~~~~


### `BlazeUserInteraction`

Kita harus menyalakan sebuah peladen HTTP, yang sebenarnya jauh lebih mudah bila
dibandingkan yang terdengar. Pertama, kita mengimpor

{lang="text"}
~~~~~~~~
  import org.http4s._
  import org.http4s.dsl._
  import org.http4s.server.Server
  import org.http4s.server.blaze._
~~~~~~~~

Kita harus membuat sebuah `dsl` untuk tipe efek kita, yang nantinya akan kita
impor

{lang="text"}
~~~~~~~~
  private val dsl = new Http4sDsl[Task] {}
  import dsl._
~~~~~~~~

Sekarang, kita dapat menggunakan [dsl http4s](https://http4s.org/v0.18/dsl) untuk
membuat titik akhir HTTP. Kita tidak akan mendeskripsikan apa yang kita lakukan,
kita hanya perlu mengimplementasikannya. Titik akhir ini mirip dengan *DSL* HTTP
lain

{lang="text"}
~~~~~~~~
  private object Code extends QueryParamDecoderMatcher[String]("code")
  private val service: HttpService[Task] = HttpService[Task] {
    case GET -> Root :? Code(code) => ...
  }
~~~~~~~~

Tipe kembalian untuk tiap pencocokan pola adalah sebuah `Task[Response[Task]]`.
Pada implementasi kita, kita menginginkan untuk menerima `code` dan menempatkannya
pada *promise* `ptoken`:

{lang="text"}
~~~~~~~~
  final class BlazeUserInteraction private (
    pserver: Promise[Throwable, Server[Task]],
    ptoken: Promise[Throwable, String]
  ) extends UserInteraction[Task] {
    ...
    private val service: HttpService[Task] = HttpService[Task] {
      case GET -> Root :? Code(code) =>
        ptoken.complete(code) >> Ok(
          "That seems to have worked, go back to the console."
        )
    }
    ...
  }
~~~~~~~~

namun, definisi dari rute layanan kita masih belum cukup. Kita harus menjalankan
sebuah peladen, yang dapat kita lakukan dengan `BlazeBuilder`

{lang="text"}
~~~~~~~~
  private val launch: Task[Server[Task]] =
    BlazeBuilder[Task].bindHttp(0, "localhost").mountService(service, "/").start
~~~~~~~~

Dengan mengikat layanan ke *port* `0`, kita meminta kepada sistem operasi untuk
menetapkan port manapun. Kita dapat menemukan port mana yang sebenarnya berjalan
dengan melakukan kueri pada bidang `server.address`.

Implementasi kita atas metoda `.start` dan `.stop` pun tidak banyak basa-basi

{lang="text"}
~~~~~~~~
  def start: Task[String Refined Url] =
    for {
      server  <- launch
      updated <- pserver.complete(server)
      _ <- if (updated) Task.unit
           else server.shutdown *> fail("server was already running")
    } yield mkUrl(server)
  
  def stop: Task[CodeToken] =
    for {
      server <- pserver.get
      token  <- ptoken.get
      _      <- IO.sleep(1.second) *> server.shutdown
    } yield CodeToken(token, mkUrl(server))
  
  private def mkUrl(s: Server[Task]): String Refined Url = {
    val port = s.address.getPort
    Refined.unsafeApply(s"http://localhost:${port}/")
  }
  private def fail[A](s: String): String =
    Task.fail(new IOException(s) with NoStackTrace)
~~~~~~~~

`1.second` *sleep* penting untuk menghindari matinya peladen sebelum respons
dikirimkan balik ke peramban. IO tidak pernah main-main bila kita berbicara
mengenai performa konkurensi.

Dan pada akhirnya, untuk membuat sebuah `BlazeUserInteraction`, kita hanya perlu
dua *promise* yang belum dimulai

{lang="text"}
~~~~~~~~
  object BlazeUserInteraction {
    def apply(): Task[BlazeUserInteraction] = {
      for {
        p1 <- Promise.make[Void, Server[Task]].widenError[Throwable]
        p2 <- Promise.make[Void, String].widenError[Throwable]
      } yield new BlazeUserInteraction(p1, p2)
    }
  }
~~~~~~~~

Kita bisa saja menggunakan `IO[Void, ?]`, namun karena bagian-bagian aplikasi
kita lainnya menggunakan `Task` (mis, `IO[Throwable, ?]`), kita dapat memperluas
cakupan galat dengan menggunakan `.widenError` agar kita dapat menghindari
pengenalan plat cetak baru sehingga fokus kita kembali terpecah.


## Terima Kasih

Demikian! Kami ucapkan selamat kepada pembaca yang selesai membaca sampai akhir.

Bila pembaca budiman mempelajari sesuatu dari buku ini, mohon untuk memberi tahu
handai-taulan dan kawan-kawan mengenai buku ini. Buku ini tidak memiliki Bagian
Pemasaran, sehingga promosi dari-mulut-ke-mulut sajalah pembaca lain dapat tahu.

Pembaca budiman juga dapat ikut serta atas pengembangan Scalaz dengan bergabung
pada [ruang obrolan gitter](https://gitter.im/scalaz/scalaz). Dari sini, pembaca
dapat meminta saran, membantu pengguna baru (karena pembaca budiman sudah ahli),
dan berkontribusi untuk rilis selanjutnya.
