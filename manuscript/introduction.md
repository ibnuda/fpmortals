
# Pengantar

Sudah menjadi naluri manusia untuk ragu dan curiga pada paradigma baru.
Sebagai perspektif mengenai betapa berubahnya kita, dan pergeseran
yang sudah kita terima pada JVM, mari kita rekap cepat apa yang terjadi
pada 20 tahun belakangan ini.

Java 1.2 memperkenalkan APA Koleksi, yang memperkenankan kita untuk
menulis metoda yang mengabstraksi koleksi tak tetap. Antarmuka ini
sangat berguna untuk penulisan algoritma umum dan merupakan pondasi
dari basis kode kita.

Namun ada sebuah masalah, kita harus melakukan konversi tipe
pada saat waktu-jalan:

{lang="text"}
~~~~~~~~
  public String first(Collection collection) {
    return (String)(collection.get(0));
  }
~~~~~~~~

Menyikapi hal itu, pengembang mendefinisikan objek domain pada logika bisnis
mereka dan disebut sebagai `CollectionOfThings`. Setelah itu, APA Koleksi
menjadi detail implementasi.

Pada tahun 2005, Java 5 memperkenalkan *generik*, yang memperkenankan kita
untuk mendefinisikan `Collection<Thing>`, mengabstrakkan kontainer **dan**
elemennya. Generik mengubah cara kita menulis Java.

Penulis dari kompiler generik Java, Martin Odersky, lalu menciptakan
Scala dengan sistem tipe yang lebih kuat, data tak-ubah, dan pewarisan
jamak. Bahasa ini membawa penggabungan antara pemrograman berorientasi
objek dan pemrograman fungsional.

Bagi kebanyakan pengembang, PF mempunyai makna penggunaan data tak-ubah
sebanyak mungkin. Namun, keadaan tak-tetak masih menjadi kebatilan yang
harus ada dan juga harus diisolasi dan dikekang. Misal, dengan aktor
Akka atau kelas `synchronized`. Gaya PF semacam ini menghasilkan program
yang lebih sederhana dan dapat dengan mudah diparalelisasi dan distribusi.
Dengan kata lain, peningkatan atas Java. Namun, Gaya semacam ini hanya
berkutat pada permukaan dari keuntungan PF, yang akan kita temukan pada
buku ini.

Scala juga memiliki `Future`, yang mempermudah penulisan aplikasi
asinkronus. Namun, ketika `Future` menjadi tipe kembalian, *semua*
harus ditulis ulang agar mengakomodasinya. Termasuk tes yang harus
tunduk pada tenggat waktu arbiter.

Sekarang, kita memiliki masalah yang sama dengan Java 1.0: tidak ada
cara untuk mengabstraksi eksekusi, sebagaimana kita tidak punya cara
untuk mengabstraksi koleksi.

## Abstraksi atas Eksekusi

Misalkan kita ingin berinteraksi dengan pengguna melalui antarmuka baris perinta.
Kita dapat membaca (menggunakan metoda `read`) apa yang pengguna tulis dan kita
juga dapat menulis (menggunakan metoda `write`) pesan kepada mereka.

{lang="text"}
~~~~~~~~
  trait TerminalSync {
    def read(): String
    def write(t: String): Unit
  }
  
  trait TerminalAsync {
    def read(): Future[String]
    def write(t: String): Future[Unit]
  }
~~~~~~~~

Bagaimana kita  menulis kode generik yang dapat menggemakan masukan pengguna
secara sinkronus maupun asinkronus tergantung pada implementasi waktu-jalan kita?

Kita dapat menulis versi sinkronus dan melapisinya dengan `Future`. Namun,
kita harus memikirkan kumpulan utas mana yang harus  kita gunakan untuk
tugas ini, atau kita dapat menggunakan `Await.result` untuk menanti yang
terjadi pada `Future` dan memperkenalkan penghalangan utas. Yang manapun juga,
akan sangat banyak plat cetak yang digunakan dan kita berurusan dengan APA
yang berbeda secara mendasar dan juga tidak selaras.

Kita juga dapat menyelesaikan masalah, sebagaimana Java 1.2, menggunakan induk
yang sama dengan memakai fitur bahasa milik Scala, *tipe lebih tinggi* (TLT).

A> **Tipe Lebih Tinggi** memperkenankan kita untuk menggunakan *konstruktor tipe*
A> pada parameter tipe kita, yang terlihat seperti `C[_]`. Beginilah cara untuk
A> menyampaikan bahwa apapun `C`, `C` harus menerima sebuah parameter tipe.
A> Sebagai contoh:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   trait Foo[C[_]] {
A>     def create(i: Int): C[Int]
A>   }
A> ~~~~~~~~
A> 
A> `List` is a type constructor because it takes a type (e.g. `Int`) and constructs
A> a type (`List[Int]`). We can implement `Foo` using `List`:
A>
A> `List` merupakan konstruktor tipe karena `List` menerima sebuah tipe dan membangun
A> sebuah tipe (`List[Int]`). Kita dapat mengimplementasikan `Foo` menggunakan
A> `List`:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object FooList extends Foo[List] {
A>     def create(i: Int): List[Int] = List(i)
A>   }
A> ~~~~~~~~
A> 
A> We can implement `Foo` for anything with a type parameter hole, e.g.
A> `Either[String, _]`. Unfortunately it is a bit clunky and we have to
A> create a type alias to trick the compiler into accepting it:
A>
A> Kita dapat mengimplementasikan `Foo` dengan semua yang memiliki rongga parameter
A> tipe. Mis, `Either[String, _]`. Sayangnya, implementasi semacam ini sedikit
A> kikuk dan kita harus membuat alias tipe agar kompiler mau menerimanya:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   type EitherString[T] = Either[String, T]
A> ~~~~~~~~
A>
A> Alias tipe tidak mendefinisikan tipe baru, alias tipe hanya menggunakan
A> substitusi tanpa menyediakan tambahan keamanan tipe. Kompiler melakukan
A> substitusi `EitherString[T]` dengan `Either[String, T]` di seluruh basis
A> kode. Teknik ini dapat digunakan agar kompiler menerima tipe dengan satu
A> rongga walaupun pada kenyataannya ada dua, seperti saat kita mengimplementasikan
A> `Foo` dengan `EitherString`:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object FooEitherString extends Foo[EitherString] {
A>     def create(i: Int): Either[String, Int] = Right(i)
A>   }
A> ~~~~~~~~
A> 
A> Cara lainnya, plugin [kind projector](https://github.com/non/kind-projector/)
A> memperkenankan kita untuk menghindari alias `type` dan menggunakan sintaks
A> `?` untuk memberitahu kompiler dimana rongga tipe berada:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object FooEitherString extends Foo[Either[String, ?]] {
A>     def create(i: Int): Either[String, Int] = Right(i)
A>   }
A> ~~~~~~~~
A> 
A> Dan yang terakhir, ada satu trik janggal yang bisa kita gunakan untuk mengabaikan
A> konstruktor tipe. Definisikan sebuah alias tipe agar setara dengan parameternya:
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   type Id[T] = T
A> ~~~~~~~~
A> 
A> Sebelum melanjutkan, harap dipahami bahwa `Id[Int]` adalah sama dengan `Int`,
A> dengan men-subtitusi `Int` ke `T`. Karena `Id` merupakan konstruktor tipe yang
A> valid, kita dapat menggunakan `Id` pada implementasi `Foo`
A> 
A> {lang="text"}
A> ~~~~~~~~
A>   object FooId extends Foo[Id] {
A>     def create(i: Int): Int = i
A>   }
A> ~~~~~~~~

Kita ingin mendefinisikan `Terminal` untuk konstruktor tipe `C[_]`. Dengan
mendefinisikan `Now` untuk mengkonstruk parameter tipenya (seperti `Id`), kita
dapat menngimplementasika antarmuka umum untuk terminal sinkronus dan asinkronus:

{lang="text"}
~~~~~~~~
  trait Terminal[C[_]] {
    def read: C[String]
    def write(t: String): C[Unit]
  }
  
  type Now[X] = X
  
  object TerminalSync extends Terminal[Now] {
    def read: String = ???
    def write(t: String): Unit = ???
  }
  
  object TerminalAsync extends Terminal[Future] {
    def read: Future[String] = ???
    def write(t: String): Future[Unit] = ???
  }
~~~~~~~~

We can think of `C` as a *Context* because we say "in the context of
executing `Now`" or "in the `Future`".

Kita dapat menganggap `C` sebagai *konteks* karena kita menggunakannya
pada saat berbicara sebagai "pada konteks eksekusi saat ini (`Now`)" atau
"di masa depan (`Future`)".

Namun, kita tidak tahu apapun mengenai `C` dan kita tidak dapat melakukan
apapun dengan `C[String]`. Apa yang kita butuhkan adalah semacam lingkungan
eksekusi yang memperkenankan kita untuk memanggil metoda yang mengembalikan
sebuah `C[T]` dan pada akhirnya dapat melakukan sesuatu pada `T`, termasuk
memanggil metoda lain pada `Terminal`. Kita juga membutuhkan sebuah cara untuk
membungkus sebuah nilai menjadi sebuah `C[_]`. Penanda seperti ini bisa
dibilang bekerja dengan baik, sebagaimana apa yang kita butuhkan pada kalimat
sebelumnya:

{lang="text"}
~~~~~~~~
  trait Execution[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }
~~~~~~~~

yang memperkenankan kita untuk menulis:

{lang="text"}
~~~~~~~~
  def echo[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
    e.chain(t.read) { in: String =>
      e.chain(t.write(in)) { _: Unit =>
        e.create(in)
      }
    }
~~~~~~~~

Sekarang kita dapat berbagi implementasi `echo` antara alur kode sinkronus
dan asinkronus. Kita juga dapat menulis implementasi tiruan untuk `Terminal[Now]`
dan menggunakannya pada tes kita tanpa batas waktu.

Implementasi dari `Execution[Now]` dan `Execution[Future]` dapat digunakan
oleh metoda generik seperti `echo`.

Namun, kode untuk `echo` sendiri cukup mengerikan.

Fitur bahasa Scala `implicit class` memberikan `C` beberapa metoda.
Kita akan memanggil metoda ini `flatMap` dan `map` untuk alasan yang
akan jelas sebentar lagi. Setiap metoda menerima sebuah `implicit Execution[C]`,
namun hal ini tidak lebih daripada `flatMap` dan `map` yang kita gunakan pada
`Seq`, `Option`, dan `Future`

{lang="text"}
~~~~~~~~
  object Execution {
    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
            e.chain(c)(f)
      def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
            e.chain(c)(f andThen e.create)
    }
  }
  
  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    t.read.flatMap { in: String =>
      t.write(in).map { _: Unit =>
        in
      }
    }
~~~~~~~~

Sekarang kita tahu mengapa kita menggunakan `flatMap` sebagai nama metoda:
metoda ini memperkenankan kita untuk menggunakan komprehensi *for* yang
hanya merupakan pemanis sintaks atas `flatMap` dan `map` berlapis.

{lang="text"}
~~~~~~~~
  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    for {
      in <- t.read
       _ <- t.write(in)
    } yield in
~~~~~~~~

`Execution` kita mempunyai penanda yang sama sebagaimana dengan trait pada
Scalaz yang disebut `Monad`. Namun, `chain` adalah `bind` dan `create` adalah
`pure`. Kita menganggap `C` bersifat *monad* bila ada `Monad[C]` implisit
tersedia. Sebagai tambahan Scalaz mempunyai alias tipe `Id`.

Yang bisa diambil adalah: bila kita menulis metoda yang beroperasi pada
tipe monadik, maka kita dapat menulis kode sekuensial yang mengabstraksi
konteks eksekusinya. Disini, kita telah menunjukkan sebuah abstraksi atas
eksekusi sinkronus dan asinkronus. Namun, hal yang sama juga bisa digunakan
untuk penanganan galat yang lebih teliti (dimana `C[_]` berupa `Either[Error, _]`),
manajemen akses pada keadaan volatil, melakukan I/O, atau mengaudit sesi.


## Pemrograman Fungsional Murni

Pemrograman fungsional adalah perilaku penulisan kode dengan *fungsi murni*.
Fungsi murni memiliki tiga properti:

-   **Total**: mengembalikan sebuah nilai untuk setiap masukan yang mungkin
-   **Deterministik**: mengembalikan nilai yang sama untuk masukan yang sama
-   **Rabak**: tak ada interaksi (langsung) dengan dunia luar atau keadaan program

Ketiga properti ini memberikan kita kemampuan yang belum pernah kita miliki untuk
menalar kode kita. Sebagai contoh, validasi masukan akan lebih mudah bila kita
mengisolasi dengan totalitas, penyimpanan ke tembolok mungkin bila fungsi adalah
fungsi deterministik, dan interaksi dengan dunia luar lebih mudah diatur dan dites
bila fungsi tak berhubungan langsung dunia luar.

Sesuatu yang merusak properti ini disebut *efek samping*: akses langsung atau
pengubahan keadaan tak tetap (mis., memelihara sebuah `var` pada kelas atau
menggunakan APA peninggalan yang tidak murni), berkomunikasi dengan sumber daya
eksternal (mis. pencarian berkas atau jaringan), dan pelemparan dan penangkapan
eksepsi.

Kita menulis fungsi murni dengan menghindari eksepsi dan berinteraksi dengan dunia
luar hanya melalui sebuah konteks eksekusi `F[_]` yang aman.

Pada bagian sebelumnya, kita mengabstrakkan eksekusi dan mendefinisikan `echo[Id]`
dan `echo[Future]`. Kita mungkin berharap bahwa pemanggila `echo` tidak akan
menyebabkan efek samping apapun, karena fungsi ini murni. Namun, bila kita
menggunakan `Future` atau `Id` sebagai konteks eksekusi, aplikasi kita akan
mulai mendengarkan stdin:

{lang="text"}
~~~~~~~~
  val futureEcho: Future[String] = echo[Future]
~~~~~~~~

Kita telah merusak kemurnian eksekusi dan tidak lagi menulis kode PF: `futureEcho`
merupakan hasil dari penjalanan `echo` sekali. `Future` mengurung definisi program
dengan *menafsirkannya* (menjalankannya). Dan hasilnya, aplikasi yang dibangun
dengan `Future` akan menyulitkan penalaran.

A> Sebuah ekspresi dikatakan *transparan secara rujukan* bila ekspresi ini dapat
A> digantikan dengan nilai yang berhubungan tanpa mengubah perilaku program.
A> 
A> Fungsi murni transparan secara rujukan dan memperkenankan penggunaan ulang
A> kode sangat tinggi, optimasi performa, pemahaman, dan kontrol dari sebuah program.
A>
A> Fungsi tak-muri, tentu, tidak transparan secara rujukan. Kita tidak dapat
A> mengganti `echo[Future]` dengan sebuah nilai, seperti `val futureEcho`, karena
A> pengguna dapat menulis hal yang berbeda di lain waktu.

Kita dapat mendefinisikan sebuah konteks eksekusi yang aman, `F[_]`

{lang="text"}
~~~~~~~~
  final class IO[A](val interpret: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(interpret()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
  }
  object IO {
    def apply[A](a: =>A): IO[A] = new IO(() => a)
  }
~~~~~~~~

yang dievaluasi secara luntung. `IO` hanyalah sebuah struktur data yang merujuk
pada kode (yang mungkin) tak-murni, dan sebenarnya tidak menjalankan apapun.
Kita dapat mengimplementasikan `Terminal[IO]`

{lang="text"}
~~~~~~~~
  object TerminalIO extends Terminal[IO] {
    def read: IO[String]           = IO { io.StdIn.readLine }
    def write(t: String): IO[Unit] = IO { println(t) }
  }
~~~~~~~~

dan memanggil `echo[IO]` agar dapat mendapatkan kembali sebuah nilai
{lang="text"}
~~~~~~~~
  val delayed: IO[String] = echo[IO]
~~~~~~~~

`val delayed` dapat digunakan ulang karena ini hanya merupaka definisi dari
tugas yang harus diselesaikan. Kita dapat memetakan `String` dan menyusun
program tambahan, sebagaimana kita dapat memetakan sebuah `Future`. `IO`
memaksa kita untuk tetap jujur bahwa kita bergantung pada interaksi dengan
dunia luar, namun tidak mencegah kita untuk mengakses keluaran dari interaksi
tersebut.

Kode tak-murni didalam `IO` hanya akan dievaluasi bila kita menafsirkan nilainya
dengan memanggil `.interpret()`, yang merupakan tindakan tak-murni

{lang="text"}
~~~~~~~~
  delayed.interpret()
~~~~~~~~

Sebuah aplikasi yang tersusun dari program `IO` hanya ditafsirkan satu kali,
pada metoda `main` yang juga disebut sebagai *ujung dunia*.

Pada buku ini, kita memperluas konsep yang diperkenalkan pada bab ini dan
menunjukkan bagaimana cara menulis fungsi murni dan dapat dipelihara yang
mampun mencapai tujuan bisnis kita.


