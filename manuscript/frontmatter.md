{frontmatter}

> "Love is wise; hatred is foolish. In this world, which is getting more
> and more closely interconnected, we have to learn to tolerate each
> other, we have to learn to put up with the fact that some people say
> things that we don't like. We can only live together in that way. But
> if we are to live together, and not die together, we must learn a kind
> of charity and a kind of tolerance, which is absolutely vital to the
> continuation of human life on this planet."
> 
> ― Bertrand Russell


# Mengenai Buku Ini

Buku ini ditujukan untuk tipikal pengembang yang menggunakan bahasa pemrograman
Scala, yang mungkin memiliki latar belakang Java, yang skeptis dan penasaran mengenai
paradigma Pemrograman Fungsional (PF). Buku ini menyuguhkan setiap konsep dengan
contoh praktikan, termasuk dengan penulisan aplikasi web.

Buku ini menggunakan [Scalaz 7.2](https://github.com/scalaz/scalaz) yang merupakan
kerangka kerja Pemrograman Fungsional untuk Scala yang paling populer, stabil,
berprinspip, dan komprehensif.

Buku ini dirancang agar dibaca dari awal sampai akhir secara berurutan, dengan
rehat sejenak antar bab. Pada bab awal, pembaca budiman didorong untuk menggunakan
gaya penulisan kode yang pada bab selanjutnya akan kita tinggalkan: mirip saat
kita mempelajari teori gravitasi Newton saat masih kanak-kanak, dan berlanjut
ke Riemann / Einstein / Maxwell bila kita menjadi mahasiswa Fisika.

Untuk mengikuti buku ini, sebuah komputer tidak diharuskan, namun didorong untuk
mempelajari kode sumber Scalaz. Beberapa potongan kode yang agak kompleks tersedia
bersama dengan [kode sumber buku ini](https://github.com/fommil/fpmortals/) dan
bagi pembaca budiman yang menginginkan latihan praktik, sangat dianjurkan untuk
mengimplementasi ulang Scalaz (dan contoh aplikasi) menggunakan deskripsi parsial
yang ditunjukkan di buku ini.
(dan contoh aplikasi)

Kita juga merekomendasikan [Buku Merah](https://www.manning.com/books/functional-programming-in-scala)
sebagai bacaan lainnya. Buku tersebut membimbing pembaca mengenai bagaimana cara
membangun pustaka PF pada Scala dari prinsip awal.


# Pemberitahuan *Copyleft*

Buku ini **Libre** dan mengikuti filosofi [Perangkat Lunak Bebas](https://www.gnu.org/philosophy/free-sw.id.html):
Pembaca dapat menggunakan buku ini sebagaimana yang pembaca suka, [sumber buku](https://github.com/fommil/fpmortals/)
dapat pembaca distribusikan ulang, mengirimkannya melalui surel, mengunggahnya
pada situs web, mengubahnya, menerjemahkannya, meminta bayaran atasnya, menggabungkannya
dengan bahan lain, menghapus bagian-bagiannya, dan bahkan menggambarinya.

Buku ini bersifat **Copyleft**: bila pembaca budiman mengubah buku ini dan mendistribusikannya,
pembaca juga harus memberikan kebabasan ini kepada pembacanya.

Buku ini menggunakan lisensi [Atribusi-BerbagiSerupa 4.0 Internasional](https://creativecommons.org/licenses/by-sa/4.0/legalcode.id)
(CC BY-SA 4.0).

Semua potongan kode pada buku ini dilisensikan terpisah menggunakan [CC0](https://wiki.creativecommons.org/wiki/CC0),
pembaca dapat menggunakannya tanpa batas. Kutipan dari Scalaz dan pustaka terkait
tetap menggunakan lisensinya, dan dicantumkan pada lampiran.

Contoh aplikasi `drone-dynamic-agents` didistribusikan menggunakan [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html):
hanya yang tercantum pada buku ini tersedia tanpa batasan.


# Ucapan Terima Kasih

Diego Esteban Alonso Blas, Raúl Raja Martínez dan Peter Neyens dari 47
degrees, Rúnar Bjarnason, Tony Morris, John de Goes dan Edward Kmett
atas bantuannya dalam menjelaskan prinsip PF. Kenji SHinoda dan Jason
Zaugg sebagai penulis utama Scalaz, dan Paul Chiusano / Miles Sabin
untuk pembenahan kutu ganas ([SI-2712](https://issues.scala-lang.org/browse/SI-2712)) pada kompiler Scala.

Terima kasih kepada pembaca yang memberikan umpan balik pada draf awal
buku ini.

Beberapa materi yang berguna bagi pemahaman penulis atas konsep-konsep pada
buku ini. Terima kasih kepada Juan Manuel Serrano untuk [All Roads Lead to
Lambda](https://skillsmatter.com/skillscasts/9904-london-scala-march-meetup#video), Pere Villega untuk [On Free Monads](http://perevillega.com/understanding-free-monads), Dick Wall dan Josh Suereth untuk [For:
What is it Good For?](https://www.youtube.com/watch?v=WDaw2yXAa50), Erik Bakker untuk [Options in Futures, how to unsuck them](https://www.youtube.com/watch?v=hGMndafDcc8),
Noel Markham untuk [ADTs for the Win!](https://www.47deg.com/presentations/2017/06/01/ADT-for-the-win/), Sukant Hajra untuk [Classy Monad Transformers](https://www.youtube.com/watch?v=QtZJATIPB0k),
Luka Jacobowitz untuk [Optimizing Tagless Final](https://lukajcb.github.io/blog/functional/2018/01/03/optimizing-tagless-final.html), Vincent Marquez untuk [Index your
State](https://www.youtube.com/watch?v=JPVagd9W4Lo), Gabriel Gonzalez untuk [The Continuation Monad](http://www.haskellforall.com/2012/12/the-continuation-monad.html), dan Yi Lin Wei / Zainab Ali
atas tutorial pada pertemuan di Hack The Tower.

Jiwa-jiwa penolong yang menjelaskan dengan sabar kepada penulis: Merlin Göttlinger, Edmund
Noble, Fabio Labella, Adelbert Chang, Michael Pilquist, Paul Snively, Daniel
Spiewak, Stephen Compall, Brian McKenna, Ryan Delucchi, Pedro Rodriguez, Emily
Pillmore, Aaron Vargo, Tomas Mikula, Jean-Baptiste Giraudeau, Itamar Ravid, Ross
A. Baker, Alexander Konovalov, Harrison Houghton, Alexandre Archambault,
Christopher Davenport, Jose Cardona.



# Praktikalitas

Untuk memulai sebuah projek yang menggunakan pustaka-pustaka yang ditunjukkan
pada buku ini, gunakan versi baru dari Scala dengan fitur spesifik PF diizinkan
(mis., pada `build.sbt`):

{lang="text"}
~~~~~~~~
  scalaVersion in ThisBuild := "2.12.6"
  scalacOptions in ThisBuild ++= Seq(
    "-language:_",
    "-Ypartial-unification",
    "-Xfatal-warnings"
  )
  
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum"     % "0.13.0",
    "org.scalaz"           %% "scalaz-core"    % "7.2.26"
  )
  
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
~~~~~~~~

Agar potongan kode kita tetap pendek, kita tidak akan mengikutsertakan bagian
`import`. Kecuali bila ditentukan selainnya, anggap semua potongan memiliki
impor berikut:

{lang="text"}
~~~~~~~~
  import scalaz._, Scalaz._
  import simulacrum._
~~~~~~~~


