package lecture8

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.stream.Collectors

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait DirList[F[_], Dir, File] {
  def listFiles(dir: Dir): F[List[File]]
}

trait File2String[F[_], File] {
  def file2String(file: File): F[String]
}

trait MvFile[F[_], Dir, File] {
  def mvFile(file: File, dir: Dir): F[File]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               printer: Printer[F, File],
                               listDir: DirList[F, Dir, File],
                               f2s: File2String[F, File],
                               mvFile: MvFile[F, Dir, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")

    files <- listDir.listFiles(testDir)
    _ <- files.traverse(f => {
      printer.printName(f)
      for {
        s <- f2s.file2String(f)
        d <- mkDir.mkDir(testDir, s(0).toString)
        _ <- mvFile.mvFile(f, d)
      } yield ()
    })
  } yield ()
}

class RealFileSystem[F[_] : Applicative]
  extends MkDir[F, Path]
    with MkFile[F, Path, Path]
    with DirList[F, Path, Path]
    with File2String[F, Path]
    with MvFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def listFiles(dir: Path): F[List[Path]] =
    Files.list(dir).filter(f => Files.isRegularFile(f)).collect(Collectors.toList[Path]).asScala.toList.pure[F]

  override def file2String(file: Path): F[String] =
    file.getFileName.toString.pure[F]

  override def mvFile(file: Path, dir: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]

    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."))
  }
}