package lecture8

import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.JavaConverters.asScalaBufferConverter

class FileSystemTest extends AnyFlatSpec with Matchers {

  trait env {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    val dir = Files.createTempDirectory("scala-lecture8")
    program.run(dir)
  }

  "Program" should "do all the things it was built to do" in new env {
    Files.exists(dir.resolve("test_dir")) shouldBe true
    val expectedLayout: List[Path] = List(
      dir.resolve("test_dir"),
      dir.resolve("test_dir/b"),
      dir.resolve("test_dir/b/bar"),
      dir.resolve("test_dir/b/baz"),
      dir.resolve("test_dir/f"),
      dir.resolve("test_dir/f/foo"),
    )
    val actual: List[Path] = Files.walk(dir.resolve("test_dir")).sorted().collect(Collectors.toList[Path]).asScala.toList
    actual shouldBe expectedLayout
  }
}
