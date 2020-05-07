package lecture10

import cats.effect._
import cats.effect.concurrent.{MVar, Ref, Semaphore}
import cats.syntax.all._
import cats.instances.list._
import scala.concurrent.duration._


object Echo extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    Resource.make(rec.start)(_.cancel).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Long): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put(counter.toString)
      _ <- rec(counter + 1)
    } yield ()

    Resource.make(rec(0).start)(_.cancel).void
  }

  val program: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO.unit)
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
    _ <- Resource.make[IO, Unit](IO.unit)(_ => IO(println("interrupt")))
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.use(_ => IO.never)
}
