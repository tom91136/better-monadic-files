package net.kurobako.bmf

import cats.effect.{IO, Resource}
import cats.implicits._
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class PathMSpec extends FlatSpec with Matchers with EitherValues {

	"newTempDir" should "work with Bracket typeclasses" in {


		//		val x: IO[(FileM[IO], Long)] = for{
		//			foo <- File.home.liftFile[IO].create()
		//			_ <- foo.append("foo")
		//			length <- foo.size
		//		} yield (foo, length)

		DirM.newTempDir[IO]()
			.bracket { x =>
				for {
					before <- x.count()
					created <- (x / "a").create()
					exist <- created.exists
					after <- x.count()
				} yield (before, exist, after)

			} {_.deleteUnit()}.unsafeRunSync() should ===((0, true, 1))
	}

	"tempDir" should "not delete file in comprehension" in {
		val x: Resource[IO, (DirM[IO], DirM[IO])] = for {
			a <- DirM.tempDir[IO]("a")
			b <- DirM.tempDir[IO]("b")
		} yield a -> b
		x.use { case (a, b) => a.checked *> b.checked *> IO.unit }.unsafeRunSync()
	}

	"exception" should "be contained" in {
		Thread.setDefaultUncaughtExceptionHandler((t, e) => println(s"[${t.getName}] Silenced:$e"))
		DirM.newTempDir[IO]().bracket { x =>
			for {
				a <- x.delete()
				b <- a.delete()
			} yield b
		} {_.deleteUnit()} // release fails again for deleting a already deleted file
			.attempt.unsafeRunSync()
	}

}
