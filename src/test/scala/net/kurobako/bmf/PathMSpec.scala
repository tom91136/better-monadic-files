package net.kurobako.bmf

import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.NoSuchFileException

import better.files.File
import net.kurobako.bmf.PathM._
import cats.effect.IO
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import org.scalactic.TypeCheckedTripleEquals._

class PathMSpec extends FlatSpec with Matchers with EitherValues {

	"createTempFile" should "work with Bracket typeclasses" in {




		val x: IO[(FileM[IO], Long)] = for{
			foo <- File.home.liftFile[IO].create()
			_ <- foo.append("foo")
			length <- foo.size
		} yield (foo, length)

		PathM.newTempDir[IO]()
			.bracket { x =>
				for {
					before <- x.count()
					created <- (x / "a").create()
					exist <- created.exists
					after <- x.count()
				} yield (before, exist, after)

			} {_.deleteUnit()}.unsafeRunSync() should ===((0, true, 1))
	}


	"exception" should "be contained" in {
		Thread.setDefaultUncaughtExceptionHandler((t, e) => println(s"[${t.getName}] Silenced:$e"))
		PathM.newTempDir[IO]().bracket { x =>
			for {
				a <- x.delete()
				b <- a.delete()
			} yield b
		} {_.deleteUnit()} // release fails again for deleting a already deleted file
			.attempt.unsafeRunSync()
	}

}