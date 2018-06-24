package net.kurobako.bmf

import java.io._
import java.nio.charset.Charset
import java.nio.file.{FileSystems, Path, WatchService}
import java.security.MessageDigest

import better.files.File._
import better.files.{DefaultCharset, File, _}
import cats.MonadError
import cats.effect.Resource
import cats.implicits._
import net.kurobako.bmf.FileM.Digest
import net.kurobako.bmf.PathM.{Attrs, LinkOps, MonadThrowable, VisitOps}

import scala.util.control.NonFatal

/** Represents a path; contains operations common to files and directories
  *
  * Operations that causes side effects(does actual IO) will return a `M[_]`; operations
  * that do not have side effects will simply return the value.
  *
  * @param F the [[ MonadError[F[_], A] ]] instance
  * @tparam M the monad type
  */
//noinspection AccessorLikeMethodIsEmptyParen
sealed abstract class PathM[M[_]] private[bmf](implicit val F: MonadThrowable[M]) {

	type S <: PathM[M]

	@inline val file: File
	@inline private[bmf] val newInstance: File => S
	@inline private[bmf] val instance   : S


	/** The path of the file, equivalent to {{{java.nio.file.Path.toString}}} */
	@inline def path: String = file.pathAsString

	@inline private[bmf] def attempt[A](a: => A): M[A] =
		try F.pure(a)
		catch {case NonFatal(e) => F.raiseError(e)}

	/** Checks whether the file exists */
	def exists(implicit lops: LinkOps = LinkOptions.default): M[Boolean] =
		attempt(file.exists(lops))

	def verifiedExists()(implicit lops: LinkOps = LinkOptions.default): M[Option[Boolean]] =
		attempt(file.verifiedExists(lops))

	def checked(implicit lops: LinkOps = LinkOptions.default): M[S] = for {
		exists <- verifiedExists()(lops)
		verified <- exists match {
			case Some(true) => this match {
				case x: FileM[M] if x.file.isDirectory  =>
					F.raiseError[S](new IOException(s"Expected $file to be a regular file(FileM) but was a not"))
				case x: DirM[M] if x.file.isRegularFile =>
					F.raiseError[S](new IOException(s"Expected $file to be a directory(DirM) but was a not"))
				case _                                  => F.pure[S](instance)
			}
			case _          => F.raiseError[S](new IOException(s"$file does not exist"))
		}
	} yield verified

	def asFile(): M[FileM[M]] = FileM.checked(file)
	def asDir(): M[DirM[M]] = DirM.checked(file)

	def isLink(): M[Boolean] = attempt(file.isSymbolicLink)
	def isHidden(): M[Boolean] = attempt(file.isHidden)

	/** Copy the file to the specified directory */
	def copyTo(dir: DirM[M]): M[S] = attempt(newInstance(file.copyToDirectory(dir.file)))
	/** Move the file to the specified directory */
	def moveTo(dir: DirM[M]): M[S] = attempt(newInstance(file.moveToDirectory(dir.file)))
	/** Delete the file */
	// TODO does this actually delete recursively for a dir?
	def delete(): M[S] = attempt {file.delete(); instance}
	/** Same as [[delete()]] but returns a [[Unit]] */
	// TODO does this actually delete recursively for a dir?
	def deleteUnit(): M[Unit] = attempt {file.delete(); ()}
	/** Rename the file */
	def rename(name: String): M[S] = attempt(newInstance(file.renameTo(name)))


	def watch(watchService: WatchService, events: Events = Events.all): M[S] =
		attempt {file.register(watchService, events); instance}

	def name: Option[String] = file.nameOption
	def nameOrEmpty: String = file.name

	def parent(): Option[DirM[M]] = file.parentOption.map {new DirM[M](_)}
	// XXX not ideal
	def siblings(): M[Iterator[M[PathM[M]]]] = attempt(file.siblings.map {PathM.checked(_)})


	def create(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
										  lops: LinkOps = LinkOptions.default): M[S]
	def createIfNotExists(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
													 lops: LinkOps = LinkOptions.default): M[S]


	def /!(fileName: String): FileM[M] = new FileM[M](file / fileName)
	def /(dirName: String): DirM[M] = new DirM[M](file / dirName)

	/** Check whether the path is a directory */
	@inline def isDir: Boolean
	/** Check whether the path is a file */
	@inline def isFile: Boolean = !isDir
	/** Applies `ff` or `fd` depending on whether this is a file or directory */
	@inline def fold[A](ff: FileM[M] => A, fd: DirM[M] => A): A = this match {
		case x: FileM[M] => ff(x)
		case x: DirM[M]  => fd(x)
	}

}

/** Represents an actual file(not directory); contains operations specific to a file
  *
  * Effects are documented using the monads, see [[PathM]] for more details.
  *
  * @param F the [[ MonadError[F[_], A] ]] instance
  * @tparam M the monad type
  */
final case class FileM[M[_]] private[bmf](file: File)(implicit F: MonadThrowable[M]) extends PathM[M] {

	type S = FileM[M]
	private[bmf] override val newInstance: File => FileM[M] = new FileM[M](_)
	private[bmf] override val instance   : FileM[M]         = this

	@inline override def isDir: Boolean = false
	override def create(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
												   lops: LinkOps = LinkOptions.default): M[FileM[M]] =
		attempt(newInstance(file.createIfNotExists(asDirectory = false, createParents = mkParent)))

	override def createIfNotExists(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
															  lops: LinkOps = LinkOptions.default): M[FileM[M]] =
		attempt(newInstance(file.createIfNotExists(asDirectory = false, createParents = mkParent)))

	def isLocked(mode: RandomAccessMode,
				 position: Long = 0L,
				 size: Long = Long.MaxValue,
				 isShared: Boolean = false)(implicit linkOps: LinkOps = LinkOptions.default): M[Boolean] =
		attempt(file.isLocked(mode, position, size, isShared)(linkOps))

	def size(implicit vops: VisitOps = VisitOptions.default): M[Long] = attempt(file.size(vops))

	def as[A]()(implicit ev: Array[Byte] => A): M[A] = attempt(ev.apply(file.byteArray))

	def asString()(implicit charset: Charset = DefaultCharset): M[String] =
		attempt(file.contentAsString(charset))

	def asBytes(): M[Array[Byte]] = attempt(file.loadBytes)

	private def resourceFromCloseable[A <: Closeable](a: => A): Resource[M, A] =
		Resource.make(attempt(a)) { x => attempt(x.close()) }

	def bufferedReader(implicit charset: Charset = DefaultCharset): Resource[M, BufferedReader] =
		resourceFromCloseable(file.newBufferedReader(charset))
	def bufferedWriter(implicit charset: Charset = DefaultCharset,
					   openOptions: OpenOptions = OpenOptions.default): Resource[M, BufferedWriter] =
		resourceFromCloseable(file.newBufferedWriter(charset, openOptions))

	def inputStream(implicit openOptions: OpenOptions = OpenOptions.default): Resource[M, InputStream] =
		resourceFromCloseable(file.newInputStream(openOptions))
	def outputStream(implicit openOptions: OpenOptions = OpenOptions.default): Resource[M, OutputStream] =
		resourceFromCloseable(file.newOutputStream(openOptions))

	def fileReader(): Resource[M, FileReader] =
		resourceFromCloseable(file.newFileReader)


	def append(s: String): M[FileM[M]] = attempt(newInstance(file.append(s)))

	def overwrite(s: String): M[FileM[M]] = attempt(newInstance(file.overwrite(s)))

	def hash(digest: Digest): M[String] = attempt(file.checksum(digest.backing))

	def contentType(): M[Option[String]] = attempt(file.contentType)

	def hasExtension: Boolean = file.name.contains(".")

	def extension(includeDot: Boolean = true, includeAll: Boolean = false, toLowerCase: Boolean = true): Option[String] = {
		// XXX can't delegate as the actual method touches the FS
		val dot = if (includeAll) file.name.indexOf(".") else file.name.lastIndexOf(".")
		if (dot == -1) None
		else {
			val extension = file.name.substring(if (includeDot) dot else dot + 1)
			Some(if (toLowerCase) extension.toLowerCase else extension)
		}
	}
}
object FileM {

	def unchecked[M[_]](file: File)(implicit F: MonadThrowable[M]): FileM[M] = new FileM[M](file)
	def checked[M[_]](file: File)(implicit F: MonadThrowable[M]): M[FileM[M]] = new FileM[M](file).checked

	def checked[M[_]](file: Path)(implicit F: MonadThrowable[M]): M[FileM[M]] = checked(File(file))
	def checked[M[_]](file: String)(implicit F: MonadThrowable[M]): M[FileM[M]] = checked(File(file))
	def checked[M[_]](file: java.io.File)(implicit F: MonadThrowable[M]): M[FileM[M]] = checked(file.toScala)

	def apply[M[_]](file: File)(implicit F: MonadThrowable[M]): M[FileM[M]] = checked(file)

	def newTempFile[M[_]](prefix: String = "",
						  suffix: String = "",
						  parent: Option[DirM[M]] = None)(implicit F: MonadThrowable[M]): M[FileM[M]] =
		F.catchNonFatal(unchecked(newTemporaryFile(prefix, suffix, parent.map {_.file})))

	def tempFile[M[_]](prefix: String = "",
					   suffix: String = "",
					   parent: Option[DirM[M]] = None)(implicit F: MonadThrowable[M]): Resource[M, FileM[M]] =
		Resource.make(newTempFile(prefix, suffix, parent))(_.deleteUnit())

	/** Wraps a backing [[java.security.MessageDigest]] */
	case class Digest(backing: MessageDigest) extends AnyVal
	/** Contains several commonly used digests that are guaranteed to be implemented in the JVM */
	object Digest {
		def apply(digest: String): Digest = Digest(MessageDigest.getInstance(digest))
		final val MD5   : Digest = Digest("MD5")
		final val SHA1  : Digest = Digest("SHA-1")
		final val SHA256: Digest = Digest("SHA-256")
		final val SHA512: Digest = Digest("SHA-512")
		// TODO CRC32?
	}

}

/** Represents an actual directory; contains operations specific to a directory
  *
  * Effects are documented using the monads, see [[PathM]] for more details.
  *
  * @param F the [[ MonadError[F[_], A] ]] instance
  * @tparam M the monad type
  */
final case class DirM[M[_]] private[bmf](file: File)(implicit F: MonadThrowable[M]) extends PathM[M] {

	type S = DirM[M]

	private[bmf] override val newInstance: File => DirM[M] = new DirM[M](_)
	private[bmf] override val instance   : DirM[M]         = this

	@inline override def isDir: Boolean = true

	override def create(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
												   lops: LinkOps = LinkOptions.default): M[DirM[M]] =
		attempt(newInstance(file.createIfNotExists(asDirectory = true, createParents = mkParent)))

	override def createIfNotExists(mkParent: Boolean = false)(implicit attrs: Attrs = Attributes.default,
															  lops: LinkOps = LinkOptions.default): M[DirM[M]] =
		attempt(newInstance(file.createIfNotExists(asDirectory = true, createParents = mkParent)))

	def count(recursive: Boolean = false)(implicit vops: VisitOps = VisitOptions.default): M[Int] =
		attempt((if (recursive) file.listRecursively else file.list).length)

	def list(recursive: Boolean = false): M[Iterator[M[PathM[M]]]] =
		attempt((if (recursive) file.listRecursively else file.list).map {PathM.checked(_)})

	def contains(that: PathM[M]): Boolean = that.file.path.startsWith(file.path) // XXX can't delegate as the actual method touches the FS


}
object DirM {

	def unchecked[M[_]](file: File)(implicit F: MonadThrowable[M]): DirM[M] = new DirM[M](file)
	def checked[M[_]](file: File)(implicit F: MonadThrowable[M]): M[DirM[M]] = new DirM[M](file).checked

	def checked[M[_]](file: Path)(implicit F: MonadThrowable[M]): M[DirM[M]] = checked(File(file))
	def checked[M[_]](file: String)(implicit F: MonadThrowable[M]): M[DirM[M]] = checked(File(file))
	def checked[M[_]](file: java.io.File)(implicit F: MonadThrowable[M]): M[DirM[M]] = checked(file.toScala)

	def apply[M[_]](file: File)(implicit F: MonadThrowable[M]): M[DirM[M]] = checked(file)


	def home[M[_]](implicit F: MonadThrowable[M]): DirM[M] = unchecked(File.home)
	def pwd[M[_]](implicit F: MonadThrowable[M]): DirM[M] = unchecked(File.currentWorkingDirectory)


	def newTempDir[M[_]](prefix: String = "",
						 parent: Option[DirM[M]] = None,
						 attrs: Attrs = Attributes.default)
						(implicit F: MonadThrowable[M]): M[DirM[M]] =
		F.catchNonFatal(unchecked(newTemporaryDirectory(prefix, parent.map {_.file})(attrs)))

	def tempDir[M[_]](prefix: String = "",
					  parent: Option[DirM[M]] = None,
					  attrs: Attrs = Attributes.default)
					 (implicit F: MonadThrowable[M]): Resource[M, DirM[M]] =
		Resource.make(newTempDir(prefix, parent, attrs))(_.deleteUnit())

}


object PathM {

	type VisitOps = VisitOptions
	type Attrs = Attributes
	type LinkOps = LinkOptions

	type MonadThrowable[M[_]] = MonadError[M, Throwable]


	def checked[M[_]](file: File)(implicit F: MonadThrowable[M]): M[PathM[M]] =
		F.catchNonFatal {if (file.isDirectory) new DirM[M](file) else new FileM[M](file)}
	def checked[M[_]](path: Path)(implicit F: MonadThrowable[M]): M[PathM[M]] = checked(File(path))
	def checked[M[_]](path: String)(implicit F: MonadThrowable[M]): M[PathM[M]] = checked(File(path))
	def checked[M[_]](file: java.io.File)(implicit F: MonadThrowable[M]): M[PathM[M]] = checked(file.toScala)


	def newWatchService[M[_]]()(implicit F: MonadThrowable[M]): M[WatchService] =
		F.catchNonFatal(FileSystems.getDefault.newWatchService())

	implicit class LiftFile(file: File) {

		def liftFile[M[_]](implicit F: MonadThrowable[M]): FileM[M] = FileM.unchecked(file)
		def liftDir[M[_]](implicit F: MonadThrowable[M]): DirM[M] = DirM.unchecked(file)

	}

}