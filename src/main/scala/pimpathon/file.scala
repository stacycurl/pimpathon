package pimpathon

import _root_.java.io.{RandomAccessFile, File, FileOutputStream}
import scala.io.{Codec, BufferedSource, Source}
import scala.util.Properties

import pimpathon.any._
import pimpathon.java.io.outputStream._
import pimpathon.list._
import pimpathon.string._


object file extends FileUtils()

case class FileUtils (
  suffix: String = ".tmp", prefix: String = "temp", append: Boolean = false,
  private val currentTime: () ⇒ Long = () ⇒ System.currentTimeMillis()
) {

  implicit class FilePimps(private val self: File) {
    require(Option(self).isDefined, "FileOps cannot be used with null files")

    def missing: Boolean = !self.exists
    def isScala: Boolean = hasExtension("scala")
    def isJava: Boolean  = hasExtension("java")
    def isClass: Boolean = hasExtension("class")
    def isJar: Boolean   = hasExtension("jar")
    def hasExtension(extension: String): Boolean = self.getName.endsWith(extension)
    def isChildOf(other: File): Boolean     = other.isParentOf(self)
    def isParentOf(other: File): Boolean    = other.getParentFile.equals(self)
    def isContainedIn(other: File): Boolean = other.contains(self)
    def contains(other: File): Boolean      = isAncestorOf(other)
    def isAncestorOf(other: File): Boolean  = other.ancestors.contains(self)

    def /(name: String): File = new File(self, name)
    def named(name: String = self.getName): File = new NamedFile(self, name)
    def canon: File = self.attempt(_.getCanonicalFile).getOrElse(self.getAbsoluteFile)

    def relativeTo(dir: File): File = sharedPaths(dir) |> { case (relativeFile, relativeDir) ⇒
      new File((relativeDir.const("..") ++ relativeFile).mkString(File.separator))
    }

    def changeToDirectory(): File = self.tapIf(_.isFile)(_.delete(), _.mkdir())

    def create(directory: Boolean = false): File =
      self.tap(_.getParentFile.mkdirs(), f ⇒ if (directory) f.mkdir() else f.createNewFile())

    def deleteRecursively(): File       = self.tap(_.tree.reverse.foreach(_.delete()))
    def deleteRecursivelyOnExit(): File = self.tap(f ⇒ Runtime.getRuntime.addShutdownHook(DeleteRecursively(f)))

    def touch(): File = create().tap(_.setLastModified(currentTime()))

    def tree: Stream[File]      = stream.cond(self.exists, self #:: children.flatMap(_.tree))
    def children: Stream[File]  = stream.cond(self.isDirectory && self.canRead, self.listFiles.toStream)
    def childDirs: Stream[File] = children.filter(_.isDirectory)

    def ancestors: Stream[File] = Stream.iterate(self)(_.getParentFile).takeWhile(_ != null)

    def path: List[String]     = self.getAbsolutePath.split(separator).toList.filterNot(Set("", "."))

    def md5(): String = readLines().mkString("\n").md5

    def readString()(implicit codec: Codec): String = new String(readBytes(), codec.charSet)

    def readBytes(): Array[Byte] = new RandomAccessFile(self, "r").withFinally(_.close())(raf ⇒ {
      new Array[Byte](raf.length().toInt).tap(raf.read)
    })

    def readLines()(implicit codec: Codec): List[String] = source().withFinally(_.close())(_.getLines().toList)

    def write(contents: String, append: Boolean = append): File =
      writeString(contents, append)

    def writeString(contents: String, append: Boolean = append): File =
      writeBytes(contents.getBytes, append)

    def writeLines(lines: List[String], append: Boolean = append): File =
      writeBytes((lines.mkString("\n") + "\n").getBytes, append)

    def writeBytes(bytes: Array[Byte], append: Boolean = append): File =
      self.tap(_.outputStream(append).closeAfter(_.write(bytes)))

    def outputStream(append: Boolean = append): FileOutputStream = new FileOutputStream(self, append)
    def source()(implicit codec: Codec): BufferedSource =  Source.fromFile(self)

    def className(classDir: File): String = sharedPaths(classDir)._1.mkString(".").stripSuffix(".class")

    private def separator: String = File.separator.replace("\\", "\\\\")
    private def sharedPaths(other: File) = self.path.sharedPrefix(other.path) |> (t ⇒ (t._2, t._3))
  }

  def cwd: File = file(Properties.userDir)
  def file(name: String): File = new File(name)
  def file(parent: String, name: String): File = new File(parent, name)
  def file(parent: File, name: String): File = new File(parent, name)
  def files(parent: File, names: String*): Stream[File] = names.toStream.map(parent / _)

  def resource(name: String): Option[File] =
    Option(Thread.currentThread().getContextClassLoader.getResource(name)).map(url ⇒ file(url.getPath))

  def tempFile(suffix: String = suffix, prefix: String = prefix): File =
    File.createTempFile(prefix, suffix).tap(_.deleteOnExit())

  def tempDir(suffix: String = suffix, prefix: String = prefix): File =
    File.createTempFile(prefix, suffix).changeToDirectory().tap(_.deleteRecursivelyOnExit())

  def withTempDirectory[A](f: File ⇒ A): A = withTempDirectory(suffix)(f)

  def withTempDirectory[A](suffix: String, prefix: String = prefix)(f: File ⇒ A): A =
    withTempFile[A](suffix, prefix)(tmp ⇒ f(tmp.changeToDirectory()))

  def withTempFile[A](f: File ⇒ A): A = withTempFile(suffix)(f)

  def withTempFile[A](suffix: String, prefix: String = prefix)(f: File ⇒ A): A =
    File.createTempFile(prefix, suffix).calc(file ⇒ try f(file) finally file.deleteRecursively())


  class NamedFile(file: File, name: String) extends File(file.getPath) {
    override def toString: String = name
  }

  case class DeleteRecursively(file: File) extends Thread {
    override def run(): Unit = file.deleteRecursively()
  }
}