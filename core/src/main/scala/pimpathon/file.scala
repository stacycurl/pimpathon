package pimpathon

import _root_.java.io.{File, FileOutputStream}
import scala.io.{BufferedSource, Source}
import scala.util.Properties

import pimpathon.any._
import pimpathon.java.io.outputStream._
import pimpathon.list._
import pimpathon.string._


object file extends FileUtils(".tmp", "temp")

case class FileUtils(suffix: String, prefix: String) {
  implicit class FileOps(file: File) {
    require(Option(file).isDefined, "FileOps cannot be used with null files")

    def missing: Boolean = !file.exists
    def hasExtension(extension: String): Boolean = file.getName.endsWith(extension)
    def isScala: Boolean = hasExtension("scala")
    def isJava: Boolean  = hasExtension("java")
    def isClass: Boolean = hasExtension("class")
    def isJar: Boolean   = hasExtension("jar")
    def isParentOf(other: File): Boolean    = other.getParentFile.equals(file)
    def isChildOf(other: File): Boolean     = other.isParentOf(file)
    def contains(other: File): Boolean      = other.ancestors.exists(_.equals(file))
    def isContainedIn(other: File): Boolean = other.contains(file)

    // http://rapture.io does this much better
    def /(name: String): File = new File(file, name)
    def named(name: String = file.getName): File = new NamedFile(file, name)

    def relativeTo(dir: File): File = {
      val (_, relativeFile, relativeDir) = file.path.sharedPrefix(dir.path)

      new File((relativeDir.const("..") ++ relativeFile).mkString(File.separator))
    }

    def tree: Stream[File]      = stream.cond(file.exists, file #:: children.flatMap(_.tree))
    def children: Stream[File]  = stream.cond(file.isDirectory && file.canRead, file.listFiles.toStream)
    def childDirs: Stream[File] = children.filter(_.isDirectory)

    def path: List[String]     = file.getAbsolutePath.split(separator).toList.filterNot(Set("", "."))

    def changeToDirectory(): File = file.tapIf(_.isFile)(_.delete(), _.mkdir())

    def create(directory: Boolean = false): File =
      file.tap(_.getParentFile.mkdirs(), f => if (directory) f.mkdir() else f.createNewFile())

    def deleteRecursively(): File       = file.tap(_.tree.reverse.foreach(_.delete()))
    def deleteRecursivelyOnExit(): File = file.tap(f => Runtime.getRuntime.addShutdownHook(DeleteRecursively(f)))

    def readBytes(): Array[Byte] = source().withFinally(_.close())(_.map(_.toByte).toArray)
    def readLines(): List[String] = source().withFinally(_.close())(_.getLines.toList)

    def writeBytes(bytes: Array[Byte], append: Boolean = true): File =
      file.tap(_.outputStream(append).closeAfter(_.write(bytes)))

    def writeLines(lines: List[String], append: Boolean = true): File =
      writeBytes(lines.mkString("\n").getBytes, append)

    def outputStream: FileOutputStream = outputStream(false)
    def outputStream(append: Boolean = true): FileOutputStream = new FileOutputStream(file, append)
    def source(): BufferedSource =  Source.fromFile(file)

    def md5(): String = readLines().mkString("\n").md5

    private[pimpathon] def ancestors: Stream[File] = Stream.iterate(file)(_.getParentFile).takeWhile(_ != null)
    private def separator: String = File.separator.replace("\\", "\\\\")
  }

  def cwd: File = file(Properties.userDir)
  def file(name: String): File = new File(name)
  def files(parent: File, names: String*): Stream[File] = names.toStream.map(parent / _)

  // @deprecated(message = "Use file / name", since = "16 Aug 2014")
  def file(parent: File, name: String): File = new File(parent, name)

  def tempFile(suffix: String = suffix, prefix: String = prefix): File =
    File.createTempFile(prefix, suffix).tap(_.deleteOnExit())

  def tempDir(suffix: String = suffix, prefix: String = prefix): File =
    File.createTempFile(prefix, suffix).changeToDirectory().tap(_.deleteRecursivelyOnExit())

  def withTempFile[A](f: File => A): A = withTempFile(suffix)(f)

  def withTempFile[A](suffix: String, prefix: String = prefix)(f: File => A): A = {
    val file = File.createTempFile(prefix, suffix)

    try f(file) finally file.deleteRecursively()
  }


  def withTempDirectory[A](f: File => A): A = withTempDirectory(suffix)(f)

  def withTempDirectory[A](suffix: String, prefix: String = prefix)(f: File => A): A =
    withTempFile[A](suffix, prefix)(tmp => f(tmp.changeToDirectory()))


  class NamedFile(file: File, name: String) extends File(file.getPath) {
    override def toString: String = name
  }

  case class DeleteRecursively(file: File) extends Thread {
    override def run(): Unit = file.deleteRecursively()
  }
}
