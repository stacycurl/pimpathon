package pimpathon

import _root_.java.io.File

import pimpathon.any._
import pimpathon.list._


object file extends FileUtils(".tmp", "temp")

case class FileUtils(suffix: String, prefix: String) {
  implicit class FileOps(file: File) {
    def named(name: String = file.getName): File = new NamedFile(file, name)

    def relativeTo(dir: File): File = {
      val (_, relativeFile, relativeDir) = file.path.sharedPrefix(dir.path)

      new File((relativeDir.const("..") ++ relativeFile).mkString(File.separator))
    }

    def tree: Stream[File]     = if (!file.exists) Stream.empty[File] else file #:: children.flatMap(_.tree)
    def children: Stream[File] = if (file.isFile) Stream.empty[File] else file.listFiles.toStream
    def path: List[String]     = file.getAbsolutePath.split(separator).toList.filterNot(Set("", "."))

    def changeToDirectory(): File = file.tapIf(_.isFile)(_.delete(), _.mkdir())
    def create(): File            = file.tap(_.createNewFile())

    private def separator: String = File.separator.replace("\\", "\\\\")
  }

  def file(name: String): File = new File(name)
  def file(parent: File, name: String): File = new File(parent, name)

  def tempFile(suffix: String = suffix, prefix: String = prefix): File =
    File.createTempFile(prefix, suffix).tap(_.deleteOnExit())

  def tempDir(suffix: String = suffix, prefix: String = prefix): File =
    tempFile(suffix, prefix).changeToDirectory().tap(_.deleteOnExit())

  def withTempFile[A](f: File => A): A = withTempFile(suffix)(f)

  def withTempFile[A](suffix: String, prefix: String = prefix)(f: File => A): A = {
    val file = tempFile(suffix, prefix)

    try f(file) finally file.delete
  }


  def withTempDirectory[A](f: File => A): A = withTempDirectory(suffix)(f)

  def withTempDirectory[A](suffix: String, prefix: String = prefix)(f: File => A): A =
    withTempFile[A](suffix, prefix)(tmp => f(tmp.changeToDirectory()))


  class NamedFile(file: File, name: String) extends File(file.getPath) {
    override def toString = name
  }
}
