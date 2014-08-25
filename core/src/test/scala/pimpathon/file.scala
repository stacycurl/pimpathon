package pimpathon

import _root_.java.io.{File, FileInputStream}
import _root_.java.lang.reflect.Field
import org.junit.Test
import scala.collection.JavaConversions._

import org.junit.Assert._
import pimpathon.any._
import pimpathon.file._
import pimpathon.java.io.inputStream._
import pimpathon.util._


class FileTest {
  @Test def create {
    file.withTempDirectory(dir => {
      val child = dir / "child"
      assertFalse(child.exists)

      child.create()
      assertTrue(child.exists)
      assertTrue(child.isFile)

      val childDir = dir / "childDir"
      assertFalse(childDir.exists)

      childDir.create(directory = true)
      assertTrue(childDir.exists)
      assertTrue(childDir.isDirectory)

      val nested = dir / "parent" / "child"
      assertFalse(nested.exists)

      nested.create()
      assertTrue(nested.exists)
      assertTrue(nested.isFile)
    })
  }

  @Test def deleteRecursively {
    file.withTempDirectory(tmp => {
      assertFalse((tmp / "non-existent-file").deleteRecursively().exists)
      assertFalse((tmp / "existing-file").create().deleteRecursively().exists)
      assertFalse((tmp / ("existing-dir")).create(directory = true).deleteRecursively().exists)

      val parent = (tmp / "parent").create(directory = true)
      val children = List(parent / "child", parent / "parent" / "child").map(_.create())

      assertFalse(parent.deleteRecursively().exists)
      assertEquals(Nil, children.filter(_.exists))
    })
  }

  @Test def deleteRecursivelyOnExit {
    // Check that 'deleteRecursivelyOnExit' registers a DeleteRecursively shutdown hook
    file.withTempFile(tmp => {
      tmp.deleteRecursivelyOnExit()
      assertTrue(shutdownHooks().contains(DeleteRecursively(tmp)))
      assertFalse(shutdownHooks().contains(DeleteRecursively(file.tempFile())))
    })

    // Check that running DeleteRecursively works
    file.withTempDirectory(tmp => {
      val parent = (tmp / "parent").create(directory = true)
      val files = parent :: List((parent / "child").create(), (parent / "parent" / "child").create())
      val deleteRecursively = DeleteRecursively(parent)
      assertEquals("files should exist before Thread has been run", files, files.filter(_.exists))

      deleteRecursively.run()
      assertEquals("files should not exist after Thread has been run", Nil, files.filter(_.exists))
    })
  }

  @Test def cwd {
    // not a great test, but what to do other that use an alternate implementation ?
    assertEquals("pimpathon", file.cwd.getName)
  }

  @Test def named {
    file.withTempFile(tmp => {
      assertEquals("name", tmp.named("name").toString)
    })
  }

  @Test def changeToDirectory {
    file.withTempFile(file => {
      assertTrue(file.changeToDirectory().isDirectory)
    })
  }

  @Test def children {
    file.withTempDirectory(dir => {
      assertEquals(Set.empty[File], dir.children.toSet)

      val List(child, toddler) = file.files(dir, "child", "toddler").map(_.create()).toList
      assertEquals(Set(child, toddler), dir.children.map(_.named()).toSet)
    })

    assertEquals(Stream.empty[File], (null: File).children)
  }

  @Test def relativeTo {
    file.withTempDirectory(dir => {
      assertEquals("child", (dir / "child").create().relativeTo(dir).getPath)

      assertEquals(dir.getName + "/kid",
        (dir / "kid").create().relativeTo(dir.getParentFile).getPath)

      val parent = (dir / "parent").create(directory = true)
      assertEquals("parent",       parent.relativeTo(dir).getPath)
      assertEquals("..",           dir.relativeTo(parent).getPath)
      assertEquals("parent/child", (parent / "child").create().relativeTo(dir).getPath)
    })
  }

  @Test def tree {
    assertEquals(Nil, new File("non-existent").tree)

    file.withTempFile(tmp =>      assertEquals(List(tmp), tmp.tree))
    file.withTempDirectory(tmp => assertEquals(List(tmp), tmp.tree))

    file.withTempDirectory(tmp => {
      val List(child, toddler, brat) =
        file.files(tmp, "child", "toddler", "parent/brat").map(_.create()).toList

      assertEquals(Set(tmp.named(), child, toddler, brat.getParentFile, brat),
        tmp.tree.map(_.named()).toSet)
    })
  }

  @Test def withTempFile {
    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile(tmp => {
        assertIsTemp(".tmp", "temp", expectedIsFile = true, tmp); tmp
      }).exists
    )

    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile("suffix")(tmp => {
        assertIsTemp("suffix", "temp", expectedIsFile = true, tmp); tmp
      }).exists
    )

    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile("suffix", "prefix")(tmp => {
        assertIsTemp("suffix", "prefix", expectedIsFile = true, tmp); tmp
      }).exists
    )
  }

  @Test def withTempDirectory {
    val files = file.withTempDirectory(tmp => {
      def relativeName(file: File): File = file.named("tmp/" + file.relativeTo(tmp).getPath)

      assertIsTemp(".tmp", "temp", expectedIsFile = false, tmp)

      List(tmp, (tmp / "child").create(), (tmp / "parent" / "child").create()).map(relativeName)
    })

    assertEquals("Temp directory (and contents) should not exist after 'withTempDirectory'",
      Nil, files.filter(_.exists()))

    assertFalse("Temp directory should not exist after 'withTempDirectory'",
      file.withTempDirectory("suffix")(tmp => {
        assertIsTemp("suffix", "temp", expectedIsFile = false, tmp); tmp
      }).exists
    )

    assertFalse("Temp directory should not exist after 'withTempDirectory'",
      file.withTempDirectory("suffix", "prefix")(tmp => {
        assertIsTemp("suffix", "prefix", expectedIsFile = false, tmp); tmp
      }).exists
    )
  }

  @Test def tempFile {
    val f = file.tempFile()
    assertTrue(f.isFile())
    assertTrue(f.exists())

    val prefix = "sufferin-"
    val suffix = ".sucotash"

    val f1 = file.tempFile(suffix)
    assertTrue(f1.isFile())
    assertTrue(f1.getName.endsWith(suffix))

    val f2 = file.tempFile(prefix = prefix)
    assertTrue(f2.isFile())
    assertTrue(f2.getName.startsWith(prefix))

    val f3 = file.tempFile(suffix, prefix)
    assertTrue(f3.isFile())
    assertTrue(f3.getName.startsWith(prefix))
    assertTrue(f3.getName.endsWith(suffix))
  }

  @Test def tempDir {
    val f = file.tempDir()
    assertTrue(f.isDirectory())
    assertTrue(f.exists())
    assertTrue(shutdownHooks().contains(DeleteRecursively(f)))

    val prefix = "gosh-"
    val suffix = ".darnit"

    val f1 = file.tempDir(suffix)
    assertTrue(f1.isDirectory())
    assertTrue(f1.getName.endsWith(suffix))
    assertTrue(shutdownHooks().contains(DeleteRecursively(f1)))

    val f2 = file.tempDir(prefix = prefix)
    assertTrue(f2.isDirectory())
    assertTrue(f2.getName.startsWith(prefix))
    assertTrue(shutdownHooks().contains(DeleteRecursively(f2)))

    val f3 = file.tempDir(suffix, prefix)
    assertTrue(f3.isDirectory())
    assertTrue(f3.getName.startsWith(prefix))
    assertTrue(f3.getName.endsWith(suffix))
    assertTrue(shutdownHooks().contains(DeleteRecursively(f3)))
  }


  @Test def newFile {
    val dir = file.file("this directory does not exist")
    assertFalse(dir.exists)

    file.withTempDirectory(dir => {
      val child = dir / "and this file does not exist"
      assertFalse(child.exists)
      assertEquals(dir, child.getParentFile)
      assertEquals(child, file.file(dir, "and this file does not exist"))

      val nested = dir / "parent/child"
      assertFalse(nested.exists)
      assertEquals(dir, nested.getParentFile.getParentFile)
    })
  }

  @Test def files {
    file.withTempDirectory(dir => {
      val List(child, nested) = file.files(dir, "child", "nested/child").toList

      assertEquals(dir / "child",            child)
      assertEquals(dir / "nested" / "child", nested)
    })
  }

  @Test def readBytes {
    file.withTempFile(tmp => {
      createInputStream("contents".getBytes).drain(tmp.outputStream())
      assertEquals("contents", new String(tmp.readBytes()))
    })
  }

  @Test def readLines {
    file.withTempFile(tmp => {
      createInputStream("line1\nline2".getBytes).drain(tmp.outputStream())
      assertEquals(List("line1", "line2"), tmp.readLines())
    })
  }

  @Test def writeBytes {
    file.withTempFile(tmp => {
      assertEquals(List("12"),   tmp.writeBytes("12".getBytes).readLines())
      assertEquals(List("1234"), tmp.writeBytes("34".getBytes).readLines())
      assertEquals(List("56"),   tmp.writeBytes("56".getBytes, append = false).readLines())
    })
  }

  @Test def writeLines {
    file.withTempFile(tmp => {
      assertEquals(List("1", "2"),       tmp.writeLines(List("1", "2")).readLines())
      assertEquals(List("1", "23", "4"), tmp.writeLines(List("3", "4")).readLines())
      assertEquals(List("5", "6"),       tmp.writeLines(List("5", "6"), append = false).readLines())
    })
  }

  @Test def md5 {
    file.withTempFile(tmp => {
      assertEquals("6f1ed002ab5595859014ebf0951522d9", tmp.writeLines(List("blah")).md5())
    })
  }

  private def assertIsTemp(
    expectedSuffix: String, expectedPrefix: String, expectedIsFile: Boolean, tmp: File) {

    assertTrue("Expected %s to exist !".format(tmp.getName), tmp.exists)

    assertTrue("Expected %s, to begin with %s".format(tmp.getName, expectedPrefix),
      tmp.getName.startsWith(expectedPrefix))

    assertTrue("Expected %s, to end with %s".format(tmp.getName, expectedSuffix),
      tmp.getName.endsWith(expectedSuffix))

    assertEquals("Expected %s to be a ".format(tmp.getName) + (if (expectedIsFile) "file" else "directory"),
      expectedIsFile, tmp.isFile)

    assertFalse("Expected ${tmp.getName} to not be deleted recursively on exit",
        shutdownHooks().contains(DeleteRecursively(tmp)))
  }

  private def shutdownHooks(): Set[Thread] = {
    asScalaSet(Class.forName("java.lang.ApplicationShutdownHooks")
      .getDeclaredField("hooks").tap(_.setAccessible(true))
      .get(null).asInstanceOf[_root_.java.util.Map[Thread, Thread]].keySet).toSet
  }
}
