package pimpathon

import _root_.java.io.File
import org.junit.Test
import pimpathon.function.Predicate
import scala.io.Codec
import scala.util.Properties

import org.junit.Assert._
import pimpathon.any._
import pimpathon.java.io.inputStream._
import pimpathon.util._
import scala.collection.JavaConversions._


class FileTest {
  private val file = new FileUtils(currentTime = util.currentTime)

  import file._

  @Test def rejectsNull(): Unit =
    assertThrows[Exception]("requirement failed: FileOps cannot be used with null files")(file.FilePimps(null: File))

  @Test def create(): Unit = file.withTempDirectory(dir ⇒ {
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

  @Test def deleteRecursively(): Unit = file.withTempDirectory(tmp ⇒ {
    assertFalse((tmp / "non-existent-file").deleteRecursively().exists)
    assertFalse((tmp / "existing-file").create().deleteRecursively().exists)
    assertFalse((tmp / "existing-dir").create(directory = true).deleteRecursively().exists)

    val parent = (tmp / "parent").create(directory = true)
    val children = List(parent / "child", parent / "parent" / "child").map(_.create())

    assertFalse(parent.deleteRecursively().exists)
    assertEquals(Nil, children.filter(_.exists))
  })

  @Test def deleteRecursivelyOnExit(): Unit = {
    // Check that 'deleteRecursivelyOnExit' registers a DeleteRecursively shutdown hook
    file.withTempFile(tmp ⇒ {
      tmp.deleteRecursivelyOnExit()
      assertTrue(shutdownHooks().contains(DeleteRecursively(tmp)))
      assertFalse(shutdownHooks().contains(DeleteRecursively(file.tempFile())))
    })

    // Check that running DeleteRecursively works
    file.withTempDirectory(tmp ⇒ {
      val parent = (tmp / "parent").create(directory = true)
      val files = parent :: List((parent / "child").create(), (parent / "parent" / "child").create())
      val deleteRecursively = DeleteRecursively(parent)
      assertEquals("files should exist before Thread has been run", files, files.filter(_.exists))

      deleteRecursively.run()
      assertEquals("files should not exist after Thread has been run", Nil, files.filter(_.exists))
    })
  }

  @Test def cwd(): Unit = assertEquals(Properties.userDir, file.cwd.getPath)

  @Test def named(): Unit = file.withTempFile(tmp ⇒ assertEquals("name", tmp.named("name").toString))

  @Test def changeToDirectory(): Unit = file.withTempFile(file ⇒ assertTrue(file.changeToDirectory().isDirectory))

  @Test def children(): Unit = file.withTempDirectory(dir ⇒ {
    assertEquals(Set.empty[File], dir.children.toSet)

    val List(child, toddler) = file.files(dir, "child", "toddler").map(_.create()).toList
    assertEquals(Set(child, toddler), dir.children.map(_.named()).toSet)
  })

  @Test def childDirs(): Unit = file.withTempDirectory(dir ⇒ {
    assertEquals(Set.empty[File], dir.childDirs.toSet)

    val List(child, toddler) = file.files(dir, "child", "parent/toddler").map(_.create()).toList
    assertEquals(Set(toddler.getParentFile), dir.childDirs.map(_.named()).toSet)
  })

  @Test def ancestors(): Unit = file.withTempDirectory(dir ⇒ {
    val List(child) = file.files(dir, "parent/child").map(_.create()).toList

    assertTrue(Set(dir, dir / "parent", child).forall(child.ancestors.contains))
  })

  @Test def isAncestorOf(): Unit = file.withTempDirectory(dir ⇒ {
    val List(child) = file.files(dir, "parent/child").map(_.create()).toList

    assertTrue(Set(dir, dir / "parent", child).forall(_.isAncestorOf(child)))
  })

  @Test def relativeTo(): Unit = file.withTempDirectory(dir ⇒ {
    assertEquals("child", (dir / "child").create().relativeTo(dir).getPath)

    assertEquals(dir.getName + "/kid",
      (dir / "kid").create().relativeTo(dir.getParentFile).getPath)

    val parent = (dir / "parent").create(directory = true)
    assertEquals("parent",       parent.relativeTo(dir).getPath)
    assertEquals("..",           dir.relativeTo(parent).getPath)
    assertEquals("parent/child", (parent / "child").create().relativeTo(dir).getPath)
  })

  @Test def tree(): Unit = {
    assertEquals(Nil, new File("non-existent").tree)

    file.withTempFile(tmp ⇒      assertEquals(List(tmp), tmp.tree))
    file.withTempDirectory(tmp ⇒ assertEquals(List(tmp), tmp.tree))

    file.withTempDirectory(tmp ⇒ {
      val List(child, toddler, brat, unreadableFile, unreadableChild) = file.files(tmp,
        "child", "toddler", "parent/brat", "unreadableFile", "unreadableDir/child").map(_.create()).toList

      val unreadableDir = unreadableChild.getParentFile.tap(_.setReadable(false))

      assertEqualsSet(Set(tmp.named(), child, toddler, brat.getParentFile, brat, unreadableFile, unreadableDir),
        tmp.tree.map(_.named()).toSet)
    })
  }

  @Test def withTempFile(): Unit = {
    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile(tmp ⇒ {
        assertIsTemp(".tmp", "temp", expectedIsFile = true, tmp); tmp
      }).exists
    )

    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile("suffix")(tmp ⇒ {
        assertIsTemp("suffix", "temp", expectedIsFile = true, tmp); tmp
      }).exists
    )

    assertFalse("Temp file should not exist after 'withTempFile'",
      file.withTempFile("suffix", "prefix")(tmp ⇒ {
        assertIsTemp("suffix", "prefix", expectedIsFile = true, tmp); tmp
      }).exists
    )
  }

  @Test def withTempDirectory(): Unit = {
    val files = file.withTempDirectory(tmp ⇒ {
      def relativeName(file: File): File = file.named("tmp/" + file.relativeTo(tmp).getPath)

      assertIsTemp(".tmp", "temp", expectedIsFile = false, tmp)

      List(tmp, (tmp / "child").create(), (tmp / "parent" / "child").create()).map(relativeName)
    })

    assertEquals("Temp directory (and contents) should not exist after 'withTempDirectory'",
      Nil, files.filter(_.exists()))

    assertFalse("Temp directory should not exist after 'withTempDirectory'",
      file.withTempDirectory("suffix")(tmp ⇒ {
        assertIsTemp("suffix", "temp", expectedIsFile = false, tmp); tmp
      }).exists
    )

    assertFalse("Temp directory should not exist after 'withTempDirectory'",
      file.withTempDirectory("suffix", "prefix")(tmp ⇒ {
        assertIsTemp("suffix", "prefix", expectedIsFile = false, tmp); tmp
      }).exists
    )
  }

  @Test def tempFile(): Unit = {
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

  @Test def tempDir(): Unit = {
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


  @Test def newFile(): Unit = {
    val dir = file.file("this directory does not exist")
    assertFalse(dir.exists)

    val nested = file.file("parent", "file")
    assertEquals(file.file("parent"), nested.getParentFile)

    file.withTempDirectory(dir ⇒ {
      val child = dir / "and this file does not exist"
      assertFalse(child.exists)
      assertEquals(dir, child.getParentFile)
      assertEquals(child, file.file(dir, "and this file does not exist"))

      val nested = dir / "parent/child"
      assertFalse(nested.exists)
      assertEquals(dir, nested.getParentFile.getParentFile)
    })
  }

  @Test def files(): Unit = file.withTempDirectory(dir ⇒ {
    val List(child, nested) = file.files(dir, "child", "nested/child").toList

    assertEquals(dir / "child",            child)
    assertEquals(dir / "nested" / "child", nested)
  })

  @Test def readBytes(): Unit = file.withTempFile(tmp ⇒ {
    createInputStream("contents").drain(tmp.outputStream())
    assertEquals("contents", new String(tmp.readBytes()))
  })

  @Test def readLines(): Unit = file.withTempFile(tmp ⇒ {
    createInputStream("line1\nline2").drain(tmp.outputStream())
    assertEquals(List("line1", "line2"), tmp.readLines())
  })

  @Test def readString(): Unit = file.withTempFile(tmp ⇒ {
    List("ISO-8859-1", "US-ASCII", "UTF-16", "UTF-16BE", "UTF-16LE", "UTF-8").map(Codec(_)).foreach(codec ⇒ {
      assertEquals("line1\r\nline2", tmp.writeBytes("line1\r\nline2".getBytes(codec.charSet)).readString()(codec))
    })
  })

  @Test def write(): Unit = file.withTempFile(tmp ⇒ {
    assertEquals(List("content"),      tmp.write("content", append = false).readLines())
    assertEquals(List("contents"),     tmp.write("s", append = true).readLines())
    assertEquals(List("new content"),  tmp.write("new content").readLines())
    assertEquals(List("new contents"), tmp.write("s", append = true).readLines())
  })

  @Test def writeBytes(): Unit = file.withTempFile(tmp ⇒ {
    assertEquals(List("12"),   tmp.writeBytes("12".getBytes).readLines())
    assertEquals(List("1234"), tmp.writeBytes("34".getBytes, append = true).readLines())
    assertEquals(List("56"),   tmp.writeBytes("56".getBytes).readLines())
  })

  @Test def writeLines(): Unit = file.withTempFile(tmp ⇒ {
    assertEquals(List("1", "2"),           tmp.writeLines(List("1", "2")).readLines())
    assertEquals(List("1", "2", "3", "4"), tmp.writeLines(List("3", "4"), append = true).readLines())
    assertEquals(List("5", "6"),           tmp.writeLines(List("5", "6")).readLines())
  })

  @Test def md5(): Unit = file.withTempFile(tmp ⇒ {
    assertEquals("6f1ed002ab5595859014ebf0951522d9", tmp.writeLines(List("blah")).md5())
  })

  @Test def missing(): Unit = assertTrue(file.withTempFile(tmp ⇒ {
    assertFalse(tmp.missing); tmp
  }).missing)

  @Test def hasExtension(): Unit = assertFileNameProperty(_.hasExtension("txt"), "a.txt",   "b.tmp")
  @Test def isScala(): Unit      = assertFileNameProperty(_.isScala,             "a.scala", "b.java")
  @Test def isJava(): Unit       = assertFileNameProperty(_.isJava,              "a.java",  "b.scala")
  @Test def isClass(): Unit      = assertFileNameProperty(_.isClass,             "a.class", "b.txt")
  @Test def isJar(): Unit        = assertFileNameProperty(_.isJar,               "a.jar",   "b.zip")

  @Test def isParentOf(): Unit = file.withTempDirectory(dir ⇒ {
    assertTrue(dir.isParentOf(dir / "child"))
    assertFalse((dir / "child").isParentOf(dir))
    assertFalse(dir.isParentOf(dir / "child" / "kid"))
  })

  @Test def isChildOf(): Unit = file.withTempDirectory(dir ⇒ {
    assertTrue((dir / "child").isChildOf(dir))
    assertFalse(dir.isChildOf(dir / "child"))
    assertFalse((dir / "child" / "kid").isChildOf(dir))
  })

  @Test def contains(): Unit = file.withTempDirectory(dir ⇒ {
    assertTrue(dir.contains(dir / "child"))
    assertTrue(dir.contains(dir / "parent" / "kid"))
    assertFalse((dir / "child").contains(dir))
  })

  @Test def isContainedIn(): Unit = file.withTempDirectory(dir ⇒ {
    assertTrue((dir / "child").isContainedIn(dir))
    assertTrue((dir / "parent" / "kid").isContainedIn(dir))
    assertFalse(dir.isContainedIn(dir / "child"))
  })

  @Test def className(): Unit = file.withTempDirectory(dir ⇒ {
    assertEquals("Foo", (dir / "Foo.class").className(dir))
    assertEquals("com.example.Foo", (dir / "com" / "example" / "Foo.class").className(dir))
  })

  @Test def touch(): Unit = file.withTempDirectory(dir ⇒ {
    withTime(123000) {
      assertEquals("Should be able to touch non-existent file", 123000, (dir / "child").touch().lastModified)
      assertEquals(0, (dir / "child").readBytes().length)
    }

    (dir / "child").writeLines(List("Don't touch this"))

    withTime(456000) {
      assertEquals("Should be able to touch existing file", 456000, (dir / "child").touch().lastModified)
      assertEquals("Don't touch this".length + 1, (dir / "child").readBytes().length)
    }

    withTime(789000) {
      assertEquals(789000, dir.touch().lastModified)
    }
  })

  private def assertFileNameProperty(p: Predicate[File], success: String, failure: String): Unit = {
    file.withTempDirectory(dir ⇒ {
      assertTrue(p(dir / success))
      assertFalse(p(dir / failure))
    })
  }

  private def assertIsTemp(
    expectedSuffix: String, expectedPrefix: String, expectedIsFile: Boolean, tmp: File): Unit = {

    assertTrue(s"Expected ${tmp.getName} to exist !", tmp.exists)

    assertTrue(s"Expected ${tmp.getName}, to begin with $expectedPrefix",
      tmp.getName.startsWith(expectedPrefix))

    assertTrue(s"Expected ${tmp.getName}, to end with $expectedSuffix",
      tmp.getName.endsWith(expectedSuffix))

    assertEquals(s"Expected ${tmp.getName} to be a " + (if (expectedIsFile) "file" else "directory"),
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