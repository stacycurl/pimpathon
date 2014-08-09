package pimpathon

import java.io.File
import org.junit.Test

import org.junit.Assert._
import pimpathon.file._


class FileTest {
  @Test def create {
    val tmpFile = file.withTempFile(f => f)
    assertFalse(tmpFile.exists)

    tmpFile.create()
    assertTrue(tmpFile.exists)

    tmpFile.delete()
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
    assertFalse("Temp directory should not exist after 'withTempDirectory'",
      file.withTempDirectory(tmp => {
        assertIsTemp(".tmp", "temp", expectedIsFile = false, tmp); tmp
      }).exists
    )

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

  private def assertIsTemp(expectedSuffix: String, expectedPrefix: String, expectedIsFile: Boolean,
    tmp: File) {

    assertTrue(s"Expected ${tmp.getName} to exist !", tmp.exists)

    assertTrue(s"Expected ${tmp.getName}, to begin with $expectedPrefix",
      tmp.getName.startsWith(expectedPrefix))

    assertTrue(s"Expected ${tmp.getName}, to end with $expectedSuffix",
      tmp.getName.endsWith(expectedSuffix))

    assertEquals(s"Expected ${tmp.getName} to be a " + (if (expectedIsFile) "file" else "directory"),
      expectedIsFile, tmp.isFile)
  }
}
