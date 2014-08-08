package pimpathon

import java.io.File
import org.junit.Test

import org.junit.Assert._
import pimpathon.file._


class FileTest {
  @Test def withTemp {
    val temp = file.withTemp(tmp => {
      assertTrue("Temp file should exist during 'withTemp'", tmp.exists)
      assertTrue(tmp.getName.startsWith(file.tempPrefix))
      assertTrue(tmp.getName.endsWith(file.tempSuffix))
      assertTrue(tmp.isFile)

      tmp
    })

    assertFalse("Temp file should not exist after 'withTemp'", temp.exists)
  }

  @Test def withTempDirectory {
    val temp = file.withTempDirectory(tmp => {
      assertTrue("Temp directory should exist during 'withTemp'", tmp.exists)
      assertTrue(tmp.getName.startsWith(file.tempPrefix))
      assertTrue(tmp.getName.endsWith(file.tempSuffix))
      assertTrue(tmp.isDirectory)

      tmp
    })

    assertFalse("Temp directory should not exist after 'withTemp'", temp.exists)
  }
}
