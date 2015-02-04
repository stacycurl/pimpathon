package pimpathon.java

import java.io.{InputStream, OutputStream}


package object io {
  implicit def inputStreamOps[IS <: InputStream](is: IS): InputStreamOps[IS] =
    new InputStreamOps[IS](is, inputStream)

  implicit def outputStreamOps[OS <: OutputStream](os: OS): OutputStreamOps[OS] =
    new OutputStreamOps[OS](os, outputStream)
}