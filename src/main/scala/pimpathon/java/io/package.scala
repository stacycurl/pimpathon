package pimpathon.java

import scala.language.implicitConversions

import java.io.{InputStream, OutputStream}


package object io {
  implicit def inputStreamPimps[IS <: InputStream](is: IS): InputStreamPimps[IS] =
    new InputStreamPimps[IS](is, inputStream)

  implicit def outputStreamPimps[OS <: OutputStream](os: OS): OutputStreamPimps[OS] =
    new OutputStreamPimps[OS](os, outputStream)
}