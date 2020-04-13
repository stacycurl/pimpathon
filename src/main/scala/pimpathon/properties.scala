package pimpathon

import scala.util.Properties


object properties {
  implicit class PropertiesCompanionPimps(val self: Properties.type) extends AnyVal {
    def propEnvOrElse(key: String, alt: String): String = propEnvOrNone(key).getOrElse(alt)
    def propEnvOrNone(key: String): Option[String] = Properties.propOrNone(key).orElse(Properties.envOrNone(key))
  }
}
