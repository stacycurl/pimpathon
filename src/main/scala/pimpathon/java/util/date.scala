package pimpathon.java.util

import java.util.{Calendar, Date}

import pimpathon.any._


object date {
  implicit class DatePimps(val self: Date) extends AnyVal {
    def addDay(offset: Int): Date =
      Calendar.getInstance().tap(_.setTime(self), _.add(Calendar.DAY_OF_YEAR, offset)).getTime
  }
}