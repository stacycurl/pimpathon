package pimpathon.java.util

import java.util.{Calendar, Date}

import pimpathon.any._


object date {
  implicit def datePimps(value: Date): DatePimps = new DatePimps(value)

  class DatePimps(value: Date) {
    def addDay(offset: Int): Date =
      Calendar.getInstance().tap(_.setTime(value), _.add(Calendar.DAY_OF_YEAR, offset)).getTime
  }
}