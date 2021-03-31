package pimpathon.java.util

import java.util.{Calendar, Date}
import pimpathon.PSpec
import pimpathon.any._
import pimpathon.java.util.date._


class DateSpec extends PSpec {
  "addDay" in
    on(-1, 1, 7).calling(date(2015, 3, 24).addDay).produces(date(2015, 3, 23), date(2015, 3, 25), date(2015, 4, 1))

  private def date(year: Int, month: Int, day: Int): Date =
    Calendar.getInstance().tap(_.set(year, month, day, 0, 0, 0), _.set(Calendar.MILLISECOND, 0)).getTime
}