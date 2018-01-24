/*

package info.kwarc.mmt.api.utils.time

// temporary code for measuring time, should be redone without dependency on Java 8, possibly by using scala's Duration class

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime, Period, ZoneId}


case class Instant(jinst : java.time.Instant) {
  private[time] lazy val localdatetime = LocalDateTime.ofInstant(jinst, ZoneId.systemDefault)
  lazy val date = (localdatetime.getYear, localdatetime.getMonth, localdatetime.getDayOfMonth, localdatetime.getDayOfWeek)
  lazy val time = (localdatetime.getHour, localdatetime.getMinute, localdatetime.getSecond, localdatetime.getNano)

  def -(that: Instant) = Duration(that, this)

  override def toString: String = ???
}

case class Duration(start : Instant, end : Instant) {
  private[time] lazy val jper = Period.between(start.localdatetime.toLocalDate, end.localdatetime.toLocalDate)

  def milliSecondsTotal = ChronoUnit.MILLIS.between(start.jinst, end.jinst).toInt

  def secondsTotal = milliSecondsTotal / 1000

  def minutesTotal = secondsTotal / 60

  def hoursTotal = minutesTotal / 60

  def daysTotal = hoursTotal / 24

  def get = List(daysTotal, hoursTotal % 24, minutesTotal % 60, secondsTotal % 60, milliSecondsTotal % 1000)

  override def toString: String = get.map(_.toString).mkString(":")

  def niceString: String = {
    var nonzero = false
    val ret = {
      if (daysTotal > 0) {
        nonzero = true
        daysTotal + " Days, "
      } else ""
    } + {
      if (nonzero || hoursTotal > 0) {
        nonzero = true
        (hoursTotal % 24) + " Hours, "
      } else ""
    } + {
      if (nonzero || minutesTotal > 0) {
        nonzero = true
        (minutesTotal % 60) + " Minutes, "
      } else ""
    } + {
      if (nonzero || secondsTotal > 0) {
        nonzero = true
        (secondsTotal % 60) + " Seconds, "
      } else ""
    } + (milliSecondsTotal % 60) + " Milliseconds"
    ret
  }
}

object Time {
  def now = Instant(java.time.Instant.now())

  def measureAnd[A](a: => A): (Duration, A) = {
    val t0 = now
    val ret = {
      a
    }
    val t1 = now
    (t1 - t0, ret)
  }

  def measure(a: => Unit): Duration = {
    val (dur, _) = measureAnd {
      a
    }
    dur
  }
}

case class Date(ld : LocalDate) {
  val month = Month(ld.getMonth)
}

case class WeekDay(jday : java.time.DayOfWeek) {
  def toInt = jday.getValue
  override def toString = jday.toString.head + jday.toString.tail.toLowerCase
}
object WeekDay {
  def apply(i : Int) : WeekDay = WeekDay(java.time.DayOfWeek.of(i % 7))

  val Monday = WeekDay(java.time.DayOfWeek.of(1))
  val Tuesday = WeekDay(java.time.DayOfWeek.of(2))
  val Wednesday = WeekDay(java.time.DayOfWeek.of(3))
  val Thursday = WeekDay(java.time.DayOfWeek.of(4))
  val Friday = WeekDay(java.time.DayOfWeek.of(5))
  val Saturday = WeekDay(java.time.DayOfWeek.of(6))
  val Sunday = WeekDay(java.time.DayOfWeek.of(7))
}

case class Month(jmonth : java.time.Month) {
  def toInt = jmonth.getValue
  override def toString = jmonth.toString.head + jmonth.toString.tail.toLowerCase
}
object Month {
  def apply(i : Int) : Month = Month(java.time.Month.of(i % 12))

  val January = Month(java.time.Month.JANUARY)
  val February = Month(java.time.Month.FEBRUARY)
  val March = Month(java.time.Month.MARCH)
  val April = Month(java.time.Month.APRIL)
  val May = Month(java.time.Month.MAY)
  val June = Month(java.time.Month.JUNE)
  val July = Month(java.time.Month.JULY)
  val August = Month(java.time.Month.AUGUST)
  val September = Month(java.time.Month.SEPTEMBER)
  val October = Month(java.time.Month.OCTOBER)
  val November = Month(java.time.Month.NOVEMBER)
  val December = Month(java.time.Month.DECEMBER)
}

*/

/*
sealed class Month(int : Int) {
  def toJava = java.time.Month
  def toInt = int
  val days = 31
  override def toString = this.getClass.getCanonicalName.split('.').last.dropRight(1)
}

object Month {
  def apply(i : Int) = i % 12 match {
    case 0 => December
    case 1 => January
    case 2 => February
    case 3 => March
    case 4 => April
    case 5 => May
    case 6 => June
    case 7 => July
    case 8 => August
    case 9 => September
    case 10 => October
    case 11 => November
    case _ => throw GeneralError("Basic arithmetic broke!")
  }

  case object January extends Month(1)
  case object February extends Month(2) {
    override val days = 29
  }
  case object March extends Month(3)
  case object April extends Month(4) {
    override val days = 30
  }
  case object May extends Month(5)
  case object June extends Month(6) {
    override val days = 30
  }
  case object July extends Month(7)
  case object August extends Month(8)
  case object September extends Month(9) {
    override val days = 30
  }
  case object October extends Month(10)
  case object November extends Month(11) {
    override val days = 30
  }
  case object December extends Month(12)
}
*/
