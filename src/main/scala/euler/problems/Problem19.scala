package euler.problems

import euler.utils.Problem

/**
  * @author guido
  */
object Problem19 extends Problem {
  def weekdayStream: Stream[Int] = 1 #:: 2 #:: 3 #:: 4 #:: 5 #:: 6 #:: 7 #:: weekdayStream

  def monthStream: Stream[Int] = 1 #:: 2 #:: 3 #:: 4 #:: 5 #:: 6 #:: 7 #:: 8 #:: 9 #:: 10 #:: 11 #:: 12 #:: monthStream

  val daysPerMonth: Map[Int, Int] = Map(1 -> 31, 2 -> 28, 3 -> 31, 4 -> 30, 5 -> 31, 6 -> 30, 7 -> 31, 8 -> 31, 9 -> 30,
    10 -> 31, 11 -> 30, 12 -> 31)

  case class Date(day: Int, month: Int, year: Int, weekday: Int)

  def dateStream(days: Stream[Int], months: Stream[Int], years: Stream[Int], weekdays: Stream[Int]): Stream[Date] = {
    Date(days.head, months.head, years.head, weekdays.head) #:: {
      if (days.tail.isEmpty) {
        if (months.head == 12) {
          dateStream((1 until daysPerMonth(1) + 1).toStream, months.tail, years.tail, weekdays.tail)
        } else if (months.head == 1) {
          if(years.head % 4 != 0) {
            dateStream((1 until daysPerMonth(2) + 1).toStream, months.tail, years, weekdays.tail)
          } else if(years.head % 100 != 0) {
            // Leap year
            dateStream((1 until 30).toStream, months.tail, years, weekdays.tail)
          } else if(years.head % 400 != 0) {
            dateStream((1 until daysPerMonth(2) + 1).toStream, months.tail, years, weekdays.tail)
          } else {
            // Leap year
            dateStream((1 until 30).toStream, months.tail, years, weekdays.tail)
          }
        } else {
          dateStream((1 until daysPerMonth(months.tail.head) + 1).toStream, months.tail, years, weekdays.tail)
        }
      } else {
        dateStream(days.tail, months, years, weekdays.tail)
      }
    }
  }


  private val count = dateStream((1 until daysPerMonth(1) + 1).toStream, monthStream, Stream.from(1900), weekdayStream)
    .dropWhile(_.year < 1901)
    .takeWhile(_.year < 2001)
    .count {
      case Date(1, _, _, 7) => true
      case _ => false
    }

  def solution(): Int = count
}
