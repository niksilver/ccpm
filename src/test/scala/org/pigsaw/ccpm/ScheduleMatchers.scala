package org.pigsaw.ccpm

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

/**
 * Scalatest `Matchers` that are useful for the `Schedule` class.
 */
trait ScheduleMatchers {

  // The following definitions allow us to more easily assert
  // that a half-duration task should come right before another task:
  //
  //   t1 should halfEndRightBefore t2
  //
  // See http://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers

  case class MatchingSchedule(sch: Schedule)

  class TaskEndRightBefore(tLater: Task, sch: Schedule) extends Matcher[Task] {
    def apply(tEarlier: Task) = {
      val end = sch.end(tEarlier)
      val earlierStart = sch.start(tEarlier)
      val laterStart = sch.start(tLater)
      MatchResult(
        end == laterStart,
        s"$tEarlier with start $earlierStart did not come right before $tLater with start $laterStart",
        s"$tEarlier with start $earlierStart came right before $tLater with start $laterStart")
    }
  }

  def endRightBefore(tEarlier: Task)(implicit iSched: MatchingSchedule) = new TaskEndRightBefore(tEarlier, iSched.sch)

  // The following definitions allow us to more easily assert
  // that a half-duration task should come some time before another task:
  //
  //   t1 should halfEndSomeTimeBefore t2

  class TaskEndSomeTimeBefore(tLater: Task, sch: Schedule) extends Matcher[Task] {
    def apply(tEarlier: Task) = {
      val end = sch.end(tEarlier)
      val earlierStart = sch.start(tEarlier)
      val laterStart = sch.start(tLater)
      MatchResult(
        end <= laterStart,
        s"$tEarlier with start $earlierStart did not half-end some time before $tLater with start $laterStart",
        s"$tEarlier with start $earlierStart half-ended some time before $tLater with start $laterStart")
    }
  }

  def endSomeTimeBefore(tEarlier: Task)(implicit iSched: MatchingSchedule) = new TaskEndSomeTimeBefore(tEarlier, iSched.sch)

}