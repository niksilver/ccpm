package org.pigsaw.ccpm

/**
 * Description of a desire to move a `task` to a new `start`.
 */
case class Move(task: Task, start: Double) extends RippleMove[Move] {
  def samePiece(m2: Move): Boolean = (task == m2.task)
  def max(m2: Move) = Seq(this, m2) maxBy { _.start }
}

class PlanAdjuster extends RippleAdjuster[Plan, Move] {

  def attempt(p: Plan, m: Move): Seq[Attempt[Move]] = {
    val task = m.task
    val preventers = p.preventsMove(task, m.start)
    if (preventers.isEmpty) {
      Seq(Actual(m))
    } else {
      for {
        preventer <- preventers.toSeq
        if (!p.criticalChain.contains(preventer))
        extraNeeded = p.schedule.end(preventer) - m.start
        prevStart = p.schedule.start(preventer) - extraNeeded
      } yield Prerequisite(Move(preventer, prevStart))
    }
  }

  def make(p: Plan, m: Move): (Plan, Move) = ???
}