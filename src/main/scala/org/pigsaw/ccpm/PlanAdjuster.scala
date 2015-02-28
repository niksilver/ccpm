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

  def move(p: Plan, m: Move): (Plan, Move) = {
    val task = m.task
    if (p.criticalChain contains task) {
      dontMove(p, task)
    } else {
      doMove(p, task, m.start)
    }
  }
  
  private def dontMove(p: Plan, task: Task): (Plan, Move) = {
    val start = p.schedule.start(task)
    (p, Move(task, start))
  }
  
  private def doMove(p: Plan, task: Task, start: Double): (Plan, Move) = {
    val maxDelta = p.schedule.start(task) - start
    val sch2 = p.moveBack(task, maxDelta)
    val p2 = p.withSchedule(sch2)
    val m2 = Move(task, sch2.start(task))
    (p2, m2)
  }
}