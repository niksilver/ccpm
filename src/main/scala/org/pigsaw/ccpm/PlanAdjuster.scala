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
    val backers = p.backingTasks(task)
    if (backers.nonEmpty) {
      ???
    } else {
      Seq(Actual(m))
    }
  }

  def make(p: Plan, m: Move): (Plan, Move) = ???
}