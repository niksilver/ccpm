package org.pigsaw.ccpm

case class Move(task: Task, start: Double) extends RippleMove[Move] {
  def samePiece(m2: Move): Boolean = (task == m2.task)
  def max(m2: Move) = Seq(this, m2) maxBy { _.start }
}