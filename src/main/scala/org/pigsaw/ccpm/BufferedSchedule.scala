package org.pigsaw.ccpm

class BufferedSchedule(starts: Map[Task, Double], val buffers: Set[Buffer]) extends Schedule(starts) {

}

object BufferedSchedule {
  
  /** Make a schedule with buffers,
   *  based on an existing (non-buffered) schedule
   */
  def make(sch: Schedule) = {
    val tasks = sch.tasks
    val taskIds = tasks map { _.id }
    val ids = if (tasks.isEmpty) Set() else Set(Buffer.nextId(taskIds))
    val buffers = ids map { Buffer(_, 0)}
    new BufferedSchedule(sch.starts, buffers)
  }
  
}