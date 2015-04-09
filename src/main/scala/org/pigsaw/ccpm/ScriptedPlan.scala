package org.pigsaw.ccpm

/* Copyright Nik Silver 2015.
 * 
 * This file is part of CCPM.
 *
 * CCPM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * CCPM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CCPM.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * A project plan described with the DSL.
 */
class ScriptedPlan extends PlanVerbs with Plan {

  /**
   * Tasks in the plan, in the order in which they were created.
   */
  lazy val tasks = scala.collection.immutable.ListSet(pc.tasks.toSeq.reverse: _*)
  
  /**
   * Resources in the plan, in the order in which they were declared
   */
  override lazy val resources: Set[String] = scala.collection.immutable.Set(pc.resources.toSeq: _*)

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  lazy val dependencies: Set[(Task, Task)] = scala.collection.immutable.Set(pc.dependencies.toSeq: _*)
}
