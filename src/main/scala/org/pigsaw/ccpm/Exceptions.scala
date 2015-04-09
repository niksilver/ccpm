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

class ProjectException(msg: String) extends Exception(msg)

class DuplicateTaskException(msg: String) extends ProjectException(msg)
class UnknownPeriodException(msg: String) extends ProjectException(msg)
class UnknownTaskException(msg: String) extends ProjectException(msg)
class UnknownResourceException(msg: String) extends ProjectException(msg)
class CyclicDependencyException(msg: String) extends ProjectException(msg)
class DuplicateDependencyException(msg: String) extends ProjectException(msg)
