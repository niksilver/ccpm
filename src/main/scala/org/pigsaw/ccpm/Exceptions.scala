package org.pigsaw.ccpm

class ProjectException(msg: String) extends Exception(msg) {
  def this() = this("")
}

class DuplicateTaskException(msg: String) extends ProjectException(msg)
class UnknownTaskException(msg: String) extends ProjectException(msg)
class UnknownResourceException(msg: String) extends ProjectException(msg)
class CyclicDependencyException(msg: String) extends ProjectException(msg)
class DuplicateDependencyException(msg: String) extends ProjectException(msg)
