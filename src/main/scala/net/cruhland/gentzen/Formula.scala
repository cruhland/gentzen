package net.cruhland.gentzen

import collection.generic.Growable

sealed trait Formula {

  import Formula._

  final def render: String = {
    val sb = new StringBuilder
    renderInto(this, sb)
    sb.toString
  }

}

object Formula {

  private def renderInto(f: Formula, target: Growable[Char]): Unit = {
    f match {
      case Atom(name) => target ++= name
      case Group(children) =>
        target += '('
        renderInto(children.head, target)
        for (child <- children.tail) {
          target += ' '
          renderInto(child, target)
        }
        target += ')'
    }
  }

}

case class Atom(name: String) extends Formula
case class Group(children: List[Formula]) extends Formula
