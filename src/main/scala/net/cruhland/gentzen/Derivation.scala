package net.cruhland.gentzen

object Derivation {

  def matches(
    template: Formula[Schema], formula: Formula[String]
  ): Option[MatchError] = {
    template match {
      case Atom(Constant("0")) =>
        formula match {
          case Atom("0") => None
          case _ => Some(ConstantMismatch("0", formula))
        }
      case _ => None
    }
  }

}

sealed trait Schema
case class FormulaVariable(name: String) extends Schema
case class Constant(name: String) extends Schema

sealed trait MatchError
case class ConstantMismatch(name: String, formula: Formula[String]) extends MatchError
