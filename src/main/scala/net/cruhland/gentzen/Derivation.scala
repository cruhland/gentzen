package net.cruhland.gentzen

object Derivation {

  def matches(template: Formula, formula: Formula): Option[MatchError] = {
    template match {
      case Atom(atom) => atom match {
        case Constant(_) =>
          if (template == formula) None
          else Some(ConstantMismatch("0", formula))
        case _ => None
      }
      case _ => None
    }
  }

}

sealed trait AtomValue
case class FormulaVariable(name: String) extends AtomValue
case class Constant private(value: String) extends AtomValue

object Constant {

  val InvalidChars: Set[Char] = Set(' ', '(', ')')

  def build(value: String): Option[Constant] = {
    if (value.isEmpty || value.exists(InvalidChars)) None
    else Some(buildWithoutValidation(value))
  }

  def buildWithoutValidation(value: String): Constant = Constant(value)

}

sealed trait MatchError
case class ConstantMismatch(value: String, formula: Formula) extends MatchError
