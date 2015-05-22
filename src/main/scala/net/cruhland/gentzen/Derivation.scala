package net.cruhland.gentzen

object Derivation {

  def matches(
    template: Formula,
    formula: Formula,
    varCheck: (Formula, String) => Boolean = (_, _) => false
  ): Boolean = {
    def helper(t: Formula, f: Formula): Option[Map[String, Formula]] = {
      (t, f) match {
        case (Atom(Constant(tv)), Atom(Constant(fv))) if tv == fv =>
          Some(Map.empty)
        case (Atom(FormulaVariable(fv, cv)), _) if cv.forall(varCheck(f, _)) =>
          Some(Map(fv -> f))
        case (Group(tcn), Group(fcn)) if tcn.size == fcn.size =>
          tcn.zip(fcn).map((helper _).tupled).reduce { (left, right) =>
            for {
              leftVars <- left
              rightVars <- right
              mergedVars <- merge(leftVars, rightVars)
            } yield mergedVars
          }
        case _ => None
      }
    }

    helper(template, formula).isDefined
  }

  private[this] def merge[A, B](
    left: Map[A, B], right: Map[A, B]
  ): Option[Map[A, B]] = {
    val allKeys = left.keySet ++ right.keySet

    val mergedEntries = allKeys.toSeq.flatMap { key =>
      (left.get(key), right.get(key)) match {
        case (Some(leftVal), Some(rightVal)) if leftVal == rightVal =>
          Some(key -> leftVal)
        case (Some(leftVal), None) => Some(key -> leftVal)
        case (None, Some(rightVal)) => Some(key -> rightVal)
        case _ => None
      }
    }

    if (mergedEntries.size == allKeys.size) Some(mergedEntries.toMap) else None
  }

}

sealed trait AtomValue
case class FormulaVariable(name: String, checkVar: Option[String])
    extends AtomValue
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
