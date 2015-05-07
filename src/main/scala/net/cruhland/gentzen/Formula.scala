package net.cruhland.gentzen

import collection.generic.Growable

sealed trait Formula[A] {

  import Formula._

  final def render(implicit ev: Formula[A] => Formula[String]): String = {
    val sb = new StringBuilder
    renderInto(ev(this), sb)
    sb.toString
  }

}

object Formula {

  def parse(text: String): Either[String, Formula[String]] = {
    formulaSeqParser(text.toList) match {
      case Right((Nil, Nil)) => Left("parse: no formulas")
      case Right((Nil, formula :: Nil)) => Right(formula)
      case Right((Nil, formulas)) =>
        // `formulas` must contain at least two elements, therefore
        // we can call `Group.buildWithoutValidation()`
        Right(Group.buildWithoutValidation(formulas))
      case Right(_) => Left("parse: extra chars at end")
      case Left(message) => Left("parse: " + message)
    }
  }

  case class GeneralParser[S, A](f: S => Either[String, (S, A)]) {

    def apply(medium: S): Either[String, (S, A)] = f(medium)

    def map[B](g: A => B): GeneralParser[S, B] = {
      GeneralParser(s => f(s).right map { case (s1, a) => (s1, g(a)) })
    }

    def flatMap[B](g: A => GeneralParser[S, B]): GeneralParser[S, B] = {
      GeneralParser { s =>
        f(s) match {
          case Right((s1, a)) => g(a)(s1)
          case Left(message) => Left(message)
        }
      }
    }

  }

  type Parser[A] = GeneralParser[List[Char], A]

  private def char(v: Char): Parser[Char] = {
    GeneralParser { (chars: List[Char]) =>
      chars match {
        case c :: cs if c == v => Right((cs, c))
        case _ => Left("expected char '" + v + "'")
      }
    }
  }

  private def formulaSeqParser: Parser[List[Formula[String]]] = {
    for {
      firstFormula <- formulaParser
      followingFormulas <- zeroOrMore(followingFormulaParser)
    } yield firstFormula :: followingFormulas
  }

  private def followingFormulaParser: Parser[Formula[String]] = {
    for {
      _ <- char(' ')
      formula <- formulaParser
    } yield formula
  }

  private def zeroOrMore[A](parser: Parser[A]): Parser[List[A]] = {
    GeneralParser { (chars: List[Char]) =>
      parser(chars) match {
        case Right((cs1, parsedValue)) =>
          zeroOrMore(parser)(cs1) match {
            case Right((cs2, parsedValues)) =>
              Right((cs2, parsedValue :: parsedValues))
            case Left(message) => Left("zeroOrMore: " + message)
          }
        case _ => Right((chars, Nil))
      }
    }
  }

  private def formulaParser: Parser[Formula[String]] = {
    GeneralParser { (chars: List[Char]) =>
      groupParser(chars) match {
        case Left(_) => atomParser(chars)
        case r => r
      }
    }
  }

  private def atomParser: Parser[Atom[String]] = {
    GeneralParser { (chars: List[Char]) =>
      val (atomChars, remainingChars) = chars.span(!Atom.InvalidNameChars(_))
      if (atomChars.isEmpty) {
        Left("atomParser: need at least one char")
      } else {
        // Call `Atom.buildWithoutValidation()` because we
        // know `atomChars` only contains valid name chars
        val atom = Atom.buildWithoutValidation(new String(atomChars.toArray))
        Right((remainingChars, atom))
      }
    }
  }

  private def isAtomChar(c: Char): Boolean = {
    !(c == ' ' || c == '(' || c == ')')
  }

  private def groupParser: Parser[Group[String]] = {
    for {
      _ <- char('(')
      firstFormula <- formulaParser
      _ <- char(' ')
      otherFormulas <- formulaSeqParser
      _ <- char(')')
    } yield {
      // `formulaSeqParser` guarantees that `otherFormulas` will contain
      // at least one element, thus we can use `buildWithoutValidation()`
      Group.buildWithoutValidation(firstFormula :: otherFormulas)
    }
  }

  private def renderInto(f: Formula[String], target: Growable[Char]): Unit = {
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

case class Atom[A] private(name: A) extends Formula[A]

object Atom {

  val InvalidNameChars: Set[Char] = Set(' ', '(', ')')

  def build(name: String): Option[Atom[String]] = {
    if (name.isEmpty || name.exists(InvalidNameChars)) None
    else Some(buildWithoutValidation(name))
  }

  def buildWithoutValidation(name: String): Atom[String] = Atom(name)

  def buildSchema(schema: Schema): Atom[Schema] = Atom(schema)

}

case class Group[A] private(children: Seq[Formula[A]]) extends Formula[A]

object Group {

  def build[A](children: Traversable[Formula[A]]): Option[Group[A]] = {
    if (children.size < 2) None
    else Some(buildWithoutValidation(children))
  }

  def buildWithoutValidation[A](children: Traversable[Formula[A]]): Group[A] = {
    Group(children.toSeq)
  }

}
