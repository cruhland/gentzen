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

  def parse(text: String): Either[String, Formula] = {
    formulaSeqParser(text.toList) match {
      case Right((Nil, Nil)) => Left("parse: no formulas")
      case Right((Nil, formula :: Nil)) => Right(formula)
      case Right((Nil, formulas)) => Right(Group(formulas))
      case Right(_) => Left("parse: extra chars at end")
      case Left(message) => Left("parse: " + message)
    }
  }

  type GeneralParser[S, A] = S => Either[String, (S, A)]
  type Parser[A] = GeneralParser[List[Char], A]

  private def formulaSeqParser: Parser[List[Formula]] = {
    (chars: List[Char]) =>
      formulaParser(chars) match {
        case Right((cs1, firstFormula)) =>
          zeroOrMore(followingFormulaParser)(cs1) match {
            case Right((cs2, followingFormulas)) =>
              Right((cs2, firstFormula :: followingFormulas))
            case Left(message) => Left("formulaSeqParser: " + message)
          }
        case Left(message) => Left("formulaSeqParser: " + message)
      }
  }

  private def followingFormulaParser: Parser[Formula] = {
    (chars: List[Char]) =>
      chars match {
        case ' ' :: cs1 => formulaParser(cs1)
        case _ => Left("followingFormulaParser: missing space")
      }
  }

  private def zeroOrMore[A](parser: Parser[A]): Parser[List[A]] = {
    (chars: List[Char]) =>
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

  private def formulaParser: Parser[Formula] = { (chars: List[Char]) =>
    groupParser(chars) match {
      case Left(_) => atomParser(chars)
      case r => r
    }
  }

  private def atomParser: Parser[Atom] = { (chars: List[Char]) =>
    val (atomChars, remainingChars) = chars.span(isAtomChar)
    if (atomChars.isEmpty) Left("atomParser: need at least one char")
    else Right((remainingChars, Atom(atomChars.mkString)))
  }

  private def isAtomChar(c: Char): Boolean = {
    !(c == ' ' || c == '(' || c == ')')
  }

  private def groupParser: Parser[Group] = { (chars: List[Char]) =>
    chars match {
      case '(' :: cs1 =>
        formulaParser(cs1) match {
          case Right((cs2, firstFormula)) =>
            cs2 match {
              case ' ' :: cs3 =>
                formulaSeqParser(cs3) match {
                  case Right((cs4, otherFormulas)) =>
                    cs4 match {
                      case ')' :: cs5 =>
                        Right((cs5, Group(firstFormula :: otherFormulas)))
                      case _ => Left("groupParser: `)` expected")
                    }
                  case Left(message) => Left("groupParser: " + message)
                }
              case _ => Left("groupParser: space expected")
            }
          case Left(message) => Left("groupParser: " + message)
        }
      case _ => Left("groupParser: `(` expected")
    }
  }

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
