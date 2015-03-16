package org.parboiled2.lms

import scala.virtualization.lms.common._

trait ParboiledOpsExp extends ParboiledOps with BaseExp with TupleOpsExp {
  case class ParserStateDef(input: Exp[String], cursor: Exp[Int]) extends Def[ParserState]

  case class Empty() extends Def[Rule]
  case class StringLiteral(str: Exp[String], ps: Exp[ParserState]) extends Def[Rule]
  case class Sequence(lhs: Exp[Rule], rhs: Exp[Rule], ps: Exp[ParserState]) extends Def[Rule]
  case class FirstOf(lhs: Exp[Rule], rhs: Exp[Rule], ps: Exp[ParserState]) extends Def[Rule]
  case class ZeroOrMore(r: Exp[Rule], ps: Exp[ParserState]) extends Def[Rule]
  case class NotPredicate(r: Exp[Rule], ps: Exp[ParserState]) extends Def[Rule]
  case class Recursive(ruleName: String, r: () => Exp[Rule], ps: Exp[ParserState]) extends Def[Rule]
  case class Parse(r: Exp[Rule], ps: Exp[String]) extends Def[Boolean]

  def str(s: Exp[String])(implicit ps: Exp[ParserState]): Exp[Rule] = StringLiteral(s, ps)
  def sequence(lhs: Exp[Rule], rhs: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = Sequence(lhs, rhs, ps)
  def firstOf(lhs: Exp[Rule], rhs: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = FirstOf(lhs, rhs, ps)
  def zeroOrMore(r: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = ZeroOrMore(r, ps)
  def oneOrMore(r: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = sequence(r, zeroOrMore(r))
  def optional(r: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = firstOf(r, Empty())
  def &(r: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = not(not(r))
  def not(r: Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = NotPredicate(r, ps)
  def rec(ruleName: String, r: => Exp[Rule])(implicit ps: Exp[ParserState]): Exp[Rule] = Recursive(ruleName, () => r, ps)

  def parseInput(r: Exp[Rule], ps: Exp[String]): Exp[Boolean] = Parse(r, ps)
  def newParserState(input: Exp[String]): Exp[ParserState] = ParserStateDef(input, fresh[Int])
}
