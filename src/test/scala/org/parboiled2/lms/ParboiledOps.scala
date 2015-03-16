package org.parboiled2.lms

import scala.virtualization.lms.common._

trait ParboiledOps extends Base with TupleOps {
  implicit def string2rep(s: String)(implicit ps: Rep[ParserState]): Rep[Rule] = str(unit(s))

  trait Rule
  trait ParserState

  def str(s: Rep[String])(implicit pi: Rep[ParserState]): Rep[Rule]
  def newParserState(input: Rep[String]): Rep[ParserState]

  implicit class RepOps(r: Rep[Rule]) {
    def ~(l: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule] = sequence(r, l)
    def |(l: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule] = firstOf(r, l)
    def unary_!(implicit ps: Rep[ParserState]) : Rep[Rule] = not(r)
    def parse(ps: Rep[String]): Rep[Boolean] = parseInput(r, ps)
  }

  def zeroOrMore(r: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def oneOrMore(r: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def optional(r: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def &(r: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def not(r: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def rec(ruleName: String, r: => Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def sequence(rhs: Rep[Rule], lhs: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]
  def firstOf(rhs: Rep[Rule], lhs: Rep[Rule])(implicit ps: Rep[ParserState]): Rep[Rule]

  def parseInput(r: Rep[Rule], ps: Rep[String]): Rep[Boolean]
}
