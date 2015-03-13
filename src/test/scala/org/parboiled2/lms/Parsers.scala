package org.parboiled2.lms

import scala.lms.tutorial.{DslDriver, DslGen, TutorialFunSuite}
import scala.virtualization.lms.common.{Base, BaseExp, ScalaGenBase}

trait ParboiledOps extends Base {
  implicit def string2rep(s: String): Rep[Rule] = str(unit(s))

  trait Rule

  def str(s: Rep[String]): Rep[Rule]

  implicit class RepOps(r: Rep[Rule]) {
    def ~(l: Rep[Rule]): Rep[Rule] = sequence(r, l)
    def |(l: Rep[Rule]): Rep[Rule] = firstOf(r, l)
    def unary_! : Rep[Rule] = not(r)
    def parse(input: Rep[String]): Rep[Boolean] = parseInput(r, input)
  }

  def zeroOrMore(r: Rep[Rule]): Rep[Rule]
  def oneOrMore(r: Rep[Rule]): Rep[Rule]
  def optional(r: Rep[Rule]): Rep[Rule]
  def &(r: Rep[Rule]): Rep[Rule]
  def not(r: Rep[Rule]): Rep[Rule]
  def rec(ruleName: String, r: => Rep[Rule]): Rep[Rule]
  def sequence(rhs: Rep[Rule], lhs: Rep[Rule]): Rep[Rule]
  def firstOf(rhs: Rep[Rule], lhs: Rep[Rule]): Rep[Rule]

  def parseInput(r: Rep[Rule], input: Rep[String]): Rep[Boolean]
}

trait ParboiledOpsExp extends ParboiledOps with BaseExp {
  case class StringLiteral(str: Exp[String]) extends Def[Rule]
  case class Sequence(lhs: Exp[Rule], rhs: Exp[Rule]) extends Def[Rule]
  case class FirstOf(lhs: Exp[Rule], rhs: Exp[Rule]) extends Def[Rule]
  case class ZeroOrMore(r: Exp[Rule]) extends Def[Rule]
  case class OneOrMore(r: Exp[Rule]) extends Def[Rule]
  case class Optional(r: Exp[Rule]) extends Def[Rule]
  case class AndPredicate(r: Exp[Rule]) extends Def[Rule]
  case class NotPredicate(r: Exp[Rule]) extends Def[Rule]
  case class Recursive(ruleName: String, r: () => Exp[Rule]) extends Def[Rule]
  case class Parse(r: Exp[Rule], str: Exp[String]) extends Def[Boolean]

  def str(s: Exp[String]): Exp[Rule] = StringLiteral(s)
  def sequence(lhs: Exp[Rule], rhs: Exp[Rule]): Exp[Rule] = Sequence(lhs, rhs)
  def firstOf(lhs: Exp[Rule], rhs: Exp[Rule]): Exp[Rule] = FirstOf(lhs, rhs)
  def zeroOrMore(r: Exp[Rule]): Exp[Rule] = ZeroOrMore(r)
  def oneOrMore(r: Exp[Rule]): Exp[Rule] = OneOrMore(r)
  def optional(r: Exp[Rule]): Exp[Rule] = Optional(r)
  def &(r: Exp[Rule]): Exp[Rule] = AndPredicate(r)
  def not(r: Exp[Rule]): Exp[Rule] = NotPredicate(r)
  def rec(ruleName: String, r: => Exp[Rule]): Exp[Rule] = Recursive(ruleName, () => r)

  def parseInput(r: Exp[Rule], input: Exp[String]): Exp[Boolean] = Parse(r, input)
}

class Parsers extends TutorialFunSuite {
  override val under: String = "parboiled2"

  test("1") {
    val snippet = new DslDriver[String, Boolean] with ParboiledOpsExp { q =>
      override val codegen = new DslGen {
        val IR: q.type = q

        override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
          case StringLiteral(s) => emitValDef(sym, s"true // StringLiteral(${quote(s)})")
          case Parse(r, input) =>
            val cursor = fresh[Int]
            emitVarDecl(cursor)
            emitValDef(sym, s"true // Parse(${quote(r)}, ${quote(input)}) | cursor: ${quote(cursor)}")
          case Sequence(l, r) => emitValDef(sym, s"true // Sequence(${quote(l)}, ${quote(r)})")
          case FirstOf(l, r) => emitValDef(sym, s"true // FirstOf(${quote(l)}, ${quote(r)})")
          case ZeroOrMore(r) => emitValDef(sym, s"true // ZeroOrMore(${quote(r)})")
          case OneOrMore(r) => emitValDef(sym, s"true // OneOrMore(${quote(r)})")
          case Optional(r) => emitValDef(sym, s"true // Optional(${quote(r)})")
          case AndPredicate(r) => emitValDef(sym, s"true // AndPredicate(${quote(r)})")
          case NotPredicate(r) => emitValDef(sym, s"true // NotPredicate(${quote(r)})")
          case Recursive(rn, r) => emitValDef(sym, s"true // Recursive($rn, $r)")
          case _ => super.emitNode(sym, rhs)
        }
      }

      def InputLine = &(A ~ "c") ~ oneOrMore("a") ~ B ~ !(str("a") | "b" | "c")
      def A: Rep[Rule] = str("a") ~ optional(rec("A", A)) ~ "b"
      def B: Rep[Rule] = str("b") ~ optional(rec("B", B)) ~ "c"

      def snippet(x: Rep[String]) = {
        InputLine.parse(x)
      }
    }
    snippet.dumpGeneratedCode = true
    println(snippet.eval("abc"))
  }
}
