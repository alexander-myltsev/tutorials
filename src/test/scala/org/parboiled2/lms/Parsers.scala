package org.parboiled2.lms

import scala.lms.tutorial._
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

abstract class ParboiledScalaCodeGen[A:Manifest,B:Manifest] extends DslDriver[A,B] with ParboiledOpsExp { q =>
  override val codegen = new DslGen with ScalaGenTupleOps {
    val IR: q.type = q

    override def remap[C](m: Manifest[C]) = m.runtimeClass.getSimpleName match {
      case s if s.startsWith("Tuple") => m.runtimeClass.getSimpleName + "[" + m.typeArguments.map(a => structName(a)).mkString(", ") + "]"
      case _ => super.remap(m)
    }

    def emitDefDef(sym: Sym[Any], rhs: String): Unit = {
      stream.println("def " + quote(sym) + " = " + rhs)
    }

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case ParserStateDef(input, cursor: Sym[Variable[Any]]) =>
        emitVarDef(cursor, "0")
      case Empty() => emitValDef(sym, "true")
      case StringLiteral(s, psSym: Sym[_]) =>
        val Def(ps: ParserStateDef) = psSym
        val qpsc = quote(ps.cursor)
        val qpsi = quote(ps.input)
        emitDefDef(sym, s"""{
           |  val len = ${quote(s)}.length
           |  if ($qpsc + len <= $qpsi.length && $qpsi.substring($qpsc, $qpsc + len) == ${quote(s)}) { $qpsc += len; true }
           |  else false
           |}""".stripMargin)
      case Parse(r, ps) =>
        emitValDef(sym, s"${quote(r)}")
      case Sequence(l, r, psSym: Sym[_]) =>
        val Def(ps: ParserStateDef) = psSym
        val qpsc = quote(ps.cursor)
        emitDefDef(sym,s"""{
             |  val save = $qpsc
             |  if (${quote(l)} && ${quote(r)}) true
             |  else {
             |    $qpsc = save
             |    false
             |  }
             |}""".stripMargin)
      case FirstOf(l, r, psSym: Sym[_]) =>
        val Def(ps: ParserStateDef) = psSym
        val qpsc = quote(ps.cursor)
        emitDefDef(sym, s"""{
           |  val save = $qpsc
           |  if (${quote(l)}) true
           |  else {
           |    $qpsc = save
           |    ${quote(r)}
           |  }
           |}""".stripMargin)
      case ZeroOrMore(r, psSym: Sym[_]) =>
        val Def(ps: ParserStateDef) = psSym
        val qpsc = quote(ps.cursor)
        emitDefDef(sym,
          s"""{
           |  var save = $qpsc
           |  while (${quote(r)}) {
           |    save = $qpsc
           |  }
           |  $qpsc = save
           |  true
           |}""".stripMargin)
      case NotPredicate(r, psSym: Sym[_]) =>
        val Def(ps: ParserStateDef) = psSym
        val qpsc = quote(ps.cursor)
        emitDefDef(sym, s"""{
           |  val save = $qpsc
           |  val r = ${quote(r)}
           |  $qpsc = save
           |  !r
           |}""".stripMargin)
      case Recursive(rn, r, psSym: Sym[_]) => ???
      case _ => super.emitNode(sym, rhs)
    }
  }
}

class Parsers extends TutorialFunSuite {
  override val under: String = "parboiled2"

  test("1") {
    val snippet = new ParboiledScalaCodeGen[String, Boolean] {
      def snippet(input: Rep[String]) = {
        implicit val ps: Rep[ParserState] = newParserState(input)

//        def InputLine = &(A ~ "c") ~ oneOrMore("a") ~ B ~ !(str("a") | "b" | "c")
//        def A: Rep[Rule] = str("a") ~ optional(rec("A", A)) ~ "b"
//        def B: Rep[Rule] = str("b") ~ optional(rec("B", B)) ~ "c"

        def InputLine = &(str("a") ~ "b") ~ "abc" ~ optional(":") ~ oneOrMore("p")

        InputLine.parse(input)
      }
    }
    snippet.dumpGeneratedCode = true
    println(snippet.eval("abc:pppppp"))
  }
}
