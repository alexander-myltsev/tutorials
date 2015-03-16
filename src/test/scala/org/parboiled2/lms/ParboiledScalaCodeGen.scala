package org.parboiled2.lms

import scala.lms.tutorial._
import scala.virtualization.lms.common._

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
