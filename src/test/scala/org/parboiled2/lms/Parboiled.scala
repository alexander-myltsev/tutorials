package org.parboiled2.lms

import scala.lms.tutorial.Dsl

import scala.lms.tutorial.TutorialFunSuite

import scala.lms.tutorial.DslDriver

trait ParboiledStaged extends Dsl {
  trait ParserState {
    val input: Rep[String]
    var cursor: Rep[Int]
    val stack: Rep[Array[Any]] = Array[Any]("_", 0, 0, 0, 0, 0, 0, 0)
    var stackIdx: Rep[Int] = 0

    def push(v: Rep[Any]) = {
      stack(stackIdx) = v
      stackIdx += 1
    }

    def pop(): Rep[Any] = {
      stackIdx -= 1
      stack(stackIdx)
    }

    def showStack() = {
      print("value-stack: ")
      for (i ← 0 until stack.length - 1: Rep[Range]) {
        print(stack(i) + ", ")
      }
      println(stack(stack.length - 1))
    }
  }

  type Rule = ParserState ⇒ Rep[Boolean]

  implicit def strToRule(s: String): Rule = str(s)

  def str(s: Rep[String]): Rule = new Rule {
    override def apply(ps: ParserState): Rep[Boolean] = {
      val len = string_length(s)
      if (string_length(ps.input) >= ps.cursor + len && ps.input.substring(ps.cursor, ps.cursor + len) == s) {
        ps.cursor += len
        true
      } else false
    }
  }

  def capture(r: Rule) = Capture(r)

  implicit class RuleOps(r: Rule) {
    def |(l: Rule) = FirstOf(r, l)
    def ~(l: Rule) = Sequence(r, l)
  }

  case class FirstOf(lhs: Rule, rhs: Rule) extends Rule {
    override def apply(ps: ParserState): Rep[Boolean] = {
      val p = ps.cursor
      if (lhs(ps)) true
      else {
        ps.cursor = p
        rhs(ps)
      }
    }
  }

  case class Sequence(lhs: Rule, rhs: Rule) extends Rule {
    override def apply(ps: ParserState): Rep[Boolean] = {
      lhs(ps) && rhs(ps)
    }
  }

  case class Capture(r: Rule) extends Rule {
    override def apply(ps: ParserState): Rep[Boolean] = {
      val pos = ps.cursor
      val res = r(ps)
      if (res) {
        ps.push(ps.input.substring(pos, ps.cursor))
      }
      res
    }
  }

  def parse(rule: Rule, text: Rep[String]): Rep[Boolean] = {
    val ps = new ParserState {
      val input: Rep[String] = text
      var cursor: Rep[Int] = 0
    }
    val res = rule(ps)
    ps.showStack()
    res
  }
}

class ParboiledStagedTest extends TutorialFunSuite {
  val under = "pb2"
  val snippet = new DslDriver[String, Boolean] with ParboiledStaged {
    val InputRule: Rule = (str("ab") ~ "cd") | capture("def")

    def snippet(x: Rep[String]) = parse(InputRule, x)
  }
  snippet.dumpGeneratedCode = true

  def testmatch(text: String, expected: Boolean) {
    test(s"""parse("$text") == $expected""") {
      assertResult(expected) { snippet.eval(text) }
    }
  }

  testmatch("def", true)
  testmatch("abd", false)
  testmatch("kkk", false)
}
