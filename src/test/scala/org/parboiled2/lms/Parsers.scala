package org.parboiled2.lms

import scala.collection.mutable
import scala.lms.tutorial._

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
    assert(snippet.eval("abcpppppp") === true)
    assert(snippet.eval("abc:pppppp") === true)
    assert(snippet.eval("abc:p") === true)
    assert(snippet.eval("abc:") === false)
    assert(snippet.eval("abc") === false)
  }
}
