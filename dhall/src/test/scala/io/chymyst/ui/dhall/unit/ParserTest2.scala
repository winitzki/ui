package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Grammar
import io.chymyst.ui.dhall.unit.ParserTest.{check, toFail}
import io.chymyst.ui.dhall.unit.TestFixtures._
import munit.FunSuite

class ParserTest2 extends FunSuite {

  test("import_only") {
    importExpressions.foreach { case (s, d) =>
      check(Grammar.import_only(_), s, d, s.length)
    }
  }

  test("import_hashed") {
    import io.chymyst.ui.dhall.Grammar
    import io.chymyst.ui.dhall.SyntaxConstants.File
    import io.chymyst.ui.dhall.SyntaxConstants.FilePrefix.Here
    import io.chymyst.ui.dhall.SyntaxConstants.ImportType.Path

    check(Grammar.import_hashed(_), s"./local/import sha256:$sha256example", (Path(Here, File(List
    ("local", "import"))), Some(sha256example)), 86)
  }

  test("import_expression") {
    (primitiveExpressions ++ selectorExpressions ++ completionExpressions ++ importExpressions).foreach { case (s, d) =>
      check(Grammar.import_expression(_), s, d, s.length)
    }
  }


}
