package io.chymyst.ui.dhall.unit

import io.chymyst.ui.dhall.Grammar
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
    check(primitiveExpressions ++ selectorExpressions ++ completionExpressions ++ importExpressions,
      Grammar.import_expression(_))
  }

  test("plus_expression") {
    check(primitiveExpressions ++ selectorExpressions ++ completionExpressions ++ importExpressions ++ plusExpressions,
      Grammar.plus_expression(_))
  }

  test("primitive_expression") {
    check(recordExpressions ++ primitiveExpressions,
      Grammar.primitive_expression(_))
  }

  test("let_binding") {
    check(letBindings,
      Grammar.let_binding(_))
  }

  test("expression_let_binding") {
    check(letBindingExpressions,
      Grammar.expression_let_binding(_))
  }
}
