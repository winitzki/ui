package io.chymyst.ui.macros.unit
import utest._
import io.chymyst.ui.macros.CompileTimeRandom._

import java.util.UUID
class CompileTimeRandomTest extends TestSuite {

  case class DataWithId(x: Int, id: UUID)

  case class DataWithImplicitId(x: Int)(implicit val id: UUID)

  val tests: Tests = this {
    * -  {

    }
    * - {

    }
  }
}

