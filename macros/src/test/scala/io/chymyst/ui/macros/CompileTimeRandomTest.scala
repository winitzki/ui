package io.chymyst.ui.macros

import io.chymyst.ui.macros.CompileTimeRandom
import munit.FunSuite

import java.util.UUID

class CompileTimeRandomTest extends FunSuite {

  def getLongIds: (Long, Long) = {
    val id1 = CompileTimeRandom.long
    val id2 = CompileTimeRandom.long
    (id1, id2)
  }

  def getUUIDs: (UUID, UUID) = {
    val id1 = CompileTimeRandom.uuid
    val id2 = CompileTimeRandom.uuid
    (id1, id2)
  }

  test("1") {
    val (id1, id2) = getLongIds
    val (id1a, id2a) = getLongIds
    assert(id1 != id2) // Must generate different long IDs when called at different places.
    assert(id1a != id2a)
    assert(id1 == id1a && id2 == id2a) // Must generate the same long IDs when called multiple times.
  }

  test("2") {
    val (id1, id2) = getUUIDs
    val (id1a, id2a) = getUUIDs
    assert(id1 != id2) // Must generate different UUIDs when called at different places.
    assert(id1a != id2a)
    assert(id1 == id1a && id2 == id2a) // Must generate the same UUIDs when called multiple times.
  }

}

