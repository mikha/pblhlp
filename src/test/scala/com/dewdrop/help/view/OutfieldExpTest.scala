package com.dewdrop.help.view

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OutfieldExpTest extends AnyWordSpec with Matchers {
  "outfield player entry" should {
    "parse input" in {
      check("   2 3d4", (2, 3, 4))
      check("10 11 12", (10, 11, 12))
      check("21\t22\t23\t  ", (21, 22, 23))
      check("5+10+15", (5, 10, 15))
      check("3 - 12 - 25", (3, 12, 25))
      check("6 974\t39 568\t6 250", (6974, 39568, 6250))
    }
  }
  def check(input: String, expectation: (Int, Int, Int)): Unit = {
    input match {
      case OutfieldExp(e1, e2, e3) =>
        e1 shouldBe expectation._1
        e2 shouldBe expectation._2
        e3 shouldBe expectation._3
      case _ => fail(s"$input is wrong")
    }
  }
}
