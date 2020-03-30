/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

  // Tests of an empty B-tree

  "An puzzle state with 5 frogs and 8 toads:" should
    "have 5 + 8 + 1 = 14 cells" in {
    assert(PuzzleState(5, 8).size == 14)
  }

  it should "have its empty cell at position 5" in {
    assertResult(5) {
      PuzzleState(5, 8).emptyLoc
    }
  }

  it should "be constructed in the initial puzzle state" in {
    assert(PuzzleState(5, 8).isInitialState())
  }

  it should "not be constructed in the terminal puzzle state" in {
    assert(!PuzzleState(5, 8).isTerminalState())
  }

  // Tests For Slide and Jump Helper Functions
  
  "A puzzle state with 4 frogs and 3 toads using slideFromRight:" should
    "now have an empty cell at position 5" in {
    assert(PuzzleState(4, 3).slideFromRight.get.emptyLoc == 5)
  }

  "A puzzle state with 1 frogs and 3 toads using slideFromRight, jumpFromLeft, jumpFromRight:" should
    "return None because the move is illegal (Out of Bounds Exception)" in {
    assert(PuzzleState(1, 3).slideFromRight.get.jumpFromLeft.get.jumpFromRight == None)
  }

  "A puzzle state with 3 frogs and 7 toads using slideFromLeft:" should
    "now have an empty cell at position 2" in {
    assert(PuzzleState(3, 7).slideFromLeft.get.emptyLoc == 2)
  }

  "A puzzle state with 4 frogs and 3 toads using slideFromRight then jumpFromLeft:" should
    "now have an empty cell at position 3" in {
    assert(PuzzleState(4, 3).slideFromRight.get.jumpFromLeft.get.emptyLoc == 3)
  }

  "A puzzle state with 2 frogs and 8 toads using slideFromLeft then jumpFromRight:" should
    "now have an empty cell at position 3" in {
    assert(PuzzleState(2, 8).slideFromLeft.get.jumpFromRight.get.emptyLoc == 3)
  }

}
