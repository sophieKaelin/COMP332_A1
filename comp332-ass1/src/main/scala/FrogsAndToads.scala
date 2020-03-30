/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._

/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState  (
    board: Vector[PuzzleState.Cell],
    loc: Int
) {

  import PuzzleState._

  val size = board.size
  val emptyLoc = loc

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  } 

/**
  * Helper Methods for Slide and Jump Operations 
  */
  def slideFromRight(): Option[PuzzleState] = {
  	val e = this.emptyLoc
  	val b = this.board
  	val newEmpty = e + 1
  	if(newEmpty >= 0 && newEmpty < b.size && e >= 0 && e < b.size && b(newEmpty) == Toad){
  		Some(new PuzzleState(b.slice(0, e) ++
					Vector(Toad, Empty) ++
					b.slice(e + 2, b.size), newEmpty
			))
  	}
  	else {
      None
    }
  }

  def slideFromLeft(): Option[PuzzleState] = {
  	val e = this.emptyLoc
  	val b = this.board
  	val newEmpty = e - 1
  	if(newEmpty >= 0 && newEmpty < b.size && e >= 0 && e < b.size && b(newEmpty) == Frog){
  		Some(new PuzzleState(b.slice(0, e - 1) ++
					Vector(Empty, Frog) ++
					b.slice(e + 1, b.size), newEmpty
			))
  	}
    else {
  	 None
    }
  }

  def jumpFromLeft(): Option[PuzzleState] = {
  	val e = this.emptyLoc
  	val b = this.board
  	val newEmpty = e - 2
  	if(newEmpty >= 0 && newEmpty < b.size && e >= 0 && e < b.size && b(newEmpty) == Frog){
  		Some(new PuzzleState(b.slice(0, e - 2) ++
					Vector(Empty, b(e-1) , Frog) ++
					b.slice(e + 1, b.size), newEmpty
			))
  	}
    else {
      None
    }
  }

  def jumpFromRight(): Option[PuzzleState] = {
    val e = this.emptyLoc
    val b = this.board
    val newEmpty = e + 2
    if(newEmpty >= 0 && newEmpty < b.size && e >= 0 && e < b.size && b(newEmpty) == Toad){
      Some(new PuzzleState(b.slice(0, e) ++
          Vector(Toad, b(e+1) , Empty) ++
          b.slice(e + 3, b.size), newEmpty
      ))
    }
    else {
      None
    }
  }

  override def toString() = {
    val cellStrings: Vector[String] = board.map(_.toString)
    val puzzleStateString: String = cellStrings.reduceLeft(_ ++ "|" ++ _)
    val result: String = "[" ++ puzzleStateString ++ "]"
    result
  }

  def getCell(loc: Int) = {
    board(loc)
  }

}

/**
  * Companion object for the [[PuzzleState]] class, provides a public constructor.
  */
object PuzzleState {

  /**
    * Case class for case objects to represent the possible contents of a
    * cell in the puzzle.
    */
  sealed abstract class Cell {
    override def toString(): String = {
        this match {
            case Frog ⇒ "F"
            case Toad ⇒ "T"
            case Empty ⇒ " "
        }
    }
  }

  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell

  /**
    * Construct a [[PuzzleState]] object in the initial state for a
    * puzzle with specified numbers of frogs and toads.
    *
    * @param frogs number of frogs to place on the left of the [[PuzzleState]]
    * to be constructed
    * @param toads number of toads top place on the right of the [[PuzzleState]]
    * to be constructed
    */
  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }

  /**
    * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
    * [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[PuzzleState]] objects passed through in the transit from
    * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
    * is found.
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    if(start.isTerminalState()) {
      return Seq(start)
    }
    if(start.jumpFromLeft() != None){
      val jumpLeft = solve(start.jumpFromLeft().get)
      if (jumpLeft != Seq()) {
        return solve(start.jumpFromLeft().get) ++ Seq(start) 
      }
    }

    if(start.jumpFromRight() != None){
      val jumpRight = solve(start.jumpFromRight().get)
      if (jumpRight != Seq()) {
        return solve(start.jumpFromRight().get) ++ Seq(start) 
      }
    }

    if(start.slideFromLeft() != None){
      val slideLeft = solve(start.slideFromLeft().get)
      if (slideLeft != Seq()) {
        return solve(start.slideFromLeft().get) ++ Seq(start) 
      }
    }

    if(start.slideFromRight() != None){
      val slideRight = solve(start.slideFromRight().get)
      if (slideRight != Seq()) {
        return solve(start.slideFromRight().get) ++ Seq(start) 
      }
    }
    return Seq()

  }

  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each state in that solution as
    * an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    val solution = solve(start).reverse

    if(solution(0).isTerminalState){
      Seq(createState(solution(0), 0))
    }
    else if(solution(0).isInitialState){
      animate(solution(1)) :+ createState(solution(0), 0)
    }
    else {
      animate(solution(1)) :+ createState(solution(0), 0)
    }
  }

  /**
    * Helper method for animate that draws the current PuzzleState
    *
    * @param current the [[PuzzleState]]
    * @param idx the current cell in the [[PuzzleState]] being drawn
    * @return a single [[Image]] object depicting the given PuzzleState
    */
  def createState(current: PuzzleState, idx: Int): Image = {
    if (idx ==  current.size){
      Image.rectangle(0, 0)
    }
    else {
      current.getCell(idx) match {
        case Frog => Image.rectangle(30,30).fillColor(Color.lightGreen).beside(createState(current, idx+1))
        case Toad => Image.rectangle(30,30).fillColor(Color.darkGreen).beside(createState(current, idx+1)) 
        case Empty => Image.rectangle(30,30).fillColor(Color.white).beside(createState(current, idx+1)) 
      }
    }
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    * 
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(PuzzleState(frogs, toads))
}
