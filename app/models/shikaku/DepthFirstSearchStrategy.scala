package shikaku

import scala.collection.mutable.Stack

class DepthFirstSearchStrategy extends SolveStrategy {
  def solve(numberOfRows: Int, numberOfCols: Int, clues: Vector[Clue]): Vector[Square] = {
    val stack = new Stack[(Space, Int, Vector[Square])]()

    val initialState = Array.ofDim[Int](numberOfRows, numberOfCols)
    val initialClueIdx = 0
    val placedSquares = Vector[Square]()

    stack.push((initialState, initialClueIdx, placedSquares))

    while (!stack.isEmpty) {
      val (currentState, currentClueIdx, currentPlacedSquares) = stack.pop()

      if (this.isGoalState(currentState)) {
        return currentPlacedSquares
      }

      val currentClue = clues(currentClueIdx)

      val shapes = this.squareShapeCombinations(currentClue.size)
      val possibleSquares = this.possibleSquareCombinations(currentState, currentClue.position, shapes, clues)
      possibleSquares.foreach((square) => {
        stack.push((this.placeSquare(currentState, square), currentClueIdx + 1, currentPlacedSquares :+ square))
      })
    }

    return Vector[Square]()
  }


}