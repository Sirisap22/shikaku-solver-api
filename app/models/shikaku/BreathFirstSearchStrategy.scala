package shikaku
import scala.collection.mutable.Queue

class BreathFirstSearchStrategy extends SolveStrategy {
 def solve(numberOfRows: Int, numberOfCols: Int, clues: Vector[Clue]): Vector[Square] = {
    val queue = new Queue[(Space, Int, Vector[Square])]()

    val initialState = Array.ofDim[Int](numberOfRows, numberOfCols)
    val initialClueIdx = 0
    val placedSquares = Vector[Square]()

    queue.enqueue((initialState, initialClueIdx, placedSquares))

    while (!queue.isEmpty) {
      val (currentState, currentClueIdx, currentPlacedSquares) = queue.dequeue()

      if (this.isGoalState(currentState)) {
        return currentPlacedSquares
      }

      val currentClue = clues(currentClueIdx)

      val shapes = this.squareShapeCombinations(currentClue.size)
      val possibleSquares = this.possibleSquareCombinations(currentState, currentClue.position, shapes, clues)
      possibleSquares.foreach((square) => {
        queue.enqueue((this.placeSquare(currentState, square), currentClueIdx + 1, currentPlacedSquares :+ square))
      })
    }

    return Vector[Square]()
  } 
}