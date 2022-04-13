package shikaku

import scala.collection.mutable.Stack

class LegacyHeuristicSearchStrategy extends SolveStrategy {
  override def solve(numberOfRows: Int, numberOfCols: Int, _clues: Vector[Clue]): Vector[Square] = {
      // Space = board game, Clue = current clue, Square = block that place the round[state]
      val clues = _clues.sortBy(x => x.size)(Ordering[Int].reverse)
      val stack = new Stack[(Space, Int, Vector[Square])]()

      val initialState = Array.ofDim[Int](numberOfRows, numberOfCols)
      val initialClueIdx = 0
      val placedSquares = Vector[Square]()

      stack.push((initialState, initialClueIdx, placedSquares))

      while (!stack.isEmpty) {
        val (currentState, currentClueIdx, currentPlacedSquares) = stack.pop()

        // check if goalstate
        if (this.isGoalState(currentState)) {
          return currentPlacedSquares
        }

        val currentClue = clues(currentClueIdx)

        // go next state
        // gen childs
        val shapes = this.squareShapeCombinations(currentClue.size)
        val possibleSquares = this.possibleSquareCombinations(currentState, currentClue.position, shapes, clues)

        // heuristic
        val sortedPossibleSquares = possibleSquares.sortBy(square => {
          this.heuristic(numberOfRows, numberOfCols, square, placedSquares, clues.drop(currentClueIdx+1))
        })(Ordering[Double].reverse)

        // sort มากไปน้อย จะได้ไม่ต้อง reverse อีกรอบ

        sortedPossibleSquares.foreach((square) => {
          stack.push((this.placeSquare(currentState, square), currentClueIdx + 1, currentPlacedSquares :+ square))
        })
      }

      return Vector[Square]()
    }
  
  def heuristic(m: Int, n: Int, square: Square, placedSquares: Vector[Square], notPlacedClues: Vector[Clue]): Double = {
    val squareCentroid = square.getCentroid()
    val complementManhattan = (placedSquares.size + notPlacedClues.size) * (m + n - 2)
    val sumPlacedSquaresCentroid = placedSquares.map(placedSquare => {
      this.manhattan(squareCentroid, placedSquare.getCentroid())
    }).sum
    val sumNotPlacedClues = notPlacedClues.map(clue => {
      this.manhattan(squareCentroid, Centroid(clue.position.x.toDouble, clue.position.y.toDouble))
    }).sum

    return complementManhattan - (sumPlacedSquaresCentroid + sumNotPlacedClues)
  }
  
  def manhattan(p1: Centroid, p2: Centroid): Double = (p1.x - p2.x).abs + (p1.y - p2.y).abs

}

object LegacyHeuristicSearchStrategy {
  def main(args: Array[String]) = {
    val input = Vector(
(0, 0, 5),
(6, 0, 8),
(5, 0, 20),
(6, 3, 17),
(10, 3, 5),
(14, 0, 2),
(7, 3, 2),
(16, 0, 4),
(16, 2, 4),
(14, 2, 2),
(14, 1, 2),
(14, 3, 17),
(18, 3, 17),
(15, 3, 17),
(10, 11, 8),
(9, 3, 17),
(16, 1, 4),
(13, 11, 9),
(16, 3, 17),
(10, 19, 3),
(0, 1, 5),
(19, 3, 17),
(12, 11, 8),
(11, 11, 8),
(6, 1, 8),
(13, 3, 8),
(17, 3, 17),
(6, 2, 8),
(12, 3, 8),
(7, 6, 14),
(7, 5, 2),
(0, 2, 12),
(10, 8, 3),
(7, 4, 2),
(11, 3, 5),
(8, 6, 14),
(11, 8, 3),
(2, 2, 18),
(0, 14, 4),
(0, 18, 2),
(1, 14, 4),
(3, 2, 15),
(3, 17, 3),
(0, 19, 2),
(4, 2, 15),
(1, 2, 12),
(4, 17, 3)
)
  //   val clues: Vector[Clue] = for (ele <- input) yield Clue(Coord(ele._1 , ele._2), ele._3)
  //   // println(clues.sortBy(x => -x.size))
  // }
    val clues: Vector[Clue] = for (ele <- input) yield Clue(Coord(ele._1 , ele._2), ele._3)
    val ht = new LegacyHeuristicSearchStrategy()
    val time = Utils.averageRuntimeInNanoSec(1) {
      val ans = ht.solve(20, 20, clues)
      var state = Array.ofDim[Int](20, 20)
      var symbol = 1
      for (square <- ans){
        state = ht.placeSquare(state, square, symbol)
        symbol += 1
      }
      val s1 = for (row <- state) yield for (ele <- row) yield f"$ele%02d"
      s1.map(_.mkString(" ")).zipWithIndex.foreach{
        case (s, idx) => println(s"row $idx - $s")
      }

      // Utils.memory()
    }

    println(s"Runtime: ${time/1000000} ms")
  }
}
