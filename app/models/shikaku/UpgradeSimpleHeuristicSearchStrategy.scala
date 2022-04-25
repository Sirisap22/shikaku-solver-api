package shikaku

import scala.collection.mutable.Stack

class UpgradeSimpleHeuristicSearchStrategy extends SolveStrategy {
      override def solve(numberOfRows: Int, numberOfCols: Int, _clues: Vector[Clue]): Vector[Square] = {
      // Space = board game, Clue = current clue, Square = block that place the round[state]

      // heuristic
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
        // heuristic sort มากไปน้อย จะได้ไม่ต้อง reverse อีกรอบ
        val sortedPossibleSquares = possibleSquares.sortBy((square) => this.heuristic(clues.slice(currentClueIdx, clues.size), square))(Ordering[Float].reverse)

        sortedPossibleSquares.foreach((square) => {
          stack.push((this.placeSquare(currentState, square), currentClueIdx + 1, currentPlacedSquares :+ square))
        })
      }

      return Vector[Square]()
  }

  def heuristic(clues: Vector[Clue], square: Square): Float = {
    val surroundingPoints = square.getSurroundingPonts()
    val pointsInClue = clues.map(clues => clues.position)
    var sum = 0f
    surroundingPoints.foreach((point) => {
      if (pointsInClue.contains(point)) sum += 1f
    });
    return sum/surroundingPoints.size.toFloat
  }
}

object UpgradeSimpleHeuristicSearchStrategy  {
  def main(args: Array[String]): Unit = {
//     val input = Vector((1, 0, 5),
//     (1, 1, 3),
//     (3, 1, 2),
//     (3, 2, 4),
//     (5, 2, 9),
//     (5, 3, 4),
//     (7, 3, 2),
//     (7, 4, 6),
//     (1, 5, 3),
//     (2, 5, 8),
//     (1, 6, 4),
//     (2, 6, 4),
//     (0, 9, 8),
//     (4, 7, 4),
//     (4, 8, 4),
//     (5, 7, 2),
//     (5, 8, 2),
//     (7, 9, 6),
//     (8, 9, 8),
//     (9, 0, 10),
//     (10, 0, 4),
//     (10, 5, 3),
//     (10, 6, 4),
//     (12, 6, 4),
//     (12, 7, 6),
//     (14, 7, 6),
//     (14, 8, 6),
//     (16, 8, 6),
//     (16, 9, 2),
//     (17, 0, 4),
//     (12, 1, 2),
//     (13, 1, 2),
//     (12, 2, 9),
//     (13, 2, 6),
//     (15, 3, 6),
//     (16, 3, 6),
//     (15, 4, 2),
//     (16, 4, 4)
// )
    val input = Vector((0, 0, 9),
    (4, 0, 12),
    (7, 0, 5),
    (9, 2, 6),
    (4, 3, 8),
    (0, 3, 8),
    (2, 3, 6),
    (0, 7, 4),
    (2, 9, 3),
    (5, 9, 9),
    (9, 9, 4),
    (9, 6, 12),
    (7, 6, 8),
    (5, 6, 6)
)
//     val input = Vector((0, 0, 4),
//     (6, 0, 9),
//     (8, 0, 6),
//     (7, 1, 6),
//     (9, 1, 9),
//     (16, 0, 4),
//     (17, 1, 4),
//     (14, 1, 6),
//     (15, 2, 2),
//     (12, 2, 4),
//     (13, 3, 8),
//     (6, 3, 4),
//     (3, 2, 4),
//     (1, 2, 6),
//     (2, 3, 4),
//     (1, 4, 3),
//     (7, 4, 8),
//     (9, 4, 4),
//     (8, 5, 6),
//     (10, 5, 4),
//     (11, 6, 6),
//     (16, 5, 3),
//     (15, 6, 4),
//     (16, 7, 6),
//     (14, 7, 6),
//     (17, 9, 6),
//     (10, 8, 4),
//     (11, 9, 2),
//     (8, 8, 4),
//     (9, 9, 6),
//     (0, 8, 6),
//     (1, 9, 2),
//     (2, 7, 3),
//     (3, 8, 4),
//     (5, 7, 4),
//     (4, 6, 9)
// )
    val clues: Vector[Clue] = for (ele <- input) yield Clue(Coord(ele._1 , ele._2), ele._3)
    val sh = new UpgradeSimpleHeuristicSearchStrategy()
    
    val time = Utils.averageRuntimeInNanoSec(1) {

      val ans = sh.solve(10, 10, clues)
      var state = Array.ofDim[Int](10, 10)
      var symbol = 1
      for (square <- ans){
        state = sh.placeSquare(state, square, symbol)
        symbol += 1
      }
      val s1 = for (row <- state) yield for (ele <- row) yield f"$ele%02d"
      s1.map(_.mkString(" ")).zipWithIndex.foreach{
        case (s, idx) => println(s"row $idx - $s")
      }
      Utils.memory()
    } 
    println(s"Runtime: ${time/1000000} ms")

  }
}
