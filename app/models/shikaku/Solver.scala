package shikaku

case class Coord(val x: Int = 0, val y: Int = 0)
case class SquareShape(val width: Int = 1, val height: Int = 1)
case class Boundary(val top: Int, val bottom: Int, val left: Int, val right: Int)
case class Clue(val position: Coord, val size: Int)

class Solver(val numberOfRows: Int, val numberOfCols: Int, val clues: Vector[Clue], var solveStrategy: SolveStrategy) {
  type Space = Array[Array[Int]]

  def setStrategy(solveStrategy: SolveStrategy): Unit = {
    this.solveStrategy = solveStrategy
  }

  def solve(): Vector[Square] = {
    // return Vector[Square]() if no valid answer (clues don't match up)
    if (!this.isValidClues(numberOfRows, numberOfCols, clues)) {
      return Vector[Square]()
    }

    this.solveStrategy.solve(numberOfRows, numberOfCols, clues)
  }

  def isValidClues(numberOfRows: Int, numberOfCols: Int, clues: Vector[Clue]): Boolean = {
    var sum = 0
    for (c <- clues) {
      sum += c.size
    }
    if (numberOfRows * numberOfCols != sum) {
      return false
    }
    true
  }

  def timeIt(n: Int): Unit = {
    var answer: Vector[Square] = this.solve()

    val time = Utils.averageRuntimeInNanoSec(n) {
      this.solve()
    }

    val state = this.updateState(Array.ofDim(this.numberOfRows, this.numberOfCols), this.solveStrategy, answer)
    this.printState(state)

    println(s"Average Runtime($n times): ${time/1000000} ms")
  }

  def updateState(state: Space, solveStrategy: SolveStrategy, squares: Vector[Square]): Space = {
    var newState = solveStrategy.deepCopySpace(state)
    var symbol = 1
    for (square <- squares){
      newState = solveStrategy.placeSquare(newState, square, symbol)
      symbol += 1
    }
    newState
  }

  def printState(state: Space): Unit = {
    val s1 = for (row <- state) yield for (ele <- row) yield f"$ele%02d"
    s1.map(_.mkString(" ")).zipWithIndex.foreach{
      case (s, idx) => println(s"row $idx - $s")
    }
  }
}
