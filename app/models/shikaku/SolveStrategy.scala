package shikaku

import scala.collection.immutable.HashMap

trait SolveStrategy {
  type Space = Array[Array[Int]]
  var squareShapesCache: HashMap[Int, Vector[SquareShape]] = new HashMap[Int, Vector[SquareShape]]()

  def solve(numberOfRows: Int, numberOfCols: Int, clues: Vector[Clue]): Vector[Square]

  def isGoalState(state: Space): Boolean = {
    for (row <- state if row.contains(0)) {
      return false
    }
    true
  }

  def placeSquare(state: Space, square: Square, symbol: Int=1): Space = {
    val newState = this.deepCopySpace(state)
    square.getAllPoints().foreach((position) => newState(position.y)(position.x) = symbol)
    newState
  }

  def deepCopySpace(state: Space): Space = {
    state.map(_.clone)
  }

  def squareShapeCombinations(size: Int): Vector[SquareShape] = {
    if (squareShapesCache.contains(size)) {
      return this.squareShapesCache(size)
    }
    if (size == 1) {
      return Vector[SquareShape](SquareShape(1, 1))
    }

    var squareShapes = Vector[SquareShape]()
    for (
      i <- 1 to size;
      j <- 1 to size/2
      if (i * j == size)
    ) {
      squareShapes = squareShapes :+ SquareShape(i, j) 
      if (!(i == j)) squareShapes = squareShapes :+ SquareShape(j, i)
    }

    this.squareShapesCache = this.squareShapesCache + (size -> squareShapes)
    squareShapes
  }

  def possibleSquareCombinations(state: Space, origin: Coord, squareShapes: Vector[SquareShape], clues: Vector[Clue]): Vector[Square] = {
    // define boundaries
    val boundary = this.getBoundary(state)

    // generate all square Combinations
    val squares = this.squareCombinations(state, origin, squareShapes)

    // eliminate all impossible square
    val possibleSquares = this.eliminateImpossibleSquares(state, origin, clues, squares)

    possibleSquares
  }

  def squareCombinations(state: Space, origin: Coord, squareShapes: Vector[SquareShape]): Vector[Square] = {
    var squares = Vector[Square]()
    for (shape <- squareShapes) {
      // + 1 because the origin has to be inside the square
      val topLeft = Coord(origin.x - shape.width + 1, origin.y - shape.height + 1)
      val bottomRight = origin

      // - 1 because of + 1 above make the width and height + 1 in size so - 1 to equalize 
      val subSquares = for (
      x <- topLeft.x to bottomRight.x;
      y <- topLeft.y to bottomRight.y
      ) yield new Square(Coord(x, y), Coord(x + shape.width - 1, y + shape.height - 1))

      squares = squares ++ subSquares
    }

    squares
  }

  def eliminateImpossibleSquares(state: Space, origin: Coord, clues: Vector[Clue], squares: Vector[Square]): Vector[Square] = {
    val possibleSquares = squares.filter((square) => this.isPossibleSquare(state, origin, square, clues))
    possibleSquares
  }

  def isPossibleSquare(state: Space, origin: Coord, square: Square, clues: Vector[Clue]): Boolean = { 
    if (square.isSquareOutOfBound(this.getBoundary(state))) {
      return false
    }

    for (
    clue <- clues 
    if clue.position != origin
    if square.isPointIntersect(clue.position)
    ) {
      return false 
    }
    
    if (square.isSquareOccupied(state)) {
      return false
    }

    return true
  }
  
  def getBoundary(state: Space): Boundary = {
    Boundary(0, state.size - 1, 0, state(0).size - 1)
  }

}  
