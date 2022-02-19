package models

import shikaku.Clue

case class GameState(val numberOfRows: Int, val numberOfCols: Int, val clues: Vector[Clue])