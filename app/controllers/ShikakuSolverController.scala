package controllers

import play.api.mvc._
import play.api._
import play.api.libs.json._
import javax.inject._
import models.GameState
import shikaku.Clue
import shikaku.Square
import shikaku.Coord
import shikaku.Solver
import shikaku.DepthFirstSearchStrategy
import shikaku.BreathFirstSearchStrategy

@Singleton
class ShikakuSolverController @Inject()(val controllerComponents: ControllerComponents) 
extends BaseController {
  implicit val coord = Json.format[Coord]
  implicit val clue = Json.format[Clue]
  implicit val square = Json.format[Square]
  implicit val gameState = Json.format[GameState]

  def getSolutionByDepthFirstSearch(): Action[AnyContent] = Action { implicit request => 
    val content = request.body
    val jsonObject = content.asJson
    val game: Option[GameState] = jsonObject.flatMap(Json.fromJson[GameState](_).asOpt)

    game match {
      case Some(newGame) => {
        val input = game.get
        val depthSolver = new Solver(input.numberOfRows, input.numberOfCols, input.clues, new DepthFirstSearchStrategy())
        val solution = depthSolver.solve()
        Created(Json.toJson(solution))
      }
      case None => BadRequest
    }

  }

  def getSolutionByBreathFirstSearch(): Action[AnyContent] = Action { implicit request => 
    val content = request.body
    val jsonObject = content.asJson
    val game: Option[GameState] = jsonObject.flatMap(Json.fromJson[GameState](_).asOpt)

    game match {
      case Some(newGame) => {
        val input = game.get
        val breathSolver = new Solver(input.numberOfRows, input.numberOfCols, input.clues, new BreathFirstSearchStrategy())
        val solution = breathSolver.solve()
        Created(Json.toJson(solution))
      }
      case None => BadRequest
    }

  }
  
}
