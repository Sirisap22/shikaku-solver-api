package controllers

import play.api.mvc._
import play.api._
import play.api.libs.json._
import javax.inject._
import models._
import shikaku.Clue
import shikaku.Square
import shikaku.Coord
import shikaku.Solver
import shikaku.DepthFirstSearchStrategy
import shikaku.BreathFirstSearchStrategy
import shikaku.Utils

@Singleton
class ShikakuSolverController @Inject()(val controllerComponents: ControllerComponents) 
extends BaseController {
  implicit val coord = Json.format[Coord]
  implicit val clue = Json.format[Clue]
  implicit val square = Json.format[Square]
  implicit val gameState = Json.format[GameState]
  implicit val configTest = Json.format[ConfigTest]
  implicit val shikakutResults = Json.format[ShikakuResults]

  def getSolutionByDepthFirstSearch(): Action[AnyContent] = Action { implicit request => 
    val content = request.body
    val jsonObject = content.asJson
    val game: Option[GameState] = jsonObject.flatMap(Json.fromJson[GameState](_).asOpt)

    game match {
      case Some(newGame) => {
        val input = newGame
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
        val input = newGame
        val breathSolver = new Solver(input.numberOfRows, input.numberOfCols, input.clues, new BreathFirstSearchStrategy())
        val solution = breathSolver.solve()
        Created(Json.toJson(solution))
      }
      case None => BadRequest
    }

  }

  def testDepthFirstSearch(): Action[AnyContent] = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val config: Option[ConfigTest] = jsonObject.flatMap(Json.fromJson[ConfigTest](_).asOpt)

    config match {
      case Some(newConfig) => {
        val input = newConfig.gameState

        val (time, memory) = Utils.averageUsedMemory(newConfig.n, 1024) {
          val solver = new Solver(input.numberOfRows, input.numberOfCols, input.clues, new DepthFirstSearchStrategy())
          val res = Utils.averageRuntimeInMilliSec(newConfig.n) {
            solver.solve()
          }
          res
        }
        
        Created(Json.toJson(ShikakuResults(time, memory, memory/1024)))
      }
      case None => BadRequest
    }
  }

  def testBreathFirstSearch(): Action[AnyContent] = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val config: Option[ConfigTest] = jsonObject.flatMap(Json.fromJson[ConfigTest](_).asOpt)

    config match {
      case Some(newConfig) => {
         val input = newConfig.gameState

        val (time, memory) = Utils.averageUsedMemory(newConfig.n, 1024) {
          val solver = new Solver(input.numberOfRows, input.numberOfCols, input.clues, new BreathFirstSearchStrategy())
          val res = Utils.averageRuntimeInMilliSec(newConfig.n) {
            solver.solve()
          }
          res
        }
        
        Created(Json.toJson(ShikakuResults(time, memory, memory/1024)))
      }
      case None => BadRequest
    }
  }
  
}
