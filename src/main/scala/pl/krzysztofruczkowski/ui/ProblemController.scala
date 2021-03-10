package pl.krzysztofruczkowski.ui

import java.util.concurrent.Executors

import pl.krzysztofruczkowski.plateproblem.Direction._
import pl.krzysztofruczkowski._
import pl.krzysztofruczkowski.plateproblem.{Const, PlateProblem, PlateSolution, RandomMutationPlateProblemOptimizer, Segment, StaticData}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Button
import scalafx.scene.paint.Color._
import scalafxml.core.macros.sfxml

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Random

@sfxml
class ProblemController(val plateCanvas: Canvas, val iterButton: Button) {
  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val selectedProblem: PlateProblem = StaticData.selectedProblem.get
  val unit = 30
  val margin = 300
  val baseCircleUnit = 3
  val bigCircleUnit = 11
  val (w, h) = (selectedProblem.width - 1, selectedProblem.height - 1)
  val allPoints = for (a_ <- 0 to w; b_ <- 0 to h) yield (a_, b_)
  val allCanvasPoints = allPoints.map(p => (p._1 * unit + margin / 2, p._2 * unit + margin / 2))
  val pointToCanvasPointMap = allPoints zip allCanvasPoints.map(x => (x._1.toDouble, x._2.toDouble)) toMap
  val colors = List(Red, DarkOrange, Violet, Black, White, Cyan, Yellow, Brown, Chocolate, Wheat, Aqua)

  val infiniteColors = LazyList.continually(colors).flatten

  plateCanvas.width = w * unit + margin
  plateCanvas.height = h * unit + margin

  val gc: GraphicsContext = plateCanvas.graphicsContext2D

  def drawProblem(): Unit = {
    // fill with green
    gc.fill = Green
    gc.fillRect(0, 0, plateCanvas.width.value, plateCanvas.height.value)

    // draw pins
    gc.stroke = Black
    gc.lineWidth = 1
    allCanvasPoints.foreach(t =>
      gc.strokeOval(t._1, t._2, baseCircleUnit, baseCircleUnit)
    )

    selectedProblem.pairs zip infiniteColors foreach(x => {
      val (points, color) = x
      val (p1, p2) = (points._1.toTuple, points._2.toTuple)
      val cp1 = pointToCanvasPointMap(p1)
      val cp2 = pointToCanvasPointMap(p2)

      gc.fill = color
      gc.fillOval(cp1._1 - bigCircleUnit / 2, cp1._2 - bigCircleUnit / 2, bigCircleUnit, bigCircleUnit)
      gc.fillOval(cp2._1 - bigCircleUnit / 2, cp2._2 - bigCircleUnit / 2, bigCircleUnit, bigCircleUnit)
    })
  }

  def drawSolution(plateSolution: PlateSolution): Unit = {
    (selectedProblem.pairs map(_._1) lazyZip plateSolution.paths lazyZip infiniteColors).toList.foreach { args =>
      val (startPoint, path, color) = args
      var (x, y) = pointToCanvasPointMap(startPoint.x, startPoint.y)
      gc.fill = color
      gc.lineWidth = 4
      gc.stroke = color
      gc.beginPath()
      gc.moveTo(x, y)
      path.segments.foreach { a =>
        a match {
          case Segment(Right, value) => x += value * unit
          case Segment(Left, value) => x -= value * unit
          case Segment(Up, value) => y += value * unit
          case Segment(Down, value) => y -= value * unit
        }
        gc.lineTo(x, y)
      }
      gc.strokePath()
    }
  }
  def reset(): Unit = {
    drawProblem()
  }
  reset()

  val po = new RandomMutationPlateProblemOptimizer(selectedProblem)
  Future {
    drawSolution(po.best)
  }
  def iterate(): Unit = {
    po.iterate()
    if(po.iteration % Const.PROBLEM_CONTROLLER_REDRAW_EVERY == 0) {
        reset()
        drawSolution(po.last)
    }
  }

  val iterAction  = () => {
      Future {
        for (_ <- 1 to Const.PROBLEM_CONTROLLER_NUMBER_OF_ITERATIONS) {
          iterate()
        }
        reset()
        drawSolution(po.best)
      }
    }
  val random = new Random()
  val randAction = () => {
    Future {
      reset()
      drawSolution(selectedProblem.generateRandomSolution(random))
    }
  }
    iterButton.onAction = _ => iterAction();
}
