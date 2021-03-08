package pl.krzysztofruczkowski.ui

import java.util.concurrent.Executors

import pl.krzysztofruczkowski.Direction._
import pl.krzysztofruczkowski.{PlateProblem, PlateSolution, Segment, StaticData}
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color._
import scalafxml.core.macros.sfxml

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

@sfxml
class ProblemController(val plateCanvas: Canvas) {
  val selectedProblem: PlateProblem = StaticData.selectedProblem.get
  val unit = 30
  val margin = 100
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

  // fill with green
  gc.fill = Green
  gc.fillRect(0, 0, plateCanvas.width.value, plateCanvas.height.value)

  // draw pins
  gc.fill = Blue
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
  println(pointToCanvasPointMap)

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

  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val f = Future {
    drawSolution(selectedProblem.getTrivialSolution)
  }

//  val snapshot = plateCanvas.snapshot(new SnapshotParameters(), null)
//
//  def reset(): Unit = {
//    gc.drawImage(snapshot)
//  }
}
