package pl.krzysztofruczkowski.ui

import pl.krzysztofruczkowski.{PlateProblem, StaticData}
import scalafx.scene.SnapshotParameters
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color._
import scalafxml.core.macros.sfxml

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
  val pointToCanvasPointMap = allPoints zip allCanvasPoints toMap
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
    val p = x._1
    val p1 = (p._1.x, p._1.y)
    val p2 = (p._2.x, p._2.y)
    val cp1 = pointToCanvasPointMap(p1)
    val cp2 = pointToCanvasPointMap(p2)

    gc.fill = x._2
    gc.fillOval(cp1._1, cp1._2, bigCircleUnit, bigCircleUnit)
    gc.fillOval(cp2._1, cp2._2, bigCircleUnit, bigCircleUnit)
  })

//  val snapshot = plateCanvas.snapshot(new SnapshotParameters(), null)
//
//  def reset(): Unit = {
//    gc.drawImage(snapshot)
//  }
}
