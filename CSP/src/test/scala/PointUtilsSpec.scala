import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pl.krzysztofruczkowski.{BacktrackMapColoringSolver, MapColoringProblem, MapColoringSolution, PointUtils}

class PointUtilsSpec extends AnyWordSpec with Matchers {

  "PointUtils" must {
    "generate random points" in {
      val points = PointUtils.generateRandom(100, 100)
      points.toSet.size shouldEqual points.size
      points.size shouldEqual 100
      points foreach println
    }
    "detect intersection" in {
      val (x1, y1) = (1, 1)
      val (x2, y2) = (2, 2)
      val (x3, y3) = (1, 2)
      val (x4, y4) = (2, 1)
      PointUtils.intersectsOrCollinear(x1, y1, x2, y2, x3, y3, x4, y4) shouldEqual true
    }

    "not detect intersection" in {
      val (x1, y1) = (1, 1)
      val (x2, y2) = (1, 2)
      val (x3, y3) = (2, 2)
      val (x4, y4) = (2, 1)
      PointUtils.intersectsOrCollinear(x1, y1, x2, y2, x3, y3, x4, y4) shouldEqual false
    }

    "detect collinear intersection" in {
      val (x1, y1) = (1, 1)
      val (x2, y2) = (2, 2)
      val (x3, y3) = (3, 3)
      val (x4, y4) = (4, 4)
      PointUtils.intersectsOrCollinear(x1, y1, x2, y2, x3, y3, x4, y4) shouldEqual true
    }
  }
}
