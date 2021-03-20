package pl.krzysztofruczkowski.plateproblem

import pl.krzysztofruczkowski.plateproblem
import pl.krzysztofruczkowski.plateproblem.Direction._

import scala.util.Random

case class Path(segments: List[Segment]) {
  def mutate(implicit random: Random): Path = {
    val randomSegmentIndex = random.nextInt(segments.length)

    val segmentBefore = if (randomSegmentIndex == 0) None else Some(segments(randomSegmentIndex-1))
    val segment = segments(randomSegmentIndex)
    val segmentAfter = if (randomSegmentIndex == segments.length-1) None else Some(segments(randomSegmentIndex+1))
    val forward = random.nextBoolean()
    val multiplier = random.between(Const.MUTATION_MULTIPLIER_MIN, Const.MUTATION_MULTIPLIER_MAX + 1)
    val forwardM = if(forward) multiplier else -multiplier

    val (dir1, dir2) = if(isHorizontal(segment.direction)) (Down, Up) else (Left, Right)
    val sb = segmentBefore.getOrElse(plateproblem.Segment(if(forward) dir1 else dir2, 0))
    val sa = segmentAfter.getOrElse(plateproblem.Segment(if(forward) dir2 else dir1, 0))
    val newSb = if(sb.direction == dir2) {
      plateproblem.Segment(dir2, sb.length - 1 * forwardM)
    } else {
      plateproblem.Segment(dir1, sb.length + 1 * forwardM)
    }

    val newSa = if(sa.direction == dir1) {
      plateproblem.Segment(dir1, sa.length - 1 * forwardM)
    } else {
      plateproblem.Segment(dir2, sa.length + 1 * forwardM)
    }
    // replace segments and filter empty
    val newSegments: List[Segment] =
      segments.take(randomSegmentIndex - 1) :::
        newSb :: segment :: newSa ::
        segments.slice(randomSegmentIndex+2, segments.length)

    Path(newSegments).fixed()
  }
  def fixed(): Path = {
    val segmentsWithLength = segments.filter(_.length != 0)

    // after the filtering reduce segments from the right
    // example: (Segment(Left, 4), Segment(Right, 3)) => Segment(Left, 1)
    val segmentsReduced = segmentsWithLength.foldRight(List[Segment]())((segment, acc) => {
      if(acc.isEmpty) segment :: acc
      else if(segment.direction == acc.head.direction) {
        // add if the same direction
        acc.updated(0, Segment(acc.head.direction, acc.head.length + segment.length))
      } else if(Direction.opposing(segment.direction) == acc.head.direction) {
        // subtract if opposing same direction
        acc.updated(0, Segment(acc.head.direction, acc.head.length - segment.length))
      } else {
        // rewrite if perpendicular
        segment :: acc
      }
    })

    // swap negative values after reduction
    val newSegmentsFixed = segmentsReduced.map(s => if (s.length > 0) s else plateproblem.Segment(Direction.opposing(s.direction), -s.length))

    Path(newSegmentsFixed)
  }

}
