package pl.krzysztofruczkowski.plateproblem

import pl.krzysztofruczkowski.plateproblem
import pl.krzysztofruczkowski.plateproblem.Direction._

import scala.util.Random

case class Path(segments: List[Segment]) {
  def mutate(implicit random: Random): Path = {
    val randomSegmentIndex = random.nextInt(segments.length)
    val forward = random.nextBoolean()
    val multiplier = random.between(Const.MUTATION_MULTIPLIER_MIN, Const.MUTATION_MULTIPLIER_MAX + 1)
    mutate(randomSegmentIndex, forward, multiplier)
  }

  def mutate(segmentIndex: Int, forward: Boolean, multiplier: Int): Path = {
    val segment = segments(segmentIndex)
    val forwardM = if(forward) multiplier else -multiplier

    val dir = if(isHorizontal(segment.direction)) Up else Right
    val sa = plateproblem.Segment(dir, forwardM)
    val sb = plateproblem.Segment(dir, -forwardM)

    // insert segments
    val newSegments: List[Segment] =
      segments.take(segmentIndex) :::
        sa :: segment :: sb ::
        segments.slice(segmentIndex+1, segments.length)

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

    // run again if not reduced
    if(newSegmentsFixed.exists(_.length == 0))
      Path(newSegmentsFixed).fixed()
    else
      Path(newSegmentsFixed)
  }

}
