package pl.krzysztofruczkowski

import pl.krzysztofruczkowski.Direction.{Down, Left, Right, Up, isHorizontal}

import scala.util.Random

case class PlateSolution(paths: List[Path]) {
  def randomMutate(random: Random): PlateSolution = {
    val randomPathIndex = random.nextInt(paths.length)
    val randomPath: Path = paths(randomPathIndex)
    val segments = randomPath.segments
    val randomSegmentIndex = random.nextInt(segments.length)

    val segmentBefore = if (randomSegmentIndex == 0) None else Some(segments(randomSegmentIndex-1))
    val segment = segments(randomSegmentIndex)
    val segmentAfter = if (randomSegmentIndex == segments.length-1) None else Some(segments(randomSegmentIndex+1))
    val forward = random.nextBoolean()
    val forwardM = if(forward) 1 else -1

    val (dir1, dir2) = if(isHorizontal(segment.direction)) (Down, Up) else (Left, Right)
    val sb = segmentBefore.getOrElse(Segment(if(forward) dir1 else dir2, 0))
    val sa = segmentAfter.getOrElse(Segment(if(forward) dir2 else dir1, 0))
    val newSb = if(sb.direction == dir2) {
      Segment(dir2, sb.length - 1 * forwardM)
    } else {
      Segment(dir1, sb.length + 1 * forwardM)
    }

    val newSa = if(sa.direction == dir1) {
      Segment(dir1, sa.length - 1 * forwardM)
    } else {
      Segment(dir2, sa.length + 1 * forwardM)
    }
    // replace segments and filter empty
    val newSegments: List[Segment] =
      (segments.take(randomSegmentIndex - 1) :::
        newSb :: segment :: newSa ::
        segments.slice(randomSegmentIndex+2, segments.length)).filter(_.length != 0)

    // after the filtering reduce segments from the right
    // example: (Segment(Left, 4), Segment(Right, 3)) => Segment(Left, 1)
    val newSegmentsReduced = newSegments.foldRight(List[Segment]())((segment, acc) => {
      if(acc.isEmpty) {
        segment :: acc
      }
      else if(segment.direction == acc.head.direction) {
        acc.updated(0, Segment(acc.head.direction, acc.head.length + segment.length))
      } else if(Direction.opposing(segment.direction) == acc.head.direction) {
        acc.updated(0, Segment(acc.head.direction, acc.head.length - segment.length))
      } else {
        segment :: acc
      }
    })

    // swap negative values after reduction
    val newSegmentsFixed = newSegmentsReduced.map(s => if (s.length > 0) s else Segment(Direction.opposing(s.direction), -s.length))

    // replace the modified path
    val newPaths = paths.updated(randomPathIndex, Path(newSegmentsFixed))
    PlateSolution(newPaths)
  }

}
