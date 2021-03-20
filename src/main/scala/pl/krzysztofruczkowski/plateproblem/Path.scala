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

  def mutate_b(implicit random: Random): Path = {
    val randomSegmentIndex = random.nextInt(segments.length)
    val forward = random.nextBoolean()
    val multiplier = random.between(Const.MUTATION_MULTIPLIER_MIN, Const.MUTATION_MULTIPLIER_MAX + 1)
    val slen = segments(randomSegmentIndex).length
    val lengths = Util.splitNumberInThree(slen)

    //do not replace index with 0, as mutation will have no effect then
    val replaceIndex = random.shuffle(List(lengths._1, lengths._2, lengths._3).zipWithIndex.filter(p => p._1 != 0)).head._2
    mutate_b(randomSegmentIndex, lengths, replaceIndex, forward, multiplier)
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

  def mutate_b(segmentIndex: Int, lengths: (Int, Int, Int), replaceIndex: Int, forward: Boolean, multiplier: Int): Path = {
//    println(segments)
    val segment = segments(segmentIndex)
    def gen_s(l: Int) = Segment(segment.direction, l)

    val forwardM = if(forward) multiplier else -multiplier

    val dir = if(isHorizontal(segment.direction)) Up else Right
    val sa = plateproblem.Segment(dir, forwardM)
    val sb = plateproblem.Segment(dir, -forwardM)

    val s1 = gen_s(lengths._1)
    val s2 = gen_s(lengths._2)
    val s3 = gen_s(lengths._3)

    val newInnerSegments = List(s1, s2, s3)
    val newInnerSegmentsUpdated =
      newInnerSegments.take(replaceIndex) :::
        sa :: newInnerSegments(replaceIndex) :: sb ::
        newInnerSegments.slice(replaceIndex + 1, newInnerSegments.length)

//    println(newInnerSegments)
//    println(newInnerSegmentsUpdated)

    // insert segments
    val newSegments: List[Segment] =
      segments.take(segmentIndex) :::
        newInnerSegmentsUpdated :::
        segments.slice(segmentIndex+1, segments.length)

//    println(newSegments)

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
