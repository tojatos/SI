package pl.krzysztofruczkowski
import pl.krzysztofruczkowski.Direction._

import scala.util.Random

class RandomPlateProblemOptimizer(plateProblem: PlateProblem, seed: Long = new Random().nextLong()) extends PlateProblemOptimizer(plateProblem) {
  val random = new Random(seed)
  println(s"Used seed: $seed")

  var best: PlateSolution = plateProblem.getTrivialSolution
  var last: PlateSolution = best
  var bestFitness: Double = plateProblem.fitness(best)

  println(s"Initial fitness: $bestFitness")

  def mutate(plateSolution: PlateSolution): PlateSolution = {
    val randomPathIndex = random.nextInt(plateSolution.paths.length)
    val randomPath: Path = plateSolution.paths(randomPathIndex)
//    val randomDirection: Direction = Direction(random.nextInt(Direction.maxId))
    val segments = randomPath.segments
    val randomSegmentIndex = random.nextInt(segments.length)
//    val step = 1

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
//    println(sb, sa)
//    println(newSb, newSa)
    // replace segments
    val newSegments: List[Segment] =
      (segments.take(randomSegmentIndex - 1) :::
        newSb :: segment :: newSa ::
        segments.slice(randomSegmentIndex+2, segments.length)).filter(_.length != 0)
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

    val newSegmentsFixed = newSegmentsReduced.map(s => if (s.length > 0) s else Segment(Direction.opposing(s.direction), -s.length))

    val newPaths = plateSolution.paths.updated(randomPathIndex, Path(newSegmentsFixed))
    PlateSolution(newPaths)
  }

  override def getBest = best

  var iteration = 0
  override def iterate(): Unit = {
    iteration += 1
    if(iteration % 200 == 0) last = best //reset to best every 100 times
    val newSolution: PlateSolution = mutate(last)
    last = newSolution
//    println(newSolution)
    val newFitness = plateProblem.fitness(newSolution)
//    println(newFitness)
    if (newFitness > bestFitness) {
      best = newSolution
      bestFitness = newFitness
      println("!!!!!!!! " + best)
      println("!!!!!!!! " + bestFitness)
    }
  }
}
