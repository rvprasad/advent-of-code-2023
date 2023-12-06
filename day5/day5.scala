import io.Source
import scala.util.Using
import java.lang.Long as JLong
import math.Ordered.orderingToOrdered

case class Range(src: Long, dest: Long, len: Long)
case class Almanac(
    seeds: List[String],
    srcName2DestNameAndRanges: Map[String, (String, List[Range])]
)

def createAlmanac(filename: String): Almanac = {
  def gatherRanges( iterator: Iterator[String], rangeList: List[Range] = List.empty
  ): List[Range] = {
    if (!iterator.hasNext) return rangeList

    val currLine = iterator.next()
    if (currLine.isEmpty()) { return rangeList }

    val dest :: src :: len :: _ =
      currLine.split(" ").map(JLong.parseLong).toList: @unchecked
    gatherRanges(iterator, Range(src, dest, len) :: rangeList)
  }

  def createAlmanac(
      iterator: Iterator[String],
      almanac: Almanac = Almanac(List.empty, Map.empty)
  ): Almanac = {
    if (!iterator.hasNext) return almanac

    val currLine = iterator.next()
    currLine match {
      case l if l.startsWith("seeds:") => {
        val seeds = l.split(":")(1).strip().split(" ").toList
        createAlmanac(iterator, Almanac(seeds, almanac.srcName2DestNameAndRanges))
      }
      case l if l.endsWith("map:") => {
        val key = currLine.split(" ")(0).split("-to-")
        val ranges = gatherRanges(iterator)
        val newMap = almanac.srcName2DestNameAndRanges + (key(0) -> (key(1), ranges))
        createAlmanac(iterator, Almanac(almanac.seeds, newMap))
      }
      case _ => createAlmanac(iterator, almanac)
    }
  }

  val lines = Using(Source.fromFile(filename)) {
    _.getLines.toList
  }.get.iterator
  createAlmanac(lines.iterator)
}

def calculateLocation1(almanac: Almanac): Long = {
  def getLocation(partName: String, part: Long, almanac: Almanac): Long = {
    partName match {
      case "location" => part
      case _ => {
        val destNameAndRanges =
          almanac.srcName2DestNameAndRanges(partName)
        val range = destNameAndRanges._2.find(r => (r.src <= part) && (part < r.src + r.len))
        val newPart= range.map(r => r.dest + (part - r.src)).getOrElse(part)
        getLocation(destNameAndRanges._1, newPart, almanac)
      }
    }
  }
  almanac.seeds.map(s => getLocation("seed", s.toLong, almanac)).min
}

@main def main(filename: String) = {
  val almanac = createAlmanac(filename)
  println(calculateLocation1(almanac))
}
