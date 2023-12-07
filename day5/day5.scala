import io.Source
import scala.util.Using
import java.lang.Long as JLong

case class Mapping(src: Long, dest: Long, len: Long)
case class Almanac(
    seeds: List[String],
    srcName2DestNameAndMappings: Map[String, (String, List[Mapping])]
)

def createAlmanac(filename: String): Almanac = {
  def gatherMappings( iterator: Iterator[String], mappingList: List[Mapping] = List.empty
  ): List[Mapping] = {
    if (!iterator.hasNext) return mappingList

    val currLine = iterator.next()
    if (currLine.isEmpty()) { return mappingList }

    val dest :: src :: len :: _ =
      currLine.split(" ").map(JLong.parseLong).toList: @unchecked
    gatherMappings(iterator, Mapping(src, dest, len) :: mappingList)
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
        createAlmanac(iterator, Almanac(seeds, almanac.srcName2DestNameAndMappings))
      }
      case l if l.endsWith("map:") => {
        val key = currLine.split(" ")(0).split("-to-")
        val mappings = gatherMappings(iterator)
        val newMap = almanac.srcName2DestNameAndMappings + (key(0) -> (key(1), mappings))
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


def getLocation(partName: String, part: Long, almanac: Almanac): Long = {
  partName match {
    case "location" => part
    case _ => {
      val (destPartName, destPartMappings)= almanac.srcName2DestNameAndMappings(partName)
      val range = destPartMappings.find(r => (r.src <= part) && (part < r.src + r.len))
      val newPart= range.map(r => r.dest + (part - r.src)).getOrElse(part)
      getLocation(destPartName, newPart, almanac)
    }
  }
}

def calculateLocation1(almanac: Almanac): Long = {
  almanac.seeds.map(s => getLocation("seed", s.toLong, almanac)).min
}

def calculateLocation2(almanac: Almanac): Long = {
  case class Range(start: Long, len: Long)

  def getDestRanges(range: Range, mappings: List[Mapping]): List[Range] = {
    if (range.len < 1) return List.empty

    mappings.find(m => (m.src <= range.start) && (range.start < m.src + m.len)) match {
      case None => {
        val mapping = mappings.filter(_.src > range.start)
          .reduceOption((a, b) => if (a.src < b.src) { a } else { b })
        if (mapping.isEmpty || range.start + range.len <= mapping.get.src) {
          List(Range(range.start, range.len))
        } else {
          val destLen = mapping.get.src - range.start
          List(Range(range.start, destLen)) ++ getDestRanges(Range(range.start + destLen, range.len - destLen), mappings)
        }
      }
      case Some(mapping) => {
        val destStart = mapping.dest + range.start - mapping.src
        if (range.start + range.len <= mapping.src + mapping.len) {
          List(Range(destStart, range.len))
        } else {
          val destLen = mapping.src + mapping.len - range.start
          List(Range(destStart, destLen)) ++ getDestRanges(Range(range.start + destLen, range.len - destLen), mappings)
        }
      }
    }
  }

  def getLocations(partName: String, partRange: Range): List[Long] = {
    partName match {
      case "location" => List(partRange.start)
      case _ if partRange.len == 1 => List(getLocation(partName, partRange.start, almanac))
      case _ => {
        val (destPartName, mappings)= almanac.srcName2DestNameAndMappings(partName)
        getDestRanges(partRange, mappings).flatMap(getLocations(destPartName, _))
      }
    }
  }

  (0 until almanac.seeds.length by 2).map(i =>
    Range(almanac.seeds(i).toLong, almanac.seeds(i + 1).toLong)
    ).flatMap(getLocations("seed", _)).min
}

@main def main(filename: String) = {
  val almanac = createAlmanac(filename)
  println(calculateLocation1(almanac))
  println(calculateLocation2(almanac))
}
