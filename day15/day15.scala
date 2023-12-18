import io.Source
import scala.collection.mutable.LinkedHashMap
import scala.util.Using

def getSteps(filename: String): List[String] = {
  Using(Source.fromFile(filename)) {
    _.getLines.next().split(",").toList
  }.get
}

def calculateHash(str: String): Int = {
  str.foldLeft(0) { (acc, e) => ((acc + e.toInt) * 17) % 256 }
}

def part1(steps: List[String]): Int = {
  steps.map(calculateHash).sum
}

def part2(steps: List[String]): Int = {
  val boxes = Array.fill(256) { LinkedHashMap[String, Int]() }
  for step <- steps do
    val splitter = if (step.contains("=")) { "=" } else { "-" }
    val tmp1 = step.split(splitter)
    val label = tmp1(0)
    val boxId = calculateHash(label)
    val box = boxes(boxId)
    splitter match
      case "=" => box.put(label, tmp1(1).toInt)
      case "-" => box.remove(label)
  boxes.zip(Stream from 1).map { (box, boxId) => {
    box.values.zip(Stream from 1).map { (focalLength, slotId) =>
      boxId * focalLength * slotId }.sum }}.sum
}

@main def main(filename: String) = {
  val steps = getSteps(filename)
  println(part1(steps))
  println(part2(steps))
}
