import io.Source
import scala.util.Using
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap

trait Component :
  def process(pulse: Boolean, fromComponent: String): Option[Boolean]

class Null extends Component:
  var lastPulse = true

  def getLastPulse = lastPulse
  def process(pulse: Boolean, fromComponent: String): Option[Boolean] =
    if lastPulse then lastPulse = pulse
    None

class Broadcast extends Component:
  def process(pulse: Boolean, fromComponent: String): Option[Boolean] = Some(pulse)

class FlipFlop extends Component:
  var on = false

  def isOn: Boolean = on
  def process(pulse: Boolean, fromComponent: String): Option[Boolean] =
    if pulse then
      None
    else
      on = !on
      Some(on)

class Conjunction extends Component:
  var srcName2State = HashMap[String, Boolean]()

  def addSource(src: String) = srcName2State(src) = false
  def isAllHigh: Boolean = srcName2State.values.forall(identity)
  def process(pulse: Boolean, srcName: String): Option[Boolean] =
    srcName2State(srcName) = pulse
    Some(!isAllHigh)

// component name -> component and successors
type Configuration = Map[String, (Component, Array[String])]

type Accumulator = (Queue[(String, Boolean, String)], Int, Int)

def executeConfiguration(acc: Accumulator, configuration: Configuration): Accumulator = {
  val (queue, lows, highs) = acc
  if queue.isEmpty then return acc
  val newQueue = Queue[(String, Boolean, String)]()
  var lowCount = 0
  var highCount = 0
  while (!queue.isEmpty) {
    val (sourceName, pulse, targetName) = queue.dequeue()
    if configuration.contains(targetName) then
      val (targetComponent, successors) = configuration(targetName)
      val propagate = targetComponent.process(pulse, sourceName)
      if propagate.isDefined then
        val new_pulse = propagate.get
        newQueue.enqueueAll(successors.map((targetName, new_pulse, _)))
      if pulse then highCount += 1 else lowCount += 1
  }
  executeConfiguration((newQueue, lows + lowCount, highs + highCount), configuration)
}

def solvePart1(configuration: Configuration) = {
  val (_, lows, highs) =
    (0 until 1000).foldLeft((Queue[(String, Boolean, String)](), 0, 0)) { (acc, _) =>
      acc._1.enqueue(("button", false, "broadcaster")) // false is low pulse & true is high pulse
      executeConfiguration(acc, configuration)
    }
  println(s"$lows $highs ${lows * highs}")
}

def createConfig(filename: String) : Configuration = {
  val ret = HashMap[String, (Component, Array[String])]()
  val lines = Using(Source.fromFile(filename)) {
    _.getLines.toList
  }.get.toList.foreach { line =>
    val Array(src, trg) = line.replace(" ", "").split("->")
    val targets = trg.split(",")
    if src == "broadcaster" then
      ret(src) = (Broadcast(), targets)
    else
      val componentName = src.substring(1)
      src(0) match
        case '%' => ret(componentName) = (FlipFlop(), targets)
        case '&' => ret(componentName) = (Conjunction(), targets)
  }

  for (source, (_, targets)) <- ret do
    for target <- targets do
      if !ret.contains(target) then
        ret(target) = (Null(), Array.empty)
      else if ret(target)._1.isInstanceOf[Conjunction] then
        ret(target)._1.asInstanceOf[Conjunction].addSource(source)

  ret.toMap
}

def solvePart2(configuration: Configuration) = {
  val rxComponent = configuration("rx")._1.asInstanceOf[Null]

  def wrapper(acc: Accumulator, count:Int): Int = {
    acc._1.enqueue(("button", false, "broadcaster")) // false is low pulse & true is high pulse
    val updatedAcc = executeConfiguration(acc, configuration)
    if !rxComponent.getLastPulse then count else wrapper(updatedAcc, count + 1)
  }

  val acc = (Queue[(String, Boolean, String)](), 0, 0)
  println(s"${wrapper(acc, 0)}")
}

@main def main(filename: String) = {
  solvePart1(createConfig(filename))
  solvePart2(createConfig(filename))
}
