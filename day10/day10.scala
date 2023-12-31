import io.Source
import scala.util.Using
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import java.util.Collection

type Node = (Int, Int)
type Graph = Map[Node, Char]
case class Sketch(graph: Graph, num_rows: Int, num_cols: Int)

def createSketch(filename: String): Sketch = {
  val lines = Using(Source.fromFile(filename)) {
    _.getLines.toList
  }.get.toList
  val graph = lines
    .map(_.toCharArray())
    .zipWithIndex
    .foldLeft(Map[Node, Char]()) { (acc, row) =>
      val (chars, row_id) = row
      chars.zipWithIndex.filter { _._1 != '.' }.foldLeft(acc) { (acc, col) =>
        val (char, col_id) = col
        acc + ((row_id, col_id) -> char)
      }
    }
  Sketch(graph, lines.size, lines.head.length)
}

def getPipeAtStartNode(node: Node, graph: Graph): Char = {
  def crosser(a: String, b: String) = for (i <- a.toCharArray; j <- b.toCharArray) yield (i, j)

  val top_tile = graph.getOrElse((node._1 - 1, node._2), '.')
  val bottom_tile = graph.getOrElse((node._1 + 1, node._2), '.')
  val left_tile = graph.getOrElse((node._1, node._2 - 1), '.')
  val right_tile = graph.getOrElse((node._1, node._2 + 1), '.')
  if (crosser("|7F", "|JL").contains((top_tile, bottom_tile))) then return '|'
  if (crosser("|7F", "-FL").contains((top_tile, left_tile))) then return 'J'
  if (crosser("|7F", "-7J").contains((top_tile, right_tile))) then return 'L'
  if (crosser("-LF", "|LJ").contains((left_tile, bottom_tile))) then return '7'
  if (crosser("-LF", "-J7").contains((left_tile, right_tile))) then return '-'
  if (crosser("|LJ", "-7J").contains((bottom_tile, right_tile))) then return 'F'
  throw IllegalStateException()
}

def getLoop(startNode: Node, graph:Graph): List[Node] = {
  def getNeighbors(node: Node): List[Node] = {
    val pipe = if graph(node) == 'S' then getPipeAtStartNode(startNode, graph) else graph(node)
    val possibleNeighbors = pipe match
      case '|' => List((node._1 - 1, node._2), (node._1 + 1, node._2))
      case '-' => List((node._1, node._2 - 1), (node._1, node._2 + 1))
      case 'L' => List((node._1 - 1, node._2), (node._1, node._2 + 1))
      case 'J' => List((node._1 - 1, node._2), (node._1, node._2 - 1))
      case '7' => List((node._1 + 1, node._2), (node._1, node._2 - 1))
      case 'F' => List((node._1 + 1, node._2), (node._1, node._2 + 1))

    val validNeighbors = possibleNeighbors.filter(graph.contains)
    List.from(validNeighbors)
  }

  val path = ListBuffer[Node]()
  path.addOne(startNode)
  var prevNode = startNode
  var node = getNeighbors(startNode).head

  while (node != startNode)
    val neighbor = getNeighbors(node).filter{ _ != prevNode }.head
    prevNode = node
    path.addOne(node)
    node = neighbor

  path.addOne(node)
  path.toList
}

def process1(sketch: Sketch) = {
  val startNode = sketch.graph.filter { (node, char) => char == 'S' }.head._1
  println(getLoop(startNode, sketch.graph).size / 2)
}

def process2(sketch: Sketch) = {
  def isValidNode(node: Node): Boolean =
    0 <= node._1 && node._1 < sketch.num_rows && 0 <= node._2 && node._2 < sketch.num_cols

  def getNeighbors(node: Node): List[Node] =
    List((node._1 - 1, node._2), (node._1 + 1, node._2), (node._1, node._2 - 1), (node._1, node._2 + 1))

  def getContiguousNeighboringPositions(node: Node, pipe: Char): (List[Node], List[Node]) = {
    val (r, c) = node
    pipe match {
      case '|' => (List((r-1, c-1), (r, c-1), (r+1, c-1)), List((r-1, c+1), (r, c+1), (r+1, c+1)))
      case '-' => (List((r-1, c-1), (r-1, c), (r-1, c+1)), List((r+1, c-1), (r+1, c), (r+1, c+1)))
      case 'L' => (List((r-1, c+1)), List((r-1, c-1), (r, c-1), (r+1, c-1), (r+1, c), (r+1, c+1)))
      case 'J' => (List((r-1, c-1)), List((r+1, c-1), (r+1, c), (r+1, c+1), (r, c+1), (r-1, c+1)))
      case '7' => (List((r+1, c-1)), List((r+1, c+1), (r, c+1), (r-1, c+1), (r-1, c), (r-1, c-1)))
      case 'F' => (List((r+1, c+1)), List((r-1, c+1), (r-1, c), (r-1, c-1), (r, c-1), (r+1, c-1)))
    }
  }

  def gatherNodesLiningOneSideOfLoop(loop: List[Node]): Set[Node] = {
    val ret = HashSet[Node]()
    var prevGroup = List.empty[Node]
    var prevPipe = ' '
    for node <- loop do
      val pipe = if sketch.graph(node) == 'S' then getPipeAtStartNode(node, sketch.graph) else sketch.graph(node)
      val group = (prevPipe, pipe) match {
        case ('F', '7') | ('L', 'J') if prevGroup.size == 1 => { val (r, c) = prevGroup.head ; List((r, c-1)) }
        case ('7', 'F') | ('J', 'L') if prevGroup.size == 1 => { val (r, c) = prevGroup.head ; List((r, c+1)) }
        case ('7', 'J') | ('F', 'L') if prevGroup.size == 1 => { val (r, c) = prevGroup.head ; List((r-1, c)) }
        case ('J', '7') | ('L', 'F') if prevGroup.size == 1 => { val (r, c) = prevGroup.head ; List((r+1, c)) }
        case _ => {
          val (group1, group2)= getContiguousNeighboringPositions(node, pipe)
          if (group1.exists(prevGroup.contains)) then group1 else group2
        }
      }
      ret.addAll(group)
      prevGroup = group
      prevPipe = pipe
    ret.toSet
  }

  def expandGroup(group: Set[Node], nodesInLoop: Set[Node]): Set[Node] = {
    val expandedGroup = HashSet[Node]()
    val workList = HashSet.from(group)
    while (!workList.isEmpty) {
      val node = workList.head
      workList -= node
      expandedGroup.addOne(node)
      if (isValidNode(node))
        val neighbors = getNeighbors(node).filter { n => !nodesInLoop.contains(n) && !expandedGroup.contains(n) }
        workList.addAll(neighbors)
    }
    expandedGroup.toSet
  }

  def printNodes(nodes: List[Node]) = {
    for r <- 0 until sketch.num_rows; c <- 0 until sketch.num_cols do
      if (c == 0) then println("")
      if (nodes.contains((r,c))) then print(sketch.graph.getOrElse((r,c), "X")) else print(".")
    println("")
  }

  val startNode = sketch.graph.filter { (node, char) => char == 'S' }.head._1
  val loop = getLoop(startNode, sketch.graph)
  val nodesInLoop = Set.from(loop)
  val liningNodes = gatherNodesLiningOneSideOfLoop(loop).filter { !nodesInLoop.contains(_) }
  val expandedGroup = expandGroup(liningNodes, nodesInLoop)
  val validExpandedGroup = expandedGroup.filter(isValidNode)
  if (expandedGroup.exists { n => !isValidNode(n) } || liningNodes.size > nodesInLoop.size) then
    println(sketch.num_rows * sketch.num_cols - nodesInLoop.size - validExpandedGroup.size)
  else
    println(validExpandedGroup.size)
}

@main def main(filename: String) = {
  val sketch = createSketch(filename)
  process1(sketch)
  process2(sketch)
}
