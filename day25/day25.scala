import io.Source
import scala.collection.mutable.HashMap
import scala.util.Using

def readGraph(filename: String) : Map[String, Set[String]] = {
  Using(Source.fromFile(filename)) {
    _.getLines.toList
  }.get.foldLeft(HashMap[String, Set[String]]()) { (acc, line) =>
    val Array(src, trg) = line.split(": ")
    val targets = trg.split(" ").toSet
    targets.foreach {t => acc.updateWith(t) { v =>
      v match
        case Some(l) => Some(l.union(Set(src)))
        case None => Some(Set(src))
    }}
    acc.updateWith(src) { v =>
      v match
        case Some(l) => Some(l.union(targets))
        case None => Some(targets)
    }
    acc
  }.toMap
}

def solvePart1(graph: Map[String, Set[String]]) = {

  def helper(comp1: Set[String], comp2: Set[String]): Int = {
    def countLinks() : Int =
      comp1.foldLeft(Set.empty) { (acc, node) =>
        acc ++ graph(node).intersect(comp2).map {(_, node)}
      }.size

    return if countLinks() == 3 then
      comp1.size * comp2.size
    else
      val node = comp1.maxBy { n => graph(n).intersect(comp2).size }
      helper(comp1 - node, comp2 + node)
  }

  println(helper(graph.keySet, Set.empty))
}


@main def main(filename: String) = {
  val graph = readGraph(filename)
  solvePart1(readGraph(filename))
}
