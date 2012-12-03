class LinkGraphFunctional private (
    val numberOfNodes: Int,
    val graph: IndexedSeq[IndexedSeq[Int]]) {
  
  def floydWarshall = {
    //println("Calculating distances...")
    (0 until numberOfNodes).foldLeft(graph)(floydWarshallIteration _)
      }
  
  def floydWarshallIteration(graph: IndexedSeq[IndexedSeq[Int]], k: Int) = {
    //println(s"\r$k")
    for (i <- 0 until numberOfNodes) yield {
      for (j <- 0 until numberOfNodes) yield {
        val ij = graph(i)(j)
        val ik = graph(i)(k)
        val kj = graph(k)(j)
        if (i != j && ik * kj != 0 && (ij == 0 || ik + kj < ij))
          ik + kj
        else
          ij
      }
    }
  }
}

import scala.util.Random
import scala.collection.mutable.ArraySeq

object LinkGraphFunctional {
  def apply(random: Random) = {
    val numberOfNodes = random.nextInt(500)
    println("Number of nodes: " + numberOfNodes + ".")

    def generateGraph(nodesLeftToDo: Int): List[IndexedSeq[Int]] = {
      // This is horrible, I know: code written in a hurry.

      if (0 == nodesLeftToDo) {
        List.empty
      } else {
        val weightsForCurrentNode = ArraySeq.fill(numberOfNodes) { (if (random.nextBoolean) 0 else 1 + random.nextInt(Byte.MaxValue)) }
        // The zeroes make breaks in the graph - so that the transitive closure is guaranteed  to be different from the original graph.

        weightsForCurrentNode :: generateGraph(nodesLeftToDo - 1)
      }
    }
    new LinkGraphFunctional(numberOfNodes, generateGraph(numberOfNodes).toIndexedSeq)
  }
}

object Driver extends scala.testing.Benchmark {
  val random = new Random(8723)
  var sut: LinkGraphFunctional = null
  var pass = 0
  
  override def setUp() = {
    println("Pass: " + pass + ".")
    sut = LinkGraphFunctional(random)
  }
  
  override def tearDown() = {
    println("***** Finished *****")
    println()
    println()
    
    pass = pass + 1
  }
  
  def run = {
      val transitiveClosure = sut.floydWarshall // I need to have transitive closure.
      val result = 
          (for (i <- 0 until sut.numberOfNodes;
	            j <- 0 until sut.numberOfNodes)
        	  yield transitiveClosure(i)(j)).hashCode

      println (result)
  }
}
