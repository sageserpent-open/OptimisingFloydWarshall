class LinkGraphFunctional private (
    val numberOfNodes: Int,
    val graph: Vector[Int]) {
  
  def floydWarshall = {
    //println("Calculating distances...")
    (0 until numberOfNodes).foldLeft(graph)(floydWarshallIteration _)
      }
  
  def floydWarshallIteration(graph: Vector[Int], k: Int) = {
    //println(s"\r$k")
    
    Vector.tabulate(numberOfNodes * numberOfNodes) (l => {
      val i = l / numberOfNodes
      val j = l % numberOfNodes
      val ij = graph(l)
      val ik = graph(i * numberOfNodes + k)
      val kj = graph(k * numberOfNodes + j)
      if (i != j && ik * kj != 0 && (ij == 0 || ik + kj < ij))
        ik + kj
      else
        ij
    })
  }
}

import scala.util.Random
import scala.collection.mutable.ArraySeq

object LinkGraphFunctional {
  def apply(random: Random) = {
    val numberOfNodes = random.nextInt(500)
    println("Number of nodes: " + numberOfNodes + ".")

    def generateGraph(nodesLeftToDo: Int): Vector[Int] =
      Vector.tabulate(numberOfNodes * numberOfNodes) (_ => if (random.nextBoolean) 0 else 1 + random.nextInt(Byte.MaxValue))
      
    new LinkGraphFunctional(numberOfNodes, generateGraph(numberOfNodes))
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
        	  yield transitiveClosure(i * sut.numberOfNodes + j)).hashCode

      println (result)
  }
}
