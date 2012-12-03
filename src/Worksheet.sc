object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import scala.util.Random
  
  val random = new Random(2)                      //> random  : scala.util.Random = scala.util.Random@2dec8909
  
  val sut = LinkGraphFunctional(random)           //> Number of nodes: 108.
                                                  //| sut  : LinkGraphFunctional = LinkGraphFunctional@6719dc16
  sut.graph                                       //> res0: Vector[Vector[Int]] = Vector(Vector(59, 26, 0, 0, 80, 0, 0, 70, 0, 18,
                                                  //|  0, 42, 0, 14, 0, 0, 0, 78, 33, 0, 42, 0, 0, 0, 100, 0, 32, 0, 106, 0, 51, 5
                                                  //| 4, 0, 0, 65, 0, 11, 0, 73, 0, 54, 0, 0, 51, 0, 78, 8, 0, 0, 106, 0, 18, 94, 
                                                  //| 0, 121, 25, 30, 30, 106, 99, 0, 45, 0, 105, 0, 0, 0, 115, 85, 0, 104, 110, 0
                                                  //| , 0, 87, 67, 0, 0, 0, 116, 2, 0, 0, 0, 1, 0, 31, 0, 72, 107, 0, 0, 108, 0, 9
                                                  //| 1, 4, 0, 0, 0, 32, 0, 0, 105, 0, 0, 55, 27, 0), Vector(0, 123, 0, 0, 120, 0,
                                                  //|  0, 0, 0, 0, 62, 0, 0, 0, 27, 84, 0, 0, 76, 0, 123, 109, 0, 0, 23, 0, 62, 55
                                                  //| , 0, 0, 0, 0, 0, 41, 111, 0, 0, 0, 0, 68, 0, 50, 74, 0, 76, 87, 101, 8, 87, 
                                                  //| 0, 0, 0, 30, 46, 0, 0, 48, 17, 0, 58, 0, 70, 0, 0, 0, 0, 41, 26, 0, 0, 97, 0
                                                  //| , 54, 69, 7, 0, 29, 42, 89, 112, 0, 0, 24, 122, 122, 10, 66, 0, 0, 0, 0, 64,
                                                  //|  0, 0, 0, 102, 39, 47, 0, 41, 0, 0, 53, 18, 0, 0, 0, 106), Vector(57, 0, 27,
                                                  //|  0, 0, 26, 63, 9, 72, 17, 50, 64, 87, 0, 0, 0, 66, 51, 62, 53, 0, 83, 0, 23,
                                                  //|  81, 123, 0, 24, 0, 41, 
                                                  //| Output exceeds cutoff limit.
                                                  
  sut.floydWarshall                               //> res1: Vector[Vector[Int]] = Vector(Vector(0, 123, 0, 0, 120, 0, 0, 0, 0, 0, 
                                                  //| 62, 0, 0, 0, 27, 84, 0, 0, 76, 0, 123, 109, 0, 0, 23, 0, 62, 55, 0, 0, 0, 0,
                                                  //|  0, 41, 111, 0, 0, 0, 0, 68, 0, 50, 74, 0, 76, 87, 101, 8, 87, 0, 0, 0, 30, 
                                                  //| 46, 0, 0, 48, 17, 0, 58, 0, 70, 0, 0, 0, 0, 41, 26, 0, 0, 97, 0, 54, 69, 7, 
                                                  //| 0, 29, 42, 89, 112, 0, 0, 24, 122, 122, 10, 66, 0, 0, 0, 0, 64, 0, 0, 0, 102
                                                  //| , 39, 47, 0, 41, 0, 0, 53, 18, 0, 0, 0, 106), Vector(0, 123, 0, 0, 120, 0, 0
                                                  //| , 0, 0, 0, 62, 0, 0, 0, 27, 84, 0, 0, 76, 0, 123, 109, 0, 0, 23, 0, 62, 55, 
                                                  //| 0, 0, 0, 0, 0, 41, 111, 0, 0, 0, 0, 68, 0, 50, 74, 0, 76, 87, 101, 8, 87, 0,
                                                  //|  0, 0, 30, 46, 0, 0, 48, 17, 0, 58, 0, 70, 0, 0, 0, 0, 41, 26, 0, 0, 97, 0, 
                                                  //| 54, 69, 7, 0, 29, 42, 89, 112, 0, 0, 24, 122, 122, 10, 66, 0, 0, 0, 0, 64, 0
                                                  //| , 0, 0, 102, 39, 47, 0, 41, 0, 0, 53, 18, 0, 0, 0, 106), Vector(0, 123, 0, 0
                                                  //| , 120, 0, 0, 0, 0, 0, 62, 0, 0, 0, 27, 84, 0, 0, 76, 0, 123, 109, 0, 0, 23, 
                                                  //| 0, 62, 55, 0, 0, 0, 0, 0
                                                  //| Output exceeds cutoff limit.
  
}