object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  import scala.util.Random
  
  val random = new Random(2)                      //> random  : scala.util.Random = scala.util.Random@51b48197
  
  val sut = LinkGraphFunctional(random)           //> Number of nodes: 108.
                                                  //| sut  : LinkGraphFunctional = LinkGraphFunctional@1d2940b3
  sut.graph                                       //> res0: IndexedSeq[IndexedSeq[Byte]] = Vector(ArraySeq(59, 26, 0, 0, 80, 0, 0,
                                                  //|  70, 0, 18, 0, 42, 0, 14, 0, 0, 0, 78, 33, 0, 42, 0, 0, 0, 100, 0, 32, 0, 10
                                                  //| 6, 0, 51, 54, 0, 0, 65, 0, 11, 0, 73, 0, 54, 0, 0, 51, 0, 78, 8, 0, 0, 106, 
                                                  //| 0, 18, 94, 0, 121, 25, 30, 30, 106, 99, 0, 45, 0, 105, 0, 0, 0, 115, 85, 0, 
                                                  //| 104, 110, 0, 0, 87, 67, 0, 0, 0, 116, 2, 0, 0, 0, 1, 0, 31, 0, 72, 107, 0, 0
                                                  //| , 108, 0, 91, 4, 0, 0, 0, 32, 0, 0, 105, 0, 0, 55, 27, 0), ArraySeq(0, 123, 
                                                  //| 0, 0, 120, 0, 0, 0, 0, 0, 62, 0, 0, 0, 27, 84, 0, 0, 76, 0, 123, 109, 0, 0, 
                                                  //| 23, 0, 62, 55, 0, 0, 0, 0, 0, 41, 111, 0, 0, 0, 0, 68, 0, 50, 74, 0, 76, 87,
                                                  //|  101, 8, 87, 0, 0, 0, 30, 46, 0, 0, 48, 17, 0, 58, 0, 70, 0, 0, 0, 0, 41, 26
                                                  //| , 0, 0, 97, 0, 54, 69, 7, 0, 29, 42, 89, 112, 0, 0, 24, 122, 122, 10, 66, 0,
                                                  //|  0, 0, 0, 64, 0, 0, 0, 102, 39, 47, 0, 41, 0, 0, 53, 18, 0, 0, 0, 106), Arra
                                                  //| ySeq(57, 0, 27, 0, 0, 26, 63, 9, 72, 17, 50, 64, 87, 0, 0, 0, 66, 51, 62, 53
                                                  //| , 0, 83, 0, 23, 81, 123,
                                                  //| Output exceeds cutoff limit.
                                                  
  sut.floydWarshall                               //> res1: IndexedSeq[IndexedSeq[Byte]] = Vector(Vector(59, 15, 16, -17, -20, -14
                                                  //| , 14, 15, 8, -14, -18, 9, 10, -15, -3, -23, 11, 8, 8, -21, -24, -114, -15, -
                                                  //| 26, 9, -113, -114, 20, -118, -117, 2, -6, 16, -4, 12, 16, 5, 20, 12, 16, 13,
                                                  //|  7, -4, -2, -9, 10, 16, 21, 16, 6, 7, -11, 15, 11, 13, 16, 14, 12, 9, 15, -6
                                                  //| , 8, -14, -10, -8, 16, -2, 8, 20, 12, 10, -13, 13, 7, 15, 11, 11, -14, 13, 1
                                                  //| 3, -13, -23, 9, 15, 12, 15, 4, -89, 14, -23, 13, 2, 12, 5, -24, 14, -16, 11,
                                                  //|  5, -11, -17, -7, 11, -1, 15, 5, 16, 8), Vector(58, 123, 53, 20, 17, 23, 51,
                                                  //|  52, 45, 23, 19, 46, 47, 22, 34, 14, 48, 45, 45, 16, 13, -77, 22, 11, 46, -7
                                                  //| 6, -92, 57, -121, -113, 39, 31, 53, 33, 49, 53, 42, 57, 49, 53, 50, 44, 33, 
                                                  //| 35, 28, 47, 53, 58, 53, 43, 44, 26, 52, 48, 50, 53, 51, 49, 46, 52, 31, 45, 
                                                  //| 23, 27, 29, 53, 35, 45, 57, 49, 47, 24, 50, 44, 52, 48, 48, 23, 50, 50, 24, 
                                                  //| 14, 46, 37, 49, 52, 41, -52, 51, 14, 50, 39, 49, 42, 13, 51, 21, 48, 42, 26,
                                                  //|  20, 30, 48, 36, 52, 42,
                                                  //| Output exceeds cutoff limit.
  
}