import npbayes.distributions._
import npbayes.wordseg.VarData
object test {
	val test = new CRP[Word](0.001,0,new Monkey[Word](50,0.5))
                                                  //> test  : npbayes.distributions.CRP[npbayes.distributions.Word] = npbayes.dist
                                                  //| ributions.CRP@5829428e
  test.hmObsCounts                                //> res0: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Map
                                                  //| ()
  test.update(Word("Ha"))                         //> at new
                                                  //| res1: Double = 1.0E-4
  test.hmObsCounts                                //> res2: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Map
                                                  //| (Ha -> 1)
	test.hmTables                             //> res3: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[Int
                                                  //| ]] = Map(Ha -> Vector(1))
  test.predProb(Word("Ha"))                       //> res4: Double = 0.999001098901099
  test.update(Word("Ha"))                         //> at old
                                                  //| 0.3706481701461719
                                                  //| 1.0
                                                  //| res5: Double = 0.9990009990009991
  test.hmObsCounts                                //> res6: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Map
                                                  //| (Ha -> 2)
  test.predProb(Word("Ha"))                       //> res7: Double = 0.9995002998500749
  test.update(Word("Ha"))                         //> at old
                                                  //| 1.169562498215244
                                                  //| 2.0
                                                  //| res8: Double = 0.9995002498750625
  test.predProb(Word("Ha"))                       //> res9: Double = 0.999666811062979

  test.update(Word("Ha"))                         //> at old
                                                  //| 1.494677132988048
                                                  //| 3.0
                                                  //| res10: Double = 0.9996667777407531
  test.predProb(Word("Ha"))                       //> res11: Double = 0.9997500874781303

  test.update(Word("Ha"))                         //> at old
                                                  //| 1.3505724716216658
                                                  //| 4.0
                                                  //| res12: Double = 0.9997500624843788
  test.predProb(Word("Ha"))                       //> res13: Double = 0.9998000599880024

  test.update(Word("Ha"))                         //> at old
                                                  //| 1.3231556879073636
                                                  //| 5.0
                                                  //| res14: Double = 0.9998000399920015
                                                 
  test.predProb(Word("Ha"))                       //> res15: Double = 0.9998333777703716
  test.hmObsCounts                                //> res16: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Ma
                                                  //| p(Ha -> 6)
//  test.hmTables
  test.remove(Word("Ha"))                         //> 4
                                                  //| 6
                                                  //| res17: Double = 0.9998000599880024
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	val x = "Hallo"                           //> x  : java.lang.String = Hallo
	val y = "Hallo"                           //> y  : java.lang.String = Hallo
	x.equals(y)                               //> res18: Boolean = true
	val small = new VarData("/home/bborschi/software/ScalaIDE/research/TPrototype/sm")
                                                  //> small  : npbayes.wordseg.VarData = npbayes.wordseg.VarData@3f57fb52
  small.goldBoundaries                            //> res19: Vector[npbayes.wordseg.Boundary] = Vector(NoBoundary, WBoundaryDrop, 
                                                  //| NoBoundary, NoBoundary, NoBoundary, UBoundaryNodrop, NoBoundary, NoBoundary,
                                                  //|  WBoundaryNodrop, NoBoundary, WBoundaryNodrop, NoBoundary, NoBoundary, UBoun
                                                  //| daryDrop)
  small.data.foreach((x: Short) => println(small.symbolTable(x)))
                                                  //> ay
                                                  //| n
                                                  //| t
                                                  //| eh
                                                  //| s
                                                  //| t
                                                  //| n
                                                  //| oh
                                                  //| ch
                                                  //| ay
                                                  //| n
                                                  //| t
                                                  //| eh
                                                  //| s
  small.goldBoundaries                            //> res20: Vector[npbayes.wordseg.Boundary] = Vector(NoBoundary, WBoundaryDrop, 
                                                  //| NoBoundary, NoBoundary, NoBoundary, UBoundaryNodrop, NoBoundary, NoBoundary,
                                                  //|  WBoundaryNodrop, NoBoundary, WBoundaryNodrop, NoBoundary, NoBoundary, UBoun
                                                  //| daryDrop)
  small.boundaries                                //> res21: Vector[npbayes.wordseg.Boundary] = Vector(NoBoundary, WBoundaryDrop, 
                                                  //| WBoundaryDrop, WBoundaryDrop, NoBoundary, UBoundaryDrop, NoBoundary, WBounda
                                                  //| ryNodrop, NoBoundary, WBoundaryDrop, WBoundaryDrop, WBoundaryDrop, WBoundary
                                                  //| Nodrop, UBoundaryDrop)
}