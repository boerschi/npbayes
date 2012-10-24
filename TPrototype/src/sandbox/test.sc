import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {
	val t = new Unigram("/home/bborschi/git/TDropping/TPrototype/tiny",1.0,0,EXACT,0.5)
                                                  //> t  : npbayes.wordseg.models.Unigram = npbayes.wordseg.models.Unigram@603b1d0
                                                  //| 4
  t.data.data                                     //> res0: Vector[Short] = Vector(0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  t.pypUni.logProb                                //> res1: Double = 4.440892098500626E-16
  t.init(false)
  t.pypUni._tCount                                //> res2: Int = 5
  t.pypUni.base.logProb                           //> res3: Double = -18.021826694558577
  
  t.pypUni.logProb                                //> res4: Double = -24.601077906568676
  t.data.symbolTable.hmStoR                       //> res5: scala.collection.mutable.HashMap[String,Short] = Map(a -> 0, t -> 2, b
                                                  //|  -> 1)
  t.pypUni.hmObsCounts                            //> res6: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Map
                                                  //| (b a b a t -> 1, a -> 2, b b a b -> 1, b -> 1, b t -> 1)
  t.data.printAnalysis()                          //> a::b a b a t::b
                                                  //| b t::a::b b a b
  t.pypUni.hmTables                               //> res7: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[Int
                                                  //| ]] = Map(b a b a t -> Vector(1), a -> Vector(2), b b a b -> Vector(1), b -> 
                                                  //| Vector(1), b t -> Vector(1))
  t.data.context(11)                              //> res8: npbayes.wordseg.data.Context = a(b b a,b b a)(b,b)$
//  t.pypUni.hmObsCounts
//  t.pypUni(Word("w e")+Word("l t"))

  t.pypUni.base.logProb                           //> res9: Double = -18.021826694558577
  
  t._predBoundary(0)                              //> res10: Double = 0.375
  t.nTokens                                       //> res11: Int = 6
  t.nUtterances                                   //> res12: Int = 2
  t.data.printAnalysis()                          //> a::b a b a t::b
                                                  //| b t::a::b b a b
  t.nUtterances                                   //> res13: Int = 2
  t.nTokens                                       //> res14: Int = 6
  val counts: HashMap[String,Double] = HashMap.empty
                                                  //> counts  : scala.collection.mutable.HashMap[String,Double] = Map()
  val tot=100000                                  //> tot  : Int = 100000
  for (i <- 0 until tot) {
   t.gibbsSweep()
   counts(t.data.getAnalysis)=
     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
  
//   println(t.pypUni._oCount)
//   println(t.pypUni._tCount)
//   println(t.pypUni.base.logProb)
//   t.data.printAnalysis()
  }
  for ((an,count) <- counts.toIndexedSeq.sortBy((x: (String,Double)) => -x._2))
    println(an+count/tot)                         //
                                                  //> a b::a b::a b
                                                  //| b::a b::b::a b
                                                  //| 0.36726
                                                  //| a b::a b::a b
                                                  //| b a b b::a b
                                                  //| 0.15014
                                                  //| a b a::b a b
                                                  //| b a b::b a b
                                                  //| 0.07238
                                                  //| a b::a b::a b
                                                  //| b a b b a b
                                                  //| 0.028
                                                  //| a b::a b::a b
                                                  //| b t::a b::b t::a b
                                                  //| 0.01877
                                                  //| a b::a b::a b
                                                  //| b a b b t::a b
                                                  //| 0.01862
                                                  //| a b::a b::a b
                                                  //| b a::b b::a b
                                                  //| 0.01831
                                                  //| a b::a b::a b
                                                  //| b a b::b a b
                                                  //| 0.01713
                                                  //| a b::a b::a b
                                                  //| b::a b b::a b
                                                  //| 0.01445
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 0.01149
                                                  //| a::b a b::a b
                                                  //| b a b::b a b
                                                  //| 0.01046
                                                  //| a b::a b::a b
                                                  //| b a b::b::a b
                                                  //| 0.00958
                                                  //| a b a t::b a b
                                                  //| b a b::b a b
                                                  //| 0.00954
                                                  //| a b::a b::a b
                                                  //| b t::a b::b::a b
                                                  //| 0.00953
                                                  //| a::b a::b a b
                                                  //| b a b::b a b
                                                  //| 0.00924
                                                  //| a b::a::b a b
                                                  //| b a b::b a b
                                                  //| 0.00915
                                                  //| a b::a b::a b
                                                  //| b::a b::b t::a b
                                                  //| 0.00913
                                                  //| a::b a b::a::b
                                                  //| b a b::b a b
                                                  //| 0.00785
                                                  //| a::b a::b a::b
                                                  //| b a::b::b a::b
                                                  //| 0.00735
                                                  //| a b::a b::a b
                                                  //| b::a b::b a b
                                                  //| 0.00699
                                                  //| a b::a b::a::b
                                                  //| b::a b::b::a b
                                                  //| 0.00585
                                                  //| a::b::a::b a b
                                                  //| b a b::b a b
                                                  //| 0.00574
                                                  //| a b::a b::a b
                                                  //| b::a b::b a::b
                                                  //| 0.00549
                                                  //| a b::a b::a b
                                                  //| b::a b::b::a::b
                                                  //| 0.00539
                                                  //| a b::a::b::a b
                                                  //| b::a b::b::a b
                                                  //| 0.00525
                                                  //| a::b::a b::a b
                                                  //| b::a b::b::a b
                                                  //| 0.00475
                                                  //| a b::a b::a b
                                                  //| b::a::b::b::a b
                                                  //| 0.00462
                                                  //| a b::a b::a b
                                                  //| b a b b a::b
                                                  //| 0.00392
                                                  //| a b::a b::a b
                                                  //| b a::b::b::a b
                                                  //| 0.00373
                                                  //| a b::a b::a b
                                                  //| b a::b b a b
                                                  //| 0.00322
                                                  //| a b::a b::a b
                                                  //| b a::b::b a::b
                                                  //| 0.00269
                                                  //| a b::a b::a b
                                                  //| b::a b b a b
                                                  //| 0.00247
                                                  //| a b::a b::a b
                                                  //| b a::b b t::a b
                                                  //| 0.00243
                                                  //| a b::a b::a b
                                                  //| b a t::b b::a b
                                                  //| 0.00239
                                                  //| a b a b a b
                                                  //| b a b b a b
                                                  //| 0.00214
                                                  //| a b::a b::a b
                                                  //| b a b t::b::a b
                                                  //| 0.00198
                                                  //| a b::a b::a b
                                                  //| b::a b b t::a b
                                                  //| 0.00185
                                                  //| a b a b a b
                                                  //| b a b::b a b
                                                  //| 0.00177
                                                  //| a b::a b::a b
                                                  //| b::a::b b::a b
                                                  //| 0.00175
                                                  //| a b::a b::a b
                                                  //| b t::a b b::a b
                                                  //| 0.00164
                                                  //| a b::a b::a b
                                                  //| b::a b b a::b
                                                  //| 0.00163
                                                  //| a b a::b a::b
                                                  //| b a::b::b a::b
                                                  //| 0.0016
                                                  //| a t::b a b::a b
                                                  //| b a b::b a b
                                                  //| 0.00153
                                                  //| a b a b::a b
                                                  //| b::a b::b::a b
                                                  //| 0.00142
                                                  //| a b a b::a b
                                                  //| b a b b::a b
                                                  //| 0.00141
                                                  //| a b::a b::a b
                                                  //| b a b::b t::a b
                                                  //| 0.00138
                                                  //| a b::a b a::b
                                                  //| b::a b::b::a b
                                                  //| 0.00129
                                                  //| a::b a t::b a b
                                                  //| b a b::b a b
                                                  //| 0.00126
                                                  //| a b a::b::a b
                                                  //| b::a b::b::a b
                                                  //| 0.00126
                                                  //| a t::b a::b a::b
                                                  //| b a::b::b a::b
                                                  //| 0.0012
                                                  //| a b t::a::b a b
                                                  //| b a b::b a b
                                                  //| 0.00116
                                                  //| a b::a b a b
                                                  //| b::a b::b::a b
                                                  //| 0.00112
                                                  //| a t::b a::b a b
                                                  //| b a b::b a b
                                                  //| 0.00108
                                                  //| a b::a t::b a b
                                                  //| b a b::b a b
                                                  //| 0.00105
                                                  //| a::b::a::b a::b
                                                  //| b a::b::b a::b
                                                  //| 0.001
                                                  //| a::b a::b a::b
                                                  //| b::a::b::b a::b
                                                  //| 9.9E-4
                                                  //| a b t::a b::a b
                                                  //| b::a b::b::a b
                                                  //| 9.8E-4
                                                  //| a b::a b a b
                                                  //| b a b b::a b
                                                  //| 9.5E-4
                                                  //| a::b a::b::a::b
                                                  //| b a::b::b a::b
                                                  //| 8.8E-4
                                                  //| a b::a b t::a b
                                                  //| b::a b::b::a b
                                                  //| 8.6E-4
                                                  //| a b::a b::a b
                                                  //| b::a b t::b::a b
                                                  //| 8.5E-4
                                                  //| a::b::a::b a b
                                                  //| b a b::b::a::b
                                                  //| 8.5E-4
                                                  //| a b::a::b::a b
                                                  //| b::a b::b::a::b
                                                  //| 8.1E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b::b::a::b
                                                  //| 8.1E-4
                                                  //| a b::a b::a b
                                                  //| b a b t::b a b
                                                  //| 7.4E-4
                                                  //| a b a b a::b
                                                  //| b a::b::b a::b
                                                  //| 7.3E-4
                                                  //| a::b a b::a::b
                                                  //| b a b::b::a::b
                                                  //| 7.3E-4
                                                  //| a b::a b::a b
                                                  //| b t::a b::b a b
                                                  //| 7.2E-4
                                                  //| a b::a t::b::a b
                                                  //| b::a b::b::a b
                                                  //| 7.2E-4
                                                  //| a::b a b::a::b
                                                  //| b::a::b::b a b
                                                  //| 7.0E-4
                                                  //| a::b::a b::a b
                                                  //| b::a b::b::a::b
                                                  //| 6.8E-4
                                                  //| a b a b::a b
                                                  //| b a b b a b
                                                  //| 6.7E-4
                                                  //| a b::a b::a t::b
                                                  //| b::a b::b::a b
                                                  //| 6.6E-4
                                                  //| a b::a b::a::b
                                                  //| b::a::b::b::a b
                                                  //| 6.6E-4
                                                  //| a b::a b::a b
                                                  //| b::a b::b::a t::b
                                                  //| 6.6E-4
                                                  //| a b a b a::b
                                                  //| b a b b a b
                                                  //| 6.3E-4
                                                  //| a b::a::b::a::b
                                                  //| b::a b::b::a b
                                                  //| 6.3E-4
                                                  //| a b::a b t::a b
                                                  //| b a b b::a b
                                                  //| 6.2E-4
                                                  //| a t::b::a b::a b
                                                  //| b::a b::b::a b
                                                  //| 6.2E-4
                                                  //| a b::a b::a b
                                                  //| b::a b::b a t::b
                                                  //| 6.2E-4
                                                  //| a b::a b::a b
                                                  //| b::a t::b::b::a b
                                                  //| 6.2E-4
                                                  //| a::b t::a::b a b
                                                  //| b a b::b a b
                                                  //| 6.1E-4
                                                  //| a b t::a b::a b
                                                  //| b a b b::a b
                                                  //| 5.8E-4
                                                  //| a b::a b::a::b
                                                  //| b::a b::b::a::b
                                                  //| 5.8E-4
                                                  //| a b::a::b::a b
                                                  //| b::a::b::b::a b
                                                  //| 5.7E-4
                                                  //| a b a b a::b
                                                  //| b a b b a::b
                                                  //| 5.7E-4
                                                  //| a::b::a b::a::b
                                                  //| b::a b::b::a b
                                                  //| 5.7E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b::b::a::b
                                                  //| 5.6E-4
                                                  //| a b::a b::a b
                                                  //| b a t::b::b::a b
                                                  //| 5.5E-4
                                                  //| a b a b::a b
                                                  //| b a b::b a b
                                                  //| 5.5E-4
                                                  //| a b a::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 5.4E-4
                                                  //| a::b::a::b::a b
                                                  //| b::a b::b::a b
                                                  //| 5.4E-4
                                                  //| a b a b a::b
                                                  //| b::a b::b::a b
                                                  //| 5.3E-4
                                                  //| a::b::a::b a b
                                                  //| b::a::b::b a b
                                                  //| 5.2E-4
                                                  //| a::b a b a b
                                                  //| b a b b a b
                                                  //| 5.1E-4
                                                  //| a b::a b a b
                                                  //| b a b b a b
                                                  //| 5.1E-4
                                                  //| a::b::a b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 5.0E-4
                                                  //| a b a b a::b
                                                  //| b::a b b a::b
                                                  //| 4.9E-4
                                                  //| a::b::a::b a::b
                                                  //| b::a::b::b::a::b
                                                  //| 4.9E-4
                                                  //| a b a b a b
                                                  //| b a::b::b a::b
                                                  //| 4.8E-4
                                                  //| a b a b a b
                                                  //| b::a b b a::b
                                                  //| 4.5E-4
                                                  //| a::b a::b::a::b
                                                  //| b::a::b::b a::b
                                                  //| 4.5E-4
                                                  //| a b::a b::a b
                                                  //| b a t::b b a b
                                                  //| 4.5E-4
                                                  //| a b::a b::a b
                                                  //| b a b b a t::b
                                                  //| 4.5E-4
                                                  //| a::b::a b::a b
                                                  //| b::a::b::b::a b
                                                  //| 4.5E-4
                                                  //| a b a b a b
                                                  //| b a b b a::b
                                                  //| 4.4E-4
                                                  //| a b::a b::a b
                                                  //| b a::b b a::b
                                                  //| 4.3E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b a::b
                                                  //| 4.2E-4
                                                  //| a t::b a b::a t::b
                                                  //| b a b::b a b
                                                  //| 4.2E-4
                                                  //| a::b a b a::b
                                                  //| b::a::b::b::a::b
                                                  //| 4.1E-4
                                                  //| a b a b a::b
                                                  //| b::a::b::b::a::b
                                                  //| 4.1E-4
                                                  //| a::b::a::b::a b
                                                  //| b::a::b::b::a::b
                                                  //| 4.1E-4
                                                  //| a b::a::b a b
                                                  //| b a b::b::a b
                                                  //| 3.9E-4
                                                  //| a b a b a b
                                                  //| b::a b b a b
                                                  //| 3.9E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b b::a::b
                                                  //| 3.8E-4
                                                  //| a::b::a::b a::b
                                                  //| b::a::b::b a::b
                                                  //| 3.8E-4
                                                  //| a b a b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 3.8E-4
                                                  //| a::b::a b a b
                                                  //| b::a::b::b::a::b
                                                  //| 3.7E-4
                                                  //| a b a b a::b
                                                  //| b a b::b a b
                                                  //| 3.7E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a b b a::b
                                                  //| 3.7E-4
                                                  //| a::b a::b::a::b
                                                  //| b a::b::b::a::b
                                                  //| 3.7E-4
                                                  //| a b a b a b
                                                  //| b a::b b a b
                                                  //| 3.7E-4
                                                  //| a::b a::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 3.6E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a b::b::a::b
                                                  //| 3.6E-4
                                                  //| a b::a b::a b
                                                  //| b t::a b b a b
                                                  //| 3.5E-4
                                                  //| a b::a b::a b
                                                  //| b a b b::a::b
                                                  //| 3.5E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b::a b
                                                  //| 3.5E-4
                                                  //| a b a::b a b
                                                  //| b a b b a b
                                                  //| 3.4E-4
                                                  //| a::b::a b a::b
                                                  //| b::a::b::b::a::b
                                                  //| 3.4E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b b a b
                                                  //| 3.4E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a::b::b::a::b
                                                  //| 3.3E-4
                                                  //| a b::a::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 3.2E-4
                                                  //| a::b::a b::a::b
                                                  //| b::a b::b::a::b
                                                  //| 3.1E-4
                                                  //| a b::a::b::a b
                                                  //| b a b b::a b
                                                  //| 3.1E-4
                                                  //| a b::a b::a::b
                                                  //| b a b b::a b
                                                  //| 3.1E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a::b::b a::b
                                                  //| 3.1E-4
                                                  //| a::b a b a b
                                                  //| b a b::b a b
                                                  //| 3.0E-4
                                                  //| a b a b a::b
                                                  //| b::a b b a b
                                                  //| 3.0E-4
                                                  //| a::b::a b::a::b
                                                  //| b::a::b::b::a b
                                                  //| 3.0E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a b::b::a::b
                                                  //| 3.0E-4
                                                  //| a::b a b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 3.0E-4
                                                  //| a b::a b::a b
                                                  //| b a::b t::b::a b
                                                  //| 2.9E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a b b::a::b
                                                  //| 2.9E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b b a b
                                                  //| 2.9E-4
                                                  //| a::b::a b::a b
                                                  //| b a b b::a b
                                                  //| 2.9E-4
                                                  //| a::b a b::a b
                                                  //| b a b::b::a b
                                                  //| 2.8E-4
                                                  //| a::b::a::b a::b
                                                  //| b a::b::b::a::b
                                                  //| 2.8E-4
                                                  //| a b a b a b
                                                  //| b a b b::a b
                                                  //| 2.8E-4
                                                  //| a b::a::b::a::b
                                                  //| b::a::b::b::a b
                                                  //| 2.7E-4
                                                  //| a::b::a::b::a b
                                                  //| b::a::b::b::a b
                                                  //| 2.7E-4
                                                  //| a t::b a b::a::b
                                                  //| b a b::b a b
                                                  //| 2.7E-4
                                                  //| a b a b a b
                                                  //| b::a b::b::a b
                                                  //| 2.7E-4
                                                  //| a::b::a::b a b
                                                  //| b::a::b::b::a::b
                                                  //| 2.6E-4
                                                  //| a b::a b::a::b
                                                  //| b::a b b::a b
                                                  //| 2.6E-4
                                                  //| a b::a::b::a b
                                                  //| b::a::b::b::a::b
                                                  //| 2.6E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b b a::b
                                                  //| 2.5E-4
                                                  //| a b::a b a t::b
                                                  //| b::a b::b::a b
                                                  //| 2.3E-4
                                                  //| a::b::a::b::a b
                                                  //| b::a b::b::a::b
                                                  //| 2.3E-4
                                                  //| a b a t::b::a b
                                                  //| b::a b::b::a b
                                                  //| 2.3E-4
                                                  //| a b::a b t::a b
                                                  //| b::a b t::b::a b
                                                  //| 2.3E-4
                                                  //| a b::a::b::a::b
                                                  //| b::a b::b::a::b
                                                  //| 2.2E-4
                                                  //| a b::a b::a b
                                                  //| b a t::b b t::a b
                                                  //| 2.2E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a b::b::a b
                                                  //| 2.2E-4
                                                  //| a b a b::a b
                                                  //| b a::b b::a b
                                                  //| 2.2E-4
                                                  //| a::b a b::a b
                                                  //| b::a b::b a b
                                                  //| 2.2E-4
                                                  //| a t::b::a t::b a b
                                                  //| b a b::b a b
                                                  //| 2.2E-4
                                                  //| a b::a b::a b
                                                  //| b a::b::b a b
                                                  //| 2.2E-4
                                                  //| a::b a::b a::b
                                                  //| b::a::b::b::a::b
                                                  //| 2.1E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b b a::b
                                                  //| 2.1E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b b a b
                                                  //| 2.1E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b b t::a b
                                                  //| 2.1E-4
                                                  //| a::b a::b a b
                                                  //| b a b::b a::b
                                                  //| 2.1E-4
                                                  //| a b a::b a b
                                                  //| b a b t::b a b
                                                  //| 2.0E-4
                                                  //| a b::a b::a b
                                                  //| b a b::b a::b
                                                  //| 2.0E-4
                                                  //| a b::a b::a b
                                                  //| b::a b b a t::b
                                                  //| 2.0E-4
                                                  //| a b::a b::a b
                                                  //| b a b t::b t::a b
                                                  //| 2.0E-4
                                                  //| a b::a b::a b
                                                  //| b t::a::b b::a b
                                                  //| 2.0E-4
                                                  //| a b a::b a b
                                                  //| b a b::b::a b
                                                  //| 2.0E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b a b
                                                  //| 1.9E-4
                                                  //| a b a b t::a b
                                                  //| b a b b::a b
                                                  //| 1.9E-4
                                                  //| a::b::a b::a b
                                                  //| b::a::b::b::a::b
                                                  //| 1.9E-4
                                                  //| a::b a::b a b
                                                  //| b a::b::b a b
                                                  //| 1.9E-4
                                                  //| a::b a::b a::b
                                                  //| b::a b::b a::b
                                                  //| 1.9E-4
                                                  //| a b::a b a b
                                                  //| b a b::b a b
                                                  //| 1.9E-4
                                                  //| a b::a b::a b
                                                  //| b::a b b::a::b
                                                  //| 1.8E-4
                                                  //| a::b a::b::a b
                                                  //| b::a b::b::a b
                                                  //| 1.8E-4
                                                  //| a::b a b a b
                                                  //| b::a::b::b::a::b
                                                  //| 1.8E-4
                                                  //| a b::a::b::a b
                                                  //| b::a b b::a b
                                                  //| 1.8E-4
                                                  //| a b a b t::a b
                                                  //| b::a b::b::a b
                                                  //| 1.8E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a b b a b
                                                  //| 1.8E-4
                                                  //| a b::a b::a b
                                                  //| b a t::b::b a t::b
                                                  //| 1.7E-4
                                                  //| a b a b a b
                                                  //| b::a::b::b::a::b
                                                  //| 1.7E-4
                                                  //| a b a t::b a::b
                                                  //| b a::b::b a::b
                                                  //| 1.7E-4
                                                  //| a b::a::b a b
                                                  //| b::a b::b a b
                                                  //| 1.7E-4
                                                  //| a b a b t::a b
                                                  //| b a b b a b
                                                  //| 1.7E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b b a::b
                                                  //| 1.7E-4
                                                  //| a b a b::a b
                                                  //| b a b b t::a b
                                                  //| 1.7E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a b b a::b
                                                  //| 1.7E-4
                                                  //| a b::a b::a b
                                                  //| b t::a b b t::a b
                                                  //| 1.7E-4
                                                  //| a b t::a b::a b
                                                  //| b::a b t::b::a b
                                                  //| 1.6E-4
                                                  //| a b::a::b a::b
                                                  //| b a::b::b a::b
                                                  //| 1.6E-4
                                                  //| a b::a b::a::b
                                                  //| b a b b a b
                                                  //| 1.6E-4
                                                  //| a b::a b::a::b
                                                  //| b t::a b::b::a b
                                                  //| 1.6E-4
                                                  //| a b::a b::a b
                                                  //| b t::a b::b a::b
                                                  //| 1.5E-4
                                                  //| a b::a b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 1.5E-4
                                                  //| a b::a::b::a b
                                                  //| b a b::b::a b
                                                  //| 1.5E-4
                                                  //| a b::a::b::a b
                                                  //| b::a b::b a::b
                                                  //| 1.5E-4
                                                  //| a b::a b::a b
                                                  //| b::a t::b b::a b
                                                  //| 1.5E-4
                                                  //| a::b a b::a t::b
                                                  //| b a b::b a b
                                                  //| 1.5E-4
                                                  //| a b::a b::a::b
                                                  //| b::a::b b::a b
                                                  //| 1.5E-4
                                                  //| a::b a::b a::b
                                                  //| b a b::b a b
                                                  //| 1.5E-4
                                                  //| a b::a::b::a b
                                                  //| b a b b a b
                                                  //| 1.4E-4
                                                  //| a b a b::a b
                                                  //| b a::b b a b
                                                  //| 1.4E-4
                                                  //| a b::a b::a::b
                                                  //| b::a b::b a::b
                                                  //| 1.4E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b::b t::a b
                                                  //| 1.4E-4
                                                  //| a b::a b::a b
                                                  //| b a b::b::a::b
                                                  //| 1.4E-4
                                                  //| a b a::b a::b
                                                  //| b a::b b a b
                                                  //| 1.4E-4
                                                  //| a::b a::b a b
                                                  //| b a::b::b a::b
                                                  //| 1.4E-4
                                                  //| a b::a b t::a b
                                                  //| b a b b a b
                                                  //| 1.4E-4
                                                  //| a b::a::b a::b
                                                  //| b::a b::b::a b
                                                  //| 1.4E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b::b::a b
                                                  //| 1.4E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a t::b::b::a::b
                                                  //| 1.3E-4
                                                  //| a b a::b a b
                                                  //| b a b::b a::b
                                                  //| 1.3E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a b b::a::b
                                                  //| 1.3E-4
                                                  //| a b::a::b a b
                                                  //| b a b b::a b
                                                  //| 1.3E-4
                                                  //| a b::a b::a b
                                                  //| b a::b::b t::a b
                                                  //| 1.3E-4
                                                  //| a b::a b::a b
                                                  //| b a::b t::b t::a b
                                                  //| 1.3E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b b::a::b
                                                  //| 1.3E-4
                                                  //| a t::b::a::b a b
                                                  //| b a b::b a b
                                                  //| 1.3E-4
                                                  //| a b::a b::a::b
                                                  //| b a b::b::a b
                                                  //| 1.3E-4
                                                  //| a::b a t::b a t::b
                                                  //| b a t::b::b a t::b
                                                  //| 1.3E-4
                                                  //| a::b a b a b
                                                  //| b a b b::a b
                                                  //| 1.3E-4
                                                  //| a::b::a b::a b
                                                  //| b::a b::b t::a b
                                                  //| 1.2E-4
                                                  //| a b a b::a b
                                                  //| b::a b b::a b
                                                  //| 1.2E-4
                                                  //| a::b::a b::a b
                                                  //| b::a::b b::a b
                                                  //| 1.2E-4
                                                  //| a b::a::b::a b
                                                  //| b::a b::b t::a b
                                                  //| 1.2E-4
                                                  //| a b::a b a b
                                                  //| b::a b b::a b
                                                  //| 1.2E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b::b a b
                                                  //| 1.2E-4
                                                  //| a b::a::b::a b
                                                  //| b a b b a::b
                                                  //| 1.2E-4
                                                  //| a b::a b t::a b
                                                  //| b a b::b a b
                                                  //| 1.2E-4
                                                  //| a::b::a::b::a::b
                                                  //| b a b::b a b
                                                  //| 1.2E-4
                                                  //| a::b a b a::b
                                                  //| b a b b a b
                                                  //| 1.2E-4
                                                  //| a b::a::b a b
                                                  //| b::a b::b::a b
                                                  //| 1.2E-4
                                                  //| a::b::a b::a b
                                                  //| b::a b b::a b
                                                  //| 1.2E-4
                                                  //| a t::b a t::b a b
                                                  //| b a b::b a b
                                                  //| 1.2E-4
                                                  //| a b a b a t::b
                                                  //| b a::b::b a::b
                                                  //| 1.1E-4
                                                  //| a b::a b::a b
                                                  //| b a::b t::b a::b
                                                  //| 1.1E-4
                                                  //| a b a::b a b
                                                  //| b a::b::b a b
                                                  //| 1.1E-4
                                                  //| a::b::a b::a b
                                                  //| b::a b::b a::b
                                                  //| 1.1E-4
                                                  //| a b::a b::a b
                                                  //| b a::b::b::a::b
                                                  //| 1.1E-4
                                                  //| a b t::a b::a b
                                                  //| b a b::b a b
                                                  //| 1.1E-4
                                                  //| a::b::a::b::a t::b
                                                  //| b::a::b::b::a::b
                                                  //| 1.1E-4
                                                  //| a b::a b a::b
                                                  //| b a b b::a b
                                                  //| 1.0E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b t::b a::b
                                                  //| 1.0E-4
                                                  //| a b::a b::a b
                                                  //| b::a::b::b a::b
                                                  //| 1.0E-4
                                                  //| a b::a::b t::a b
                                                  //| b a b b::a b
                                                  //| 1.0E-4
                                                  //| a b::a::b a::b
                                                  //| b::a b::b a::b
                                                  //| 1.0E-4
                                                  //| a b t::a b t::a::b
                                                  //| b::a b t::b::a::b
                                                  //| 1.0E-4
                                                  //| a::b a::b a::b
                                                  //| b a::b::b a b
                                                  //| 1.0E-4
                                                  //| a b a::b a b
                                                  //| b a b::b::a::b
                                                  //| 1.0E-4
                                                  //| a b a::b::a::b
                                                  //| b::a b b a::b
                                                  //| 1.0E-4
                                                  //| a b a b::a::b
                                                  //| b a b b a b
                                                  //| 1.0E-4
                                                  //| a::b::a::b::a::b
                                                  //| b::a b b a b
                                                  //| 1.0E-4
                                                  //| a::b a b a::b
                                                  //| b a b b a::b
                                                  //| 1.0E-4
                                                  //| a b a b::a b
                                                  //| b a b b a::b
                                                  //| 1.0E-4
                                                  //| a b::a b a::b
                                                  //| b::a b b::a b
                                                  //| 1.0E-4
                                                  //| a b t::a t::b a b
                                                  //| b a b::b a b
                                                  //| 1.0E-4
                                                  //| a b a::b a::b
                                                  //| b a b b a b
                                                  //| 1.0E-4
                                                  //| a::b::a b a b
                                                  //| b a b b a b
                                                  //| 1.0E-4
                                                  //| a::b a b::a b
                                                  //| b a b b::a b
                                                  //| 1.0E-4
                                                  //| a b t::a b::a b
                                                  //| b a b b a b
                                                  //| 1.0E-4
                                                  //| a t::b::a t::b::a t::b
                                                  //| b::a t::b::b::a t::b
                                                  //| 1.0E-4
                                                  //| a b a b a b
                                                  //| b a b b a t::b
                                                  //| 1.0E-4
                                                  //| a::b::a t::b a b
                                                  //| b a b::b a b
                                                  //| 1.0E-4
                                                  //| a::b a b t::a b
                                                  //| b a b t::b::a b
                                                  //| 1.0E-4
                                                  //| a b t::a b t::a b
                                                  //| b a b b::a b
                                                  //| 1.0E-4
                                                  //| a b::a b::a::b
                                                  //| b a b::b a b
                                                  //| 9.0E-5
                                                  //| a b::a::b::a b
                                                  //| b t::a b::b::a b
                                                  //| 9.0E-5
                                                  //| a b::a b::a::b
                                                  //| b a b b t::a b
                                                  //| 9.0E-5
                                                  //| a b a t::b a b
                                                  //| b a b b a b
                                                  //| 9.0E-5
                                                  //| a b::a b a b
                                                  //| b a b b t::a b
                                                  //| 9.0E-5
                                                  //| a b::a b::a t::b
                                                  //| b a b b::a b
                                                  //| 9.0E-5
                                                  //| a b::a::b::a b
                                                  //| b a b::b a b
                                                  //| 9.0E-5
                                                  //| a::b::a b a::b
                                                  //| b a b b a b
                                                  //| 9.0E-5
                                                  //| a b a::b::a::b
                                                  //| b a b b a b
                                                  //| 9.0E-5
                                                  //| a b::a b t::a b
                                                  //| b t::a b::b t::a b
                                                  //| 9.0E-5
                                                  //| a::b a b a b
                                                  //| b a b b a::b
                                                  //| 9.0E-5
                                                  //| a b t::a b a b
                                                  //| b a b b a b
                                                  //| 9.0E-5
                                                  //| a b::a b a::b
                                                  //| b::a b::b::a::b
                                                  //| 9.0E-5
                                                  //| a b a b a b
                                                  //| b a::b b::a b
                                                  //| 9.0E-5
                                                  //| a b t::a b t::a b
                                                  //| b::a b::b::a b
                                                  //| 9.0E-5
                                                  //| a b a b a::b
                                                  //| b a::b::b::a::b
                                                  //| 9.0E-5
                                                  //| a::b a::b a b
                                                  //| b a::b t::b a b
                                                  //| 9.0E-5
                                                  //| a b a b a::b
                                                  //| b a::b b a b
                                                  //| 9.0E-5
                                                  //| a b::a b::a::b
                                                  //| b a::b::b::a b
                                                  //| 9.0E-5
                                                  //| a b::a b a b
                                                  //| b a b b a::b
                                                  //| 9.0E-5
                                                  //| a b a::b::a b
                                                  //| b a b b::a b
                                                  //| 9.0E-5
                                                  //| a::b a b::a::b
                                                  //| b a b::b a::b
                                                  //| 9.0E-5
                                                  //| a b::a::b::a b
                                                  //| b::a b::b a b
                                                  //| 9.0E-5
                                                  //| a::b::a::b::a::b
                                                  //| b t::a::b::b::a::b
                                                  //| 9.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a b::b a b
                                                  //| 9.0E-5
                                                  //| a::b a::b::a b
                                                  //| b::a b::b a::b
                                                  //| 8.0E-5
                                                  //| a b::a::b::a b
                                                  //| b::a::b b::a b
                                                  //| 8.0E-5
                                                  //| a b a t::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 8.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a b b t::a b
                                                  //| 8.0E-5
                                                  //| a::b a::b::a b
                                                  //| b a::b::b a::b
                                                  //| 8.0E-5
                                                  //| a b a b::a::b
                                                  //| b::a b::b::a b
                                                  //| 8.0E-5
                                                  //| a b::a b::a b
                                                  //| b t::a::b t::b t::a b
                                                  //| 8.0E-5
                                                  //| a::b::a b::a b
                                                  //| b::a b::b a b
                                                  //| 8.0E-5
                                                  //| a b a::b::a b
                                                  //| b a b b a::b
                                                  //| 8.0E-5
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b t::b::a::b
                                                  //| 8.0E-5
                                                  //| a b a b::a b
                                                  //| b a b::b::a b
                                                  //| 8.0E-5
                                                  //| a b::a b t::a b
                                                  //| b a b b t::a b
                                                  //| 8.0E-5
                                                  //| a b::a b::a::b
                                                  //| b::a b::b a b
                                                  //| 8.0E-5
                                                  //| a b::a b::a::b
                                                  //| b a b b a::b
                                                  //| 8.0E-5
                                                  //| a::b a b a::b
                                                  //| b a::b::b a::b
                                                  //| 8.0E-5
                                                  //| a b a b::a b
                                                  //| b::a::b::b::a b
                                                  //| 8.0E-5
                                                  //| a t::b::a::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 8.0E-5
                                                  //| a b::a b::a b
                                                  //| b::a t::b b a::b
                                                  //| 8.0E-5
                                                  //| a::b t::a b::a b
                                                  //| b a b b::a b
                                                  //| 8.0E-5
                                                  //| a::b a::b a::b
                                                  //| b a b b a b
                                                  //| 8.0E-5
                                                  //| a::b::a b a b
                                                  //| b::a b b a::b
                                                  //| 8.0E-5
                                                  //| a::b a b::a b
                                                  //| b a b::b::a::b
                                                  //| 8.0E-5
                                                  //| a b a b a b
                                                  //| b a b b::a::b
                                                  //| 8.0E-5
                                                  //| a::b a::b a::b
                                                  //| b a b::b a::b
                                                  //| 8.0E-5
                                                  //| a b a b a b
                                                  //| b t::a b b a b
                                                  //| 8.0E-5
                                                  //| a b::a b::a b
                                                  //| b t::a::b b a b
                                                  //| 8.0E-5
                                                  //| a::b a b::a b
                                                  //| b::a b::b::a b
                                                  //| 8.0E-5
                                                  //| a b::a::b a::b
                                                  //| b a::b b::a b
                                                  //| 8.0E-5
                                                  //| a b::a b::a b
                                                  //| b a b t::b a::b
                                                  //| 8.0E-5
                                                  //| a b::a b::a::b
                                                  //| b::a b::b t::a b
                                                  //| 8.0E-5
                                                  //| a t::b a b a b
                                                  //| b a b b a b
                                                  //| 8.0E-5
                                                  //| a b::a t::b::a b
                                                  //| b::a t::b::b::a b
                                                  //| 8.0E-5
                                                  //| a::b::a t::b::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 8.0E-5
                                                  //| a::b::a b a b
                                                  //| b::a b::b::a b
                                                  //| 8.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a b::b::a b
                                                  //| 8.0E-5
                                                  //| a::b a::b a b
                                                  //| b a b::b::a::b
                                                  //| 8.0E-5
                                                  //| a b a::b a b
                                                  //| b::a b b a::b
                                                  //| 8.0E-5
                                                  //| a::b::a::b::a::b
                                                  //| b a t::b::b a t::b
                                                  //| 8.0E-5
                                                  //| a b::a b::a b
                                                  //| b::a::b b::a::b
                                                  //| 8.0E-5
                                                  //| a b a b::a::b
                                                  //| b a::b::b a::b
                                                  //| 8.0E-5
                                                  //| a b::a::b t::a b
                                                  //| b t::a b::b t::a b
                                                  //| 8.0E-5
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b::a t::b
                                                  //| 8.0E-5
                                                  //| a b a::b::a::b
                                                  //| b a::b::b a::b
                                                  //| 8.0E-5
                                                  //| a b::a::b::a b
                                                  //| b a::b::b::a b
                                                  //| 8.0E-5
                                                  //| a::b a b a b
                                                  //| b a::b::b a::b
                                                  //| 8.0E-5
                                                  //| a::b::a b a::b
                                                  //| b::a::b::b a::b
                                                  //| 7.0E-5
                                                  //| a::b a b a::b
                                                  //| b::a::b b a b
                                                  //| 7.0E-5
                                                  //| a b a b a t::b
                                                  //| b::a b b a b
                                                  //| 7.0E-5
                                                  //| a b::a b a b
                                                  //| b a::b b::a b
                                                  //| 7.0E-5
                                                  //| a b a b a t::b
                                                  //| b a b b a::b
                                                  //| 7.0E-5
                                                  //| a::b a b a::b
                                                  //| b::a b b a b
                                                  //| 7.0E-5
                                                  //| a b::a b::a::b
                                                  //| b a::b::b a::b
                                                  //| 7.0E-5
                                                  //| a b a b a b
                                                  //| b a::b b a::b
                                                  //| 7.0E-5
                                                  //| a::b a b a b
                                                  //| b::a b b a::b
                                                  //| 7.0E-5
                                                  //| a b::a b a b
                                                  //| b a b::b::a b
                                                  //| 7.0E-5
                                                  //| a b a::b a b
                                                  //| b::a b::b a b
                                                  //| 7.0E-5
                                                  //| a b::a::b::a::b
                                                  //| b a b::b a b
                                                  //| 7.0E-5
                                                  //| a b a b a t::b
                                                  //| b::a b::b::a b
                                                  //| 7.0E-5
                                                  //| a::b a::b::a::b
                                                  //| b a::b b a::b
                                                  //| 7.0E-5
                                                  //| a b a b::a b
                                                  //| b t::a b::b::a b
                                                  //| 7.0E-5
                                                  //| a b::a::b::a b
                                                  //| b a b b t::a b
                                                  //| 7.0E-5
                                                  //| a b::a::b::a b
                                                  //| b a b b::a::b
                                                  //| 7.0E-5
                                                  //| a::b::a::b a t::b
                                                  //| b::a::b::b::a::b
                                                  //| 7.0E-5
                                                  //| a b a b::a b
                                                  //| b::a b::b::a::b
                                                  //| 7.0E-5
                                                  //| a b::a b a::b
                                                  //| b::a b::b a::b
                                                  //| 7.0E-5
                                                  //| a b::a b a::b
                                                  //| b a b::b a b
                                                  //| 7.0E-5
                                                  //| a b::a::b::a b
                                                  //| b::a::b b a::b
                                                  //| 7.0E-5
                                                  //| a b a b::a::b
                                                  //| b::a b b a b
                                                  //| 7.0E-5
                                                  //| a b a b a b
                                                  //| b a b::b a::b
                                                  //| 7.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a b b a b
                                                  //| 7.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a b b a::b
                                                  //| 7.0E-5
                                                  //| a::b::a b::a b
                                                  //| b a::b::b::a b
                                                  //| 7.0E-5
                                                  //| a b a::b a::b
                                                  //| b a::b::b::a::b
                                                  //| 7.0E-5
                                                  //| a::b t::a b::a b
                                                  //| b t::a b::b t::a b
                                                  //| 7.0E-5
                                                  //| a b a::b a b
                                                  //| b::a::b::b a b
                                                  //| 7.0E-5
                                                  //| a::b::a::b::a::b
                                                  //| b::a::b::b t::a::b
                                                  //| 7.0E-5
                                                  //| a::b::a b a::b
                                                  //| b::a b b a::b
                                                  //| 7.0E-5
                                                  //| a::b::a b t::a::b
                                                  //| b::a::b::b::a::b
                                                  //| 7.0E-5
                                                  //| a b a::b::a b
                                                  //| b a::b::b a::b
                                                  //| 7.0E-5
                                                  //| a b a b a::b
                                                  //| b::a::b::b::a b
                                                  //| 6.0E-5
                                                  //| a::b a b a b
                                                  //| b::a b b a b
                                                  //| 6.0E-5
                                                  //| a b a b::a::b
                                                  //| b::a b b a::b
                                                  //| 6.0E-5
                                                  //| a b a b a::b
                                                  //| b::a b b::a::b
                                                  //| 6.0E-5
                                                  //| a b a::b a b
                                                  //| b a b b::a b
                                                  //| 6.0E-5
                                                  //| a b::a b t::a b
                                                  //| b a::b b::a b
                                                  //| 6.0E-5
                                                  //| a b::a b::a b
                                                  //| b t::a b 
                                                  //| Output exceeds cutoff limit.
    
//  counts
  t.pypUni._oCount                                //> res15: Int = 7
  t.pypUni._tCount                                //> res16: Int = 2
  t.pypUni.base.logProb                           //> res17: Double = -4.1588830833587345
  t.pypUni.logProb                                //> res18: Double = -9.5059906140762
  t.pypUni.hmObsCounts                            //> res19: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = M
                                                  //| ap(a b -> 5, b -> 2)
  t.pypUni.hmTables                               //> res20: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[I
                                                  //| nt]] = Map(a b -> Vector(5), b -> Vector(2))
  t.data.printAnalysis()                          //> a b::a b::a b
                                                  //| b::a b::b::a b
}