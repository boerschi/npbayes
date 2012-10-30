import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {
	val t = new Unigram("/home/bborschi/data/brent/br-phono_spaced.txt",20,0,MINPATH,0.0)
                                                  //> t  : npbayes.wordseg.models.Unigram = npbayes.wordseg.models.Unigram@39b8d6f
                                                  //| 7
	t.pypUni.base.logProb                     //> res0: Double = 0.0
  t.pypUni._oCount                                //> res1: Int = 0
  t.pypUni._tCount                                //> res2: Int = 0
  t.pypUni.logProb                                //> res3: Double = 0.0
  t.init(false)
  t.data.evaluate                                 //> res4: npbayes.wordseg.Result = 0.059745471200382205 0.04876893758773804 0.07
                                                  //| 709811627864838 0.3533526522232414 0.27365729212760925 0.49853870272636414
                                                  // | 	at npbayes.wordseg.data.VarData.evaluate(Data.scala:273)
  //t.pypUni._tCount
  //t.pypUni.base.logProb
  
  t.pypUni.logProb                                //> res5: Double = -417368.7966868523
  //t.data.symbolTable.hmStoR
//  t.pypUni.hmObsCounts
//  t.data.printAnalysis()
  //t.pypUni.hmTables
 // t.data.context(11)
//  t.pypUni.hmObsCounts
//  t.pypUni(Word("w e")+Word("l t"))

//  t.pypUni.base.logProb
  
//  t._predBoundary(0)
//  t.nTokens
//  t.nUtterances
//  t.data.printAnalysis()
  t.nUtterances                                   //> res6: Int = 9790
  t.nTokens                                       //> res7: Int = 52800
  
//  t.data.evaluate
  val tot=10                                      //> tot  : Int = 10
  for (i <- 0 until tot) {
   t.gibbsSweep()
   println(i+","+t.pypUni.logProb)
//  counts(t.data.getAnalysis)=
//     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
//   println(t.pypUni._oCount)
//   println(t.data.evaluate)
   println(t.data.evaluate)
//   println(t.pypUni._tCount)
//   println(t.pypUni._oCount)
//   t.data.printAnalysis()
  }                                               //> 0,-295376.8453722212
                                                  //| 0.14869353573820862 0.13825051486492157 0.1608431339263916 0.47209354117882
                                                  //| 26 0.42777031660079956 0.5266635417938232
                                                  //| 1,-268335.07812166604
                                                  //| 0.18197914147410113 0.17938798666000366 0.18464624881744385 0.5019399541545
                                                  //| 129 0.4919472932815552 0.5123469829559326
                                                  //| 2,-261862.08537179278
                                                  //| 0.19200859692591493 0.19143997132778168 0.19258061051368713 0.5144990063799
                                                  //| 619 0.5123487710952759 0.516667366027832
                                                  //| 3,-259124.93031916313
                                                  //| 0.19772158523245711 0.197712704539299 0.19773046672344208 0.522225288430035
                                                  //| 2 0.522192120552063 0.522258460521698
                                                  //| 4,-257167.8364821865
                                                  //| 0.203604653822157 0.20376034080982208 0.20344920456409454 0.531685267169343
                                                  //| 2 0.5322607755661011 0.5311110019683838
                                                  //| 5,-255335.9673696206
                                                  //| 0.20772439224236253 0.20819923281669617 0.20725171267986298 0.5374155319932
                                                  //| 369 0.5391567349433899 0.5356855392456055
                                                  //| 6,-253866.0739388955
                                                  //| 0.21227640606095463 0.21296407282352448 0.2115931659936905 0.54409292361382
                                                  //| 04 0.5465931296348572 0.5416154861450195
                                                  //| 7,-252832.0100941626
                                                  //| 0.21608228617540787 0.21674297749996185 0.21542561054229736 0.5503349819114
                                                  //| 051 0.5527215003967285 0.5479689836502075
                                                  //| 8,-251576.60010170809
                                                  //| 0.22008203494810058 0.22088183462619781 0.21928800642490387 0.5559316288031
                                                  //| 336 0.5587983727455139 0.5530941486358643
                                                  //| 9,-250376.62710277317
                                                  //| 0.22326868639804381 0.22438611090183258 0.22216233611106873 0.5612643176204
                                                  //| 728 0.5652547478675842 0.5573298335075378

   //t.data.printAnalysis()
   

    
//  counts
  t.pypUni._oCount                                //> res8: Int = 33068
  t.pypUni._tCount                                //> res9: Int = 2127
  t.pypUni.base.logProb                           //> res10: Double = -32287.805530828817
  t.pypUni.logProb                                //> res11: Double = -250376.62710277317
//  t.pypUni.hmObsCounts
//  t.pypUni.hmTables
//  t.data.printAnalysis()
}