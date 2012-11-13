import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {
	val t = new Unigram("/home/bborschi/data/brent/br-phono_spaced.txt",20,0,MINPATH,0.0)
                                                  //> t  : npbayes.wordseg.models.Unigram = npbayes.wordseg.models.Unigram@3abc8e1
                                                  //| e
	t.pypUni.base.logProb                     //> res0: Double = -0.0
  t.pypUni._oCount                                //> res1: Int = 0
  t.pypUni._tCount                                //> res2: Int = 0
  t.pypUni.logProb                                //> res3: Double = 0.0
  t.init(false)
  t.data.evaluate                                 //> res4: npbayes.wordseg.Result = 0.08562163263559341 0.1351836919784546 0.1048
                                                  //| 4030159267324 0.2722742259502411 0.495234876871109 0.35136962484998757
                                                  // | 	at npbayes.wordseg.data.VarData.evaluate(Data.scala:273)
  //t.pypUni._tCount
  //t.pypUni.base.logProb
  
  t.pypUni.logProb                                //> res5: Double = -415922.43706126366
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
  t.nTokens                                       //> res7: Int = 52732
  
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
  }                                               //> 0,-316894.35658681084
                                                  //| 0.2929823398590088 0.2372526079416275 0.26218876933975893 0.456710696220397
                                                  //| 95 0.3338133692741394 0.38570903263645806
                                                  //| 1,-306961.5580735307
                                                  //| 0.33078497648239136 0.23707295954227448 0.27619645116096736 0.5210291743278
                                                  //| 503 0.31221145391464233 0.3904544990609906
                                                  //| 2,-302317.89472347766
                                                  //| 0.35148054361343384 0.23776161670684814 0.2836476678624227 0.55760365724563
                                                  //| 6 0.3023846745491028 0.39212346072587173
                                                  //| 3,-297862.5291697589
                                                  //| 0.3618145287036896 0.23785142600536346 0.2870201348795427 0.580881118774414
                                                  //| 1 0.2993350028991699 0.395080360586471
                                                  //| 4,-295048.19905023195
                                                  //| 0.36863070726394653 0.2382706105709076 0.28945023223319744 0.59671127796173
                                                  //| 1 0.2981913685798645 0.3976614737035042
                                                  //| 5,-292942.6455471993
                                                  //| 0.37446129322052 0.23934848606586456 0.2920342641878314 0.6112649440765381 
                                                  //| 0.2992502748966217 0.40179713361817904
                                                  //| 6,-290642.6204371877
                                                  //| 0.3804568946361542 0.24084553122520447 0.2949653472538779 0.623447537422180
                                                  //| 2 0.29980093240737915 0.4048967512738136
                                                  //| 7,-288717.7700792273
                                                  //| 0.38370653986930847 0.24114494025707245 0.29616282773089603 0.6321428418159
                                                  //| 485 0.299885630607605 0.40679133816399177
                                                  //| 8,-287127.956198185
                                                  //| 0.38701510429382324 0.24255217611789703 0.2982091308035333 0.63938254117965
                                                  //| 7 0.3017493188381195 0.4100025819427751
                                                  //| 9,-285701.8531675993
                                                  //| 0.3895028829574585 0.24374981224536896 0.2998526664288456 0.644766449928283
                                                  //| 7 0.3034436106681824 0.4126728195245504

   //t.data.printAnalysis()
   

    
//  counts
  t.pypUni._oCount                                //> res8: Int = 20901
  t.pypUni._tCount                                //> res9: Int = 3845
  t.pypUni.base.logProb                           //> res10: Double = -146711.5117852086
  t.pypUni.logProb                                //> res11: Double = -285701.8531675993
//  t.pypUni.hmObsCounts
//  t.pypUni.hmTables
//  t.data.printAnalysis()
}