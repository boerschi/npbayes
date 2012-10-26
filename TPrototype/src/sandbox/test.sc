import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {
	val t = new Unigram("/home/bborschi/data/brent/br-phono_spaced.txt",20,0,MINPATH,0.0)
                                                  //> t  : npbayes.wordseg.models.Unigram = npbayes.wordseg.models.Unigram@4e17e4c
                                                  //| a
	t.pypUni.base.logProb                     //> res0: Double = 0.0
  t.pypUni._oCount                                //> res1: Int = 0
  t.pypUni._tCount                                //> res2: Int = 0
  t.pypUni.logProb                                //> res3: Double = -2.995732273553991
  t.init(false)
  //t.pypUni._tCount
  //t.pypUni.base.logProb
  
  t.pypUni.logProb                                //> res4: Double = -416934.0198086627
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
  t.nUtterances                                   //> res5: Int = 9790
  t.nTokens                                       //> res6: Int = 52639
  val counts: HashMap[String,Double] = HashMap.empty
                                                  //> counts  : scala.collection.mutable.HashMap[String,Double] = Map()
  val tot=40                                      //> tot  : Int = 40
  for (i <- 0 until tot) {
   t.gibbsSweep()
   println(i+","+t.pypUni.logProb)
//  counts(t.data.getAnalysis)=
//     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
//   println(t.pypUni._oCount)
   println(t.pypUni._tCount)
   println(t.pypUni._oCount)
//   t.data.printAnalysis()
  }                                               //> 0,-295778.28648873576
                                                  //| 2928
                                                  //| 38941
                                                  //| 1,-268479.0339683743
                                                  //| 2546
                                                  //| 34492
                                                  //| 2,-262544.01632063725
                                                  //| 2425
                                                  //| 33757
                                                  //| 3,-259889.8962702701
                                                  //| 2348
                                                  //| 33596
                                                  //| 4,-258345.67077354382
                                                  //| 2307
                                                  //| 33541
                                                  //| 5,-256563.6200622363
                                                  //| 2264
                                                  //| 33471
                                                  //| 6,-255284.7068354888
                                                  //| 2226
                                                  //| 33489
                                                  //| 7,-253855.98927907646
                                                  //| 2190
                                                  //| 33435
                                                  //| 8,-252439.17739405908
                                                  //| 2154
                                                  //| 33373
                                                  //| 9,-251201.13480577542
                                                  //| 2128
                                                  //| 33328
                                                  //| 10,-249912.0253599946
                                                  //| 2084
                                                  //| 33343
                                                  //| 11,-248785.76220943837
                                                  //| 2063
                                                  //| 33256
                                                  //| 12,-248017.55868150614
                                                  //| 2042
                                                  //| 33261
                                                  //| 13,-247292.70109780418
                                                  //| 2024
                                                  //| 33261
                                                  //| 14,-246567.70333997632
                                                  //| 2001
                                                  //| 33285
                                                  //| 15,-245637.6075318375
                                                  //| 1970
                                                  //| 33247
                                                  //| 16,-244685.18769704783
                                                  //| 1949
                                                  //| 33201
                                                  //| 17,-243525.23704635826
                                                  //| 1919
                                                  //| 33153
                                                  //| 18,-242891.72816225272
                                                  //| 1901
                                                  //| 33166
                                                  //| 19,-241954.9120423337
                                                  //| 1882
                                                  //| 33113
                                                  //| 20,-241241.95267178386
                                                  //| 1866
                                                  //| 33093
                                                  //| 21,-240736.47433172414
                                                  //| 1850
                                                  //| 33100
                                                  //| 22,-240229.06572544962
                                                  //| 1841
                                                  //| 33111
                                                  //| 23,-239451.66995840316
                                                  //| 1827
                                                  //| 33047
                                                  //| 24,-238872.66425972639
                                                  //| 1809
                                                  //| 33041
                                                  //| 25,-238550.97705349198
                                                  //| 1798
                                                  //| 33044
                                                  //| 26,-237984.71441735924
                                                  //| 1783
                                                  //| 33036
                                                  //| 27,-237317.92937953753
                                                  //| 1773
                                                  //| 32993
                                                  //| 28,-236677.91770914439
                                                  //| 1742
                                                  //| 33005
                                                  //| 29,-236061.8321693142
                                                  //| 1731
                                                  //| 32956
                                                  //| 30,-235982.76632902876
                                                  //| 1717
                                                  //| 33053
                                                  //| 31,-235411.45259090333
                                                  //| 1712
                                                  //| 32998
                                                  //| 32,-234888.60882673808
                                                  //| 1702
                                                  //| 32956
                                                  //| 33,-234483.44589784925
                                                  //| 1691
                                                  //| 32922
                                                  //| 34,-233988.59249102866
                                                  //| 1679
                                                  //| 32891
                                                  //| 35,-233853.0977572417
                                                  //| 1676
                                                  //| 32948
                                                  //| 36,-233464.06269419484
                                                  //| 1671
                                                  //| 32906
                                                  //| 37,-233003.3213614953
                                                  //| 1655
                                                  //| 32887
                                                  //| 38,-232900.279947419
                                                  //| 1648
                                                  //| 32940
                                                  //| 39,-232510.8848311183
                                                  //| 1640
                                                  //| 32912
   //t.data.printAnalysis()
   
  for ((an,count) <- counts.toIndexedSeq.sortBy((x: (String,Double)) => -x._2))
    println(an+count/tot)
    
//  counts
  t.pypUni._oCount                                //> res7: Int = 32912
  t.pypUni._tCount                                //> res8: Int = 1640
  t.pypUni.base.logProb                           //> res9: Double = -25167.255066425863
  t.pypUni.logProb                                //> res10: Double = -232510.8848311183
//  t.pypUni.hmObsCounts
//  t.pypUni.hmTables
//  t.data.printAnalysis()
}