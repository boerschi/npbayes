import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(210); 
	val t = new Unigram("/home/bborschi/data/brent/br-phono_spaced.txt",20,0,MINPATH,0.0);System.out.println("""t  : npbayes.wordseg.models.Unigram = """ + $show(t ));$skip(23); val res$0 = 
	t.pypUni.base.logProb;System.out.println("""res0: Double = """ + $show(res$0));$skip(19); val res$1 = 
  t.pypUni._oCount;System.out.println("""res1: Int = """ + $show(res$1));$skip(19); val res$2 = 
  t.pypUni._tCount;System.out.println("""res2: Int = """ + $show(res$2));$skip(19); val res$3 = 
  t.pypUni.logProb;System.out.println("""res3: Double = """ + $show(res$3));$skip(16); 
  t.init(false);$skip(69); val res$4 = 
  //t.pypUni._tCount
  //t.pypUni.base.logProb
  
  t.pypUni.logProb;System.out.println("""res4: Double = """ + $show(res$4));$skip(318); val res$5 = 
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
  t.nUtterances;System.out.println("""res5: Int = """ + $show(res$5));$skip(12); val res$6 = 
  t.nTokens;System.out.println("""res6: Int = """ + $show(res$6));$skip(53); 
  val counts: HashMap[String,Double] = HashMap.empty;System.out.println("""counts  : scala.collection.mutable.HashMap[String,Double] = """ + $show(counts ));$skip(13); 
  val tot=40;System.out.println("""tot  : Int = """ + $show(tot ));$skip(286); 
  for (i <- 0 until tot) {
   t.gibbsSweep()
   println(i+","+t.pypUni.logProb)
//  counts(t.data.getAnalysis)=
//     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
//   println(t.pypUni._oCount)
   println(t.pypUni._tCount)
   println(t.pypUni._oCount)
//   t.data.printAnalysis()
  };$skip(138); 
   //t.data.printAnalysis()
   
  for ((an,count) <- counts.toIndexedSeq.sortBy((x: (String,Double)) => -x._2))
    println(an+count/tot);$skip(35); val res$7 = 
    
//  counts
  t.pypUni._oCount;System.out.println("""res7: Int = """ + $show(res$7));$skip(19); val res$8 = 
  t.pypUni._tCount;System.out.println("""res8: Int = """ + $show(res$8));$skip(24); val res$9 = 
  t.pypUni.base.logProb;System.out.println("""res9: Double = """ + $show(res$9));$skip(19); val res$10 = 
  t.pypUni.logProb;System.out.println("""res10: Double = """ + $show(res$10))}
//  t.pypUni.hmObsCounts
//  t.pypUni.hmTables
//  t.data.printAnalysis()
}