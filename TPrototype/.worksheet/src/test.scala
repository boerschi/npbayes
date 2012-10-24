import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(208); 
	val t = new Unigram("/home/bborschi/git/TDropping/TPrototype/tiny",1.0,0,EXACT,0.5);System.out.println("""t  : npbayes.wordseg.models.Unigram = """ + $show(t ));$skip(14); val res$0 = 
  t.data.data;System.out.println("""res0: Vector[Short] = """ + $show(res$0));$skip(19); val res$1 = 
  t.pypUni.logProb;System.out.println("""res1: Double = """ + $show(res$1));$skip(16); 
  t.init(false);$skip(19); val res$2 = 
  t.pypUni._tCount;System.out.println("""res2: Int = """ + $show(res$2));$skip(24); val res$3 = 
  t.pypUni.base.logProb;System.out.println("""res3: Double = """ + $show(res$3));$skip(22); val res$4 = 
  
  t.pypUni.logProb;System.out.println("""res4: Double = """ + $show(res$4));$skip(28); val res$5 = 
  t.data.symbolTable.hmStoR;System.out.println("""res5: scala.collection.mutable.HashMap[String,Short] = """ + $show(res$5));$skip(23); val res$6 = 
  t.pypUni.hmObsCounts;System.out.println("""res6: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$6));$skip(25); 
  t.data.printAnalysis();$skip(20); val res$7 = 
  t.pypUni.hmTables;System.out.println("""res7: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[Int]] = """ + $show(res$7));$skip(21); val res$8 = 
  t.data.context(11);System.out.println("""res8: npbayes.wordseg.data.Context = """ + $show(res$8));$skip(88); val res$9 = 
//  t.pypUni.hmObsCounts
//  t.pypUni(Word("w e")+Word("l t"))

  t.pypUni.base.logProb;System.out.println("""res9: Double = """ + $show(res$9));$skip(24); val res$10 = 
  
  t._predBoundary(0);System.out.println("""res10: Double = """ + $show(res$10));$skip(12); val res$11 = 
  t.nTokens;System.out.println("""res11: Int = """ + $show(res$11));$skip(16); val res$12 = 
  t.nUtterances;System.out.println("""res12: Int = """ + $show(res$12));$skip(25); 
  t.data.printAnalysis();$skip(16); val res$13 = 
  t.nUtterances;System.out.println("""res13: Int = """ + $show(res$13));$skip(12); val res$14 = 
  t.nTokens;System.out.println("""res14: Int = """ + $show(res$14));$skip(53); 
  val counts: HashMap[String,Double] = HashMap.empty;System.out.println("""counts  : scala.collection.mutable.HashMap[String,Double] = """ + $show(counts ));$skip(17); 
  val tot=100000;System.out.println("""tot  : Int = """ + $show(tot ));$skip(260); 
  for (i <- 0 until tot) {
   t.gibbsSweep()
   counts(t.data.getAnalysis)=
     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
  
//   println(t.pypUni._oCount)
//   println(t.pypUni._tCount)
//   println(t.pypUni.base.logProb)
//   t.data.printAnalysis()
  };$skip(133); 
  for ((an,count) <- counts.toIndexedSeq.sortBy((x: (String,Double)) => -x._2))
    println(an+count/tot);$skip(35); val res$15 =                          //
    
//  counts
  t.pypUni._oCount;System.out.println("""res15: Int = """ + $show(res$15));$skip(19); val res$16 = 
  t.pypUni._tCount;System.out.println("""res16: Int = """ + $show(res$16));$skip(24); val res$17 = 
  t.pypUni.base.logProb;System.out.println("""res17: Double = """ + $show(res$17));$skip(19); val res$18 = 
  t.pypUni.logProb;System.out.println("""res18: Double = """ + $show(res$18));$skip(23); val res$19 = 
  t.pypUni.hmObsCounts;System.out.println("""res19: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$19));$skip(20); val res$20 = 
  t.pypUni.hmTables;System.out.println("""res20: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[Int]] = """ + $show(res$20));$skip(25); 
  t.data.printAnalysis()}
}