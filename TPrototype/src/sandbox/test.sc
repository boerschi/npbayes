import npbayes.distributions._
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.HashMap

object test {
	val t = new Unigram("/home/bborschi/data/brent/br-phono_spaced.txt",20,0,MINPATH,0.0)
                                                  //> java.lang.NoClassDefFoundError: npbayes/wordseg/models/Unigram
                                                  //| 	at test$$anonfun$main$1.apply$mcV$sp(test.scala:6)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at test$.main(test.scala:5)
                                                  //| 	at test.main(test.scala)
                                                  //| Caused by: java.lang.ClassNotFoundException: npbayes.wordseg.models.Unigram
                                                  //| 	at java.net.URLClassLoader$1.run(URLClassLoader.java:217)
                                                  //| 	at java.security.AccessController.doPrivileged(Native Method)
                                                  //| 	at java.net.URLClassLoader.findClass(URLClassLoader.java:205)
                                                  //| 	at java.lang.ClassLoader.loadClass(ClassLoader.java:321)
                                                  //| 	at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:294)
                                                  //| 	at java.lang.ClassLoader.loadClass(ClassLoader.java:266)
                                                  //| 	... 6 more
	t.pypUni.base.logProb
  t.pypUni._oCount
  t.pypUni._tCount
  t.pypUni.logProb
  t.init(false)
  //t.pypUni._tCount
  //t.pypUni.base.logProb
  
  t.pypUni.logProb
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
  t.nUtterances
  t.nTokens
  val counts: HashMap[String,Double] = HashMap.empty
  val tot=40
  for (i <- 0 until tot) {
   t.gibbsSweep()
   println(i+","+t.pypUni.logProb)
//  counts(t.data.getAnalysis)=
//     counts.getOrElse(t.data.getAnalysis, 0.0)+1.0
//   println(t.pypUni._oCount)
   println(t.pypUni._tCount)
   println(t.pypUni._oCount)
//   t.data.printAnalysis()
  }
   //t.data.printAnalysis()
   
  for ((an,count) <- counts.toIndexedSeq.sortBy((x: (String,Double)) => -x._2))
    println(an+count/tot)
    
//  counts
  t.pypUni._oCount
  t.pypUni._tCount
  t.pypUni.base.logProb
  t.pypUni.logProb
//  t.pypUni.hmObsCounts
//  t.pypUni.hmTables
//  t.data.printAnalysis()
}