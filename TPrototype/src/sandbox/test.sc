import npbayes.distributions._
import npbayes.wordseg.models.Unigram
object test {
	val t = new Unigram("/home/bborschi/git/TDropping/TPrototype/sm",1,0)
                                                  //> t  : npbayes.wordseg.models.Unigram = npbayes.wordseg.models.Unigram@3934f69
                                                  //| a
  t.init(true)
  t.pypUni.hmObsCounts                            //> res0: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = Map
                                                  //| (tehst -> 1, ayn -> 1, nohch -> 1, tehsT -> 1, aynT -> 1)
  t.data.printAnalysis()                          //> aynT	tehst
                                                  //| nohch	ayn	tehsT
  val c =t.data.context(2)                        //> 0
                                                  //| 6
                                                  //| c  : npbayes.wordseg.data.Context = npbayes.wordseg.data.Context@2b275d39
  c.left                                          //> res1: npbayes.distributions.Word = $
  c.w1                                            //> res2: npbayes.distributions.Word = aynT
  c.w2                                            //> res3: npbayes.distributions.Word = tehst
  c.right                                         //> res4: npbayes.distributions.Word = $
}