import npbayes.distributions._
import npbayes.wordseg.models.Unigram
object test {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(153); 
	val t = new Unigram("/home/bborschi/git/TDropping/TPrototype/sm",1,0);System.out.println("""t  : npbayes.wordseg.models.Unigram = """ + $show(t ));$skip(15); 
  t.init(true);$skip(23); val res$0 = 
  t.pypUni.hmObsCounts;System.out.println("""res0: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$0));$skip(25); 
  t.data.printAnalysis();$skip(27); 
  val c =t.data.context(2);System.out.println("""c  : npbayes.wordseg.data.Context = """ + $show(c ));$skip(9); val res$1 = 
  c.left;System.out.println("""res1: npbayes.distributions.Word = """ + $show(res$1));$skip(7); val res$2 = 
  c.w1;System.out.println("""res2: npbayes.distributions.Word = """ + $show(res$2));$skip(7); val res$3 = 
  c.w2;System.out.println("""res3: npbayes.distributions.Word = """ + $show(res$3));$skip(10); val res$4 = 
  c.right;System.out.println("""res4: npbayes.distributions.Word = """ + $show(res$4))}
}