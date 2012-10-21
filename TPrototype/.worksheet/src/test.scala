import npbayes.distributions._
import npbayes.wordseg.VarData
object test {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(135); 
	val test = new CRP[Word](0.001,0,new Monkey[Word](50,0.5));System.out.println("""test  : npbayes.distributions.CRP[npbayes.distributions.Word] = """ + $show(test ));$skip(19); val res$0 = 
  test.hmObsCounts;System.out.println("""res0: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$0));$skip(26); val res$1 = 
  test.update(Word("Ha"));System.out.println("""res1: Double = """ + $show(res$1));$skip(19); val res$2 = 
  test.hmObsCounts;System.out.println("""res2: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$2));$skip(15); val res$3 = 
	test.hmTables;System.out.println("""res3: scala.collection.mutable.HashMap[npbayes.distributions.Word,Vector[Int]] = """ + $show(res$3));$skip(28); val res$4 = 
  test.predProb(Word("Ha"));System.out.println("""res4: Double = """ + $show(res$4));$skip(26); val res$5 = 
  test.update(Word("Ha"));System.out.println("""res5: Double = """ + $show(res$5));$skip(19); val res$6 = 
  test.hmObsCounts;System.out.println("""res6: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$6));$skip(28); val res$7 = 
  test.predProb(Word("Ha"));System.out.println("""res7: Double = """ + $show(res$7));$skip(26); val res$8 = 
  test.update(Word("Ha"));System.out.println("""res8: Double = """ + $show(res$8));$skip(28); val res$9 = 
  test.predProb(Word("Ha"));System.out.println("""res9: Double = """ + $show(res$9));$skip(27); val res$10 = 

  test.update(Word("Ha"));System.out.println("""res10: Double = """ + $show(res$10));$skip(28); val res$11 = 
  test.predProb(Word("Ha"));System.out.println("""res11: Double = """ + $show(res$11));$skip(27); val res$12 = 

  test.update(Word("Ha"));System.out.println("""res12: Double = """ + $show(res$12));$skip(28); val res$13 = 
  test.predProb(Word("Ha"));System.out.println("""res13: Double = """ + $show(res$13));$skip(27); val res$14 = 

  test.update(Word("Ha"));System.out.println("""res14: Double = """ + $show(res$14));$skip(78); val res$15 = 
                                                 
  test.predProb(Word("Ha"));System.out.println("""res15: Double = """ + $show(res$15));$skip(19); val res$16 = 
  test.hmObsCounts;System.out.println("""res16: scala.collection.mutable.HashMap[npbayes.distributions.Word,Int] = """ + $show(res$16));$skip(44); val res$17 = 
//  test.hmTables
  test.remove(Word("Ha"));System.out.println("""res17: Double = """ + $show(res$17));$skip(44); 
  println("Welcome to the Scala worksheet");$skip(17); 
	val x = "Hallo";System.out.println("""x  : java.lang.String = """ + $show(x ));$skip(17); 
	val y = "Hallo";System.out.println("""y  : java.lang.String = """ + $show(y ));$skip(13); val res$18 = 
	x.equals(y);System.out.println("""res18: Boolean = """ + $show(res$18));$skip(84); 
	val small = new VarData("/home/bborschi/software/ScalaIDE/research/TPrototype/sm");System.out.println("""small  : npbayes.wordseg.VarData = """ + $show(small ));$skip(23); val res$19 = 
  small.goldBoundaries;System.out.println("""res19: Vector[npbayes.wordseg.Boundary] = """ + $show(res$19));$skip(66); 
  small.data.foreach((x: Short) => println(small.symbolTable(x)));$skip(23); val res$20 = 
  small.goldBoundaries;System.out.println("""res20: Vector[npbayes.wordseg.Boundary] = """ + $show(res$20));$skip(19); val res$21 = 
  small.boundaries;System.out.println("""res21: Vector[npbayes.wordseg.Boundary] = """ + $show(res$21))}
}