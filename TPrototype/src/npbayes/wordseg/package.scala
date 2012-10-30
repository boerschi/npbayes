package npbayes.wordseg

import scala.collection.mutable.HashMap

class Result(val tp: Double,val tr: Double, val bp: Double, val br: Double, val lp: Double, val lr: Double) {
  val tf = 2*tp*tr/(tp+tr)
  val bf = 2*bp*br/(bp+br)
  val lf = 2*lp*lr/(lp+lr)
  
  override def toString =
    tf+" "+tp+" "+tr+" "+bf+" "+bp+" "+br
}

object `package` {
	
}