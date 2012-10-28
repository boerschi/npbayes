package npbayes.wordseg.data

import npbayes.distributions.CRP
import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.StringBuilder

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundaryDrop extends Boundary
case object WBoundaryNodrop extends Boundary
case object UBoundaryDrop extends Boundary
case object UBoundaryNodrop extends Boundary

class Context(val left: WordType,val w1Underlying: WordType, val w1Observed: WordType, val w1WithDrop: WordType, 
    val w2Underlying: WordType, val w2Observed: WordType, val right: WordType,
    val w1w2Underlying: WordType, val w1w2Observed: WordType) {
  override def toString =
    left.toString+w1Observed.toString+w2Observed.toString+right.toString
}


object SymbolTableString {
  val hmStoR = new HashMap[String,Int]
  val hmRtoS = new HashMap[Int,String]
  var nextR: Int = 0
  def nSymbols = hmStoR.size
  def getNextR: Int = {
    nextR = (nextR+1)
    (nextR-1).toInt
  }
  
  def apply(x: String): Int = {
    //println("retrieve Symbol: "+x)
    hmStoR.getOrElseUpdate(x,{val newId=getNextR; hmRtoS(newId)=x; newId})
  }
  
  def apply(x: Int): String =
    hmRtoS.getOrElse(x, throw new Error("Can't retrieve unknown value from Symbol-Table: " +x))
}

class VarData(fName: String, val dropProb: Double = 0.0,val MISSING: String = "*", val DROPSYMBOL: String = "T") {
	val UBOUNDARYSYMBOL="$"
	val UBOUNDARYWORD=Vector.empty[Int]:+SymbolTableString(UBOUNDARYSYMBOL)
	val DROPSEG=SymbolTableString(DROPSYMBOL)
	
	val (data: WordType,goldBoundaries: Array[Boundary]) = {
		var seqPhones: Vector[Int] = Vector.empty
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundaryNodrop
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
				seqPhones = seqPhones:+ SymbolTableString(c)
					seqBoundaries = seqBoundaries:+NoBoundary
				}
	      // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary
				if (SymbolTableString(seqPhones.last)==MISSING) {
					seqPhones = seqPhones.dropRight(1)
					seqBoundaries = seqBoundaries.dropRight(2):+WBoundaryDrop
				} else {
					seqBoundaries = seqBoundaries.dropRight(1):+WBoundaryNodrop
				}
			}
			seqBoundaries = seqBoundaries.last match {
				case WBoundaryDrop => seqBoundaries.dropRight(1):+UBoundaryDrop
				case WBoundaryNodrop => seqBoundaries.dropRight(1):+UBoundaryNodrop
			}	    
		}
		for (l <- Source.fromFile(fName).getLines) processLine(l)
		(seqPhones,seqBoundaries.toArray)
	}

	val _random = new Random()
	
	var boundaries = randomBoundaries.toArray
	
	def randomBoundaries = {
		var seqBoundaries: Vector[Boundary] = Vector.empty
		for (b <- goldBoundaries) {
			b match {
			case UBoundaryDrop | UBoundaryNodrop => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0) UBoundaryDrop else UBoundaryNodrop}
			case WBoundaryDrop | WBoundaryNodrop | NoBoundary => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0.5) NoBoundary else if (_random.nextDouble<dropProb) WBoundaryDrop else WBoundaryNodrop}
			}
		}
		seqBoundaries
	}
	
	def dropProb(s: WordType): Double = dropProb	//TODO - word-specific probabilities
	
	
	def setBoundary(pos: Int, b: Boundary): Unit = 
	  boundaries(pos)= b
	
	/**
	 * the noise function
	 * 
	 * P(s|u), but read: probability of realizing u as s, hence the order
	 */
	def R(u: WordType, s: WordType): Double = {
	  val res = SymbolTableString(u.last) match {
	  case DROPSYMBOL => {
	    if (u==s)
	      (1-dropProb(u))
	    else
	      if (u.init==s)
	        dropProb(u)
	      else
	        0.0
	  }
	  case _ =>
	    if (u==s)
	      1.0
	    else
	      0.0
	  }
	  res
	}
	
	
	def _wBound(op: Int=>Int)(cPos: Int): Int = boundaries(cPos) match {
    	case WBoundaryDrop | WBoundaryNodrop | UBoundaryDrop | UBoundaryNodrop => cPos
    	case NoBoundary =>  _wBound(op)(op(cPos))
	}
   

	def bToLeft: (Int=>Int) = _wBound(_-1)
	def bToRight: (Int=>Int) = _wBound(_+1)

	def getLeftWord(pos: Int): (WordType, WordType, WordType) = 
	  if (pos==0 || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			(UBOUNDARYWORD,null,null)
	  else {
		  val res: WordType =  data.slice(bToLeft(pos-1), pos)
		  val resWithDrop = res:+DROPSEG
		  boundaries(pos) match {	
		  	case UBoundaryDrop | WBoundaryDrop =>  
		  	  (resWithDrop,res,resWithDrop)
		  	case _ =>
		  	  (res,res,resWithDrop)
		  }
	  }
			
	
	def getRightWord(pos: Int): (WordType,WordType) = if (pos==(boundaries.size-1) || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			(UBOUNDARYWORD,null)
		else {
		  val end=bToRight(pos+1)
		  val res: WordType=data.slice(pos,end)
		  boundaries(end) match {
		    case UBoundaryDrop | WBoundaryDrop => 
		      (res:+DROPSEG,res)
		    case _ =>
		      (res,res)
		  }
		}
				    
	/**
	 * get all the tokens associated with a certain boundary    
	 */
	def context(pos: Int): Context = {
		val w1Start = bToLeft(pos-1)
		val w2end = bToRight(pos+1)
		val (w1Underlying,w1Observed,w1WithT) = getLeftWord(pos)
		val (w2Underlying,w2Observed) = getRightWord(pos)
		val w1w2Underlying = w1Observed++w2Underlying
		val w1w2Observed = w1Observed++w2Observed
		new Context(getLeftWord(w1Start)._1,w1Underlying,w1Observed,w1WithT,w2Underlying,w2Observed,
		    getRightWord(w2end)._1,w1w2Underlying,w1w2Observed)
	}
	

	def getAnalysis: String = {
	  def inner(sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (cPos>=boundaries.size)
	      res.toString
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1,res)
	      	case WBoundaryDrop => {
 	      	  res.append(data.slice(sPos-1, cPos).:+(SymbolTableString(DROPSYMBOL)).toString+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case WBoundaryNodrop => {
 	      	  res.append(data.slice(sPos-1, cPos).toString+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryDrop => {
	      	  res.append(data.slice(sPos-1, cPos).:+(SymbolTableString(DROPSYMBOL)).toString+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryNodrop => {
 	      	  res.append(data.slice(sPos-1, cPos).toString+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	    }
	  inner(1,1,new StringBuilder)
	}
	
	def printAnalysis(out: PrintStream = System.out) = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  out.print(data.slice(sPos-1, cPos).:+(SymbolTableString(DROPSYMBOL)).toString+"::")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print(data.slice(sPos-1, cPos).toString+"::")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print(data.slice(sPos-1, cPos).:+(SymbolTableString(DROPSYMBOL)).toString+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
 	      	  out.print(data.slice(sPos-1, cPos).toString+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	    }
	  inner(1,1)
	}
	
}