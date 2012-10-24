package npbayes.wordseg.data

import npbayes.distributions.{Word,CRP}
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

class Context(val left: Word,val w1: (Word,Word), val w2: (Word,Word), val right: Word) {
  def w1Observed = w1._2
  def w2Observed = w2._2
  def w1Underlying = w1._1
  def w2Underlying = w2._1
  override def toString =
    left.toString+w1.toString+w2.toString+right.toString
}


object SymbolTableString {
  val hmStoR = new HashMap[String,Short]
  val hmRtoS = new HashMap[Short,String]
  var nextR: Short = 0
  def nSymbols = hmStoR.size
  def getNextR: Short = {
    nextR = (nextR+1).toShort
    (nextR-1).toShort
  }
  
  def apply(x: String): Short = {
    //println("retrieve Symbol: "+x)
    hmStoR.getOrElseUpdate(x,{val newId=getNextR; hmRtoS(newId)=x; newId})
  }
  
  def apply(x: Short): String =
    hmRtoS.getOrElse(x, throw new Error("Can't retrieve unknown value from Symbol-Table: " +x))
}

class VarData(fName: String, val dropProb: Double = 0.0,val MISSING: String = "*", val DROPSYMBOL: String = "T") {
	val UBOUNDARYSYMBOL="$"
	val symbolTable = SymbolTableString
	val (data,goldBoundaries) = readCorpus
	val _random = new Random()
	var boundaries = randomBoundaries
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
	
	def dropProb(s: Word): Double = dropProb	//TODO - word-specific probabilities
	
	
	def setBoundary(pos: Int, b: Boundary): Unit = 
	  boundaries=boundaries.updated(pos, b)
	
	
	def R(u: Word, s: Word): Double = {
	  val res = symbolTable(u.phones.last) match {
	  case DROPSYMBOL => {
	    if (u==s)
	      (1-dropProb(u))
	    else
	      if (u.phones.init==s.phones)
	        dropProb(u)
	      else
	        0.0
	  }
	  case _ =>
	    if (u.phones==s.phones)
	      1.0
	    else
	      0.0
	}
//	 println("P("+s+"|"+u+")="+res)
	 res
	}
	
	def readCorpus = {
		var seqPhones: Vector[Short] = Vector.empty
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundaryNodrop
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
					seqPhones = seqPhones:+ symbolTable(c)
					seqBoundaries = seqBoundaries:+NoBoundary
				}
	      // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary
				if (symbolTable(seqPhones.last)==MISSING) {
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
		(seqPhones,seqBoundaries)
	}
	
	def _wBound(op: Int=>Int)(cPos: Int): Int = boundaries(cPos) match {
    	case WBoundaryDrop | WBoundaryNodrop | UBoundaryDrop | UBoundaryNodrop => cPos
    	case NoBoundary =>  _wBound(op)(op(cPos))
	}
   

	def bToLeft: (Int=>Int) = _wBound(_-1)
	def bToRight: (Int=>Int) = _wBound(_+1)

	def getLeftWord(pos: Int): (Word, Word) = if (pos==0 || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			(UBOUNDARYSYMBOL,null)
		else {
		  val res: Word =  Word.getWordOrConstruct(data.slice(bToLeft(pos-1), pos))
		
		  boundaries(pos) match {	
		  	case UBoundaryDrop | WBoundaryDrop => 
		  	  (res+DROPSYMBOL,res)
		  	case _ =>
		  	  (res,res)
		  }
		}
			
	
	def getRightWord(pos: Int): (Word,Word) = if (pos==(boundaries.size-1) || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			(UBOUNDARYSYMBOL,null)
		else {
		  val end=bToRight(pos+1)
		  val res: Word=Word.getWordOrConstruct(data.slice(pos,end))
		  boundaries(end) match {
		    case UBoundaryDrop | WBoundaryDrop => 
		      (res+DROPSYMBOL,res)
		    case _ =>
		      (res,res)
		  }
		}
				    
	    
	def context(pos: Int): Context = {
		val w1Start = bToLeft(pos-1)
		val w2end = bToRight(pos+1)
		new Context(getLeftWord(w1Start)._1,getLeftWord(pos),getRightWord(pos),getRightWord(w2end)._1)
	}
	

	def getAnalysis: String = {
	  def inner(remData: Vector[Boundary],sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (remData.size==0)
	      res.toString
	    else 
	      remData.head match {
	      	case NoBoundary => inner(remData.tail,sPos,cPos+1,res)
	      	case WBoundaryDrop => {
 	      	  res.append(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))).toString+"::")
 	      	  inner(remData.tail,cPos+1,cPos+1,res)
	      	}
	      	case WBoundaryNodrop => {
 	      	  res.append(Word(data.slice(sPos-1, cPos)).toString+"::")
 	      	  inner(remData.tail,cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryDrop => {
	      	  res.append(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))).toString+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryNodrop => {
 	      	  res.append(Word(data.slice(sPos-1, cPos)).toString+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1,res)
	      	}
	    }
	  inner(boundaries.tail,1,1,new StringBuilder)
	}
	
	def printAnalysis(out: PrintStream = System.out) = {
	  def inner(remData: Vector[Boundary],sPos: Int,cPos: Int): Unit = 
	    if (remData.size==0)
	      Unit
	    else 
	      remData.head match {
	      	case NoBoundary => inner(remData.tail,sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))).toString+"::")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos)).toString+"::")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))).toString+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos)).toString+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	    }
	  inner(boundaries.tail,1,1)
	}
	
}