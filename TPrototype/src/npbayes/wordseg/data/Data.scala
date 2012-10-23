package npbayes.wordseg.data

import npbayes.distributions.{Word,CRP}
import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random
import npbayes.wordseg.models.Unigram

abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundaryDrop extends Boundary
case object WBoundaryNodrop extends Boundary
case object UBoundaryDrop extends Boundary
case object UBoundaryNodrop extends Boundary

class Context(val left: Word,val w1: Word, val w2: Word, val right: Word)


object SymbolTableString {
  val hmStoR = new HashMap[String,Short]
  val hmRtoS = new HashMap[Short,String]
  var nextR: Short = 0
  def nSymbols = hmStoR.size
  def getNextR: Short = {
    nextR = (nextR+1).toShort
    (nextR-1).toShort
  }
  
  def apply(x: String): Short = 
    hmStoR.getOrElseUpdate(x,{val newId=getNextR; hmRtoS(newId)=x; newId})
  
  def apply(x: Short): String =
    hmRtoS.getOrElse(x, throw new Error("Can't retrieve unknown value from Symbol-Table: " +x))
}

class VarData(fName: String, val MISSING: String = "*", val DROPSYMBOL: String = "T") {
	val UBOUNDARYSYMBOL="$"
	val symbolTable = SymbolTableString
	val (data,goldBoundaries) = readCorpus
	val _random = new Random()
	var boundaries = randomBoundaries
	def randomBoundaries = {
		var seqBoundaries: Vector[Boundary] = Vector.empty
		for (b <- goldBoundaries) {
			b match {
			case UBoundaryDrop | UBoundaryNodrop => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0.5) UBoundaryDrop else UBoundaryNodrop}
			case WBoundaryDrop | WBoundaryNodrop | NoBoundary => seqBoundaries=seqBoundaries:+{if (_random.nextDouble<0.33) WBoundaryDrop else if (_random.nextDouble<0.5) WBoundaryNodrop else NoBoundary}
			}
		}
		seqBoundaries
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

	def getLeftWord(pos: Int): Word = if (pos==0 || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			UBOUNDARYSYMBOL
		else 
		  boundaries(pos) match {
		  	case UBoundaryDrop | WBoundaryDrop => 
		  	  Word.constructWord(data.slice(bToLeft(pos-1), pos):+(symbolTable(DROPSYMBOL)))
		  	case _ =>
		  	  Word.constructWord(data.slice(bToLeft(pos-1), pos))
		}
			
	
	def getRightWord(pos: Int): Word = if (pos==(boundaries.size-1) || boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
			UBOUNDARYSYMBOL
		else {
		  val end=bToRight(pos+1)
		  boundaries(end) match {
		    case UBoundaryDrop | WBoundaryDrop =>
		      Word.constructWord(data.slice(pos,end):+(symbolTable(DROPSYMBOL)))
		    case _ =>
		      Word.constructWord(data.slice(pos,end))
		  }
		}
				    
	    
	def context(pos: Int): Context = {
		val w1Start = bToLeft(pos-1)
		println(w1Start)
		val w2end = bToRight(pos+1)
		println(w2end)
		new Context(getLeftWord(w1Start),getLeftWord(pos),getRightWord(pos),getRightWord(w2end))
	}
	

	def printAnalysis(out: PrintStream = System.out) = {
	  def inner(remData: Vector[Boundary],sPos: Int,cPos: Int): Unit = 
	    if (remData.size==0)
	      Unit
	    else 
	      remData.head match {
	      	case NoBoundary => inner(remData.tail,sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL)))+"\t")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos))+"\t")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print(Word(data.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL)))+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
 	      	  out.print(Word(data.slice(sPos-1, cPos))+"\n")
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	    }
	  inner(boundaries.tail,1,1)
	}
	
}