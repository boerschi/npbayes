package npbayes.wordseg.data

import npbayes.distributions.Word
import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random

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
  
  def getNextR: Short = {
    nextR = (nextR+1).toShort
    (nextR-1).toShort
  }
  
  def apply(x: String): Short = 
    hmStoR.getOrElseUpdate(x,{val newId=getNextR; hmRtoS(newId)=x; newId})
  
  def apply(x: Short): String =
    hmRtoS.getOrElse(x, throw new Error("Can't retrieve unknown value from Symbol-Table: " +x))
}

class VarData(fName: String, val MISSING: String = "*") {
	val symbolTable = SymbolTableString
	val _data = readCorpus
	val _random = new Random()
	def data = _data._1
	def goldBoundaries = _data._2
	def boundaries = randomBoundaries
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
	  var seqBoundaries: Vector[Boundary] = Vector.empty	  
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
	  for (l <- Source.fromFile(fName).getLines) {
	    processLine(l)
	  }
	  (seqPhones,seqBoundaries)
	}
	
   def _wBound(op: Int=>Int)(cPos: Int): Int = boundaries(cPos) match {
    	case WBoundaryDrop | WBoundaryNodrop | UBoundaryDrop | UBoundaryNodrop => cPos
    	case NoBoundary => _wBound(op)(op(cPos))
   }

   def bToLeft: (Int=>Int) = _wBound(_-1)
   def bToRight: (Int=>Int) = _wBound(_+1)
   
   
	def getLeftWord(pos: Int): Word = {
	  
	}
	
	def context(pos: Int): Context = {
		  }
	  val wlStart = wBound(pos-)
	}
}