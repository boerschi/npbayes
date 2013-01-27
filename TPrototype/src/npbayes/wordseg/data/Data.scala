package npbayes.wordseg.data

/**
 * we define a simple beta-prior over the application of each variation rule. consequently, we
 * need to track the number of times with which we observed each individual rule applying / not applying
 */

import npbayes.distributions.CRP
import java.io._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Random
import npbayes.wordseg.models.Unigram
import scala.collection.mutable.StringBuilder
import com.google.common.collect.ImmutableList.Builder
import java.util.Arrays


abstract class Boundary
case object NoBoundary extends Boundary
case object WBoundaryDrop extends Boundary
case object WBoundaryNodrop extends Boundary
case object UBoundaryDrop extends Boundary
case object UBoundaryNodrop extends Boundary

class Identifier(f: String) {
  var itemsT: Set[SegmentType] = Set[SegmentType]()
  for (l <- scala.io.Source.fromFile(f).getLines)
    itemsT = itemsT+SymbolTable(l)
  
  def apply(x: SegmentType): Boolean =
    itemsT.contains(x)
}



/**
 * variable data --- allows for dropping a single segment at the end of a word
 * keeps boundary information and provides word-extraction functionality
 */
class VarData(fName: String, val dropProb: Double = 0.0,val MISSING: String = "*", val DROPSYMBOL: String = "T") {
	val isConsonant = new Identifier("/home/bborschi/git/TDropping/consonants.txt")
 	val isVowel = new Identifier("/home/bborschi/git/TDropping/vowels.txt")
	val isPause = new Identifier("/home/bborschi/git/TDropping/silences.txt")	
    val UBOUNDARYSYMBOL="UTTERANCEBOUNDARY"
	val UBOUNDARYWORD=segToWord(SymbolTable(UBOUNDARYSYMBOL))
	val DROPSEG=SymbolTable(DROPSYMBOL)
	
	/**
	 * Initialize the data and goldBoundaries
	 */
	val (data: WordType,goldBoundaries: Array[Boundary]) = {
		var seqPhones = Vector.empty[Int]
		var seqBoundaries: Vector[Boundary] = Vector.empty:+UBoundaryNodrop
		def processLine(line: String) = {
			for (w <- line.stripLineEnd.split("\t")) {
				for (c: String <- w.split(" ")) {
				seqPhones = seqPhones:+ SymbolTable(c)
					seqBoundaries = seqBoundaries:+NoBoundary
				}
	      // adjust for word-boundaries --- last NoBoundary is in fact a word-boundary
				if (SymbolTable(seqPhones.last)==MISSING) {
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
		val phones = new Builder[Int]
		for (x <- seqPhones)
		  phones.add(x)
		(phones.build,seqBoundaries.toArray)
	}

	val _random = new Random()
	
	var boundaries = randomBoundaries.toArray
	
	/**
	 * randomize boundaries
	 */
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
	
	
	def dropProb(s: WordType, rContext: SegmentType): Double = dropProb
	
	def dropProbContext(s: WordType, rContext: SegmentType): Double = /*dropProb*/ {
	  val lContext = s.get(s.size()-2)
	  if (isConsonant(rContext))
	    if (isConsonant(lContext))
	    	0.68
	    else
	    	0.20
	  else
	    if (isVowel(rContext))
	      if (isConsonant(lContext))
	    	0.51
	      else
	        0.13
	    else
	      if (isPause(rContext))
	        if (isConsonant(lContext))
	        	0.44
	        else
	          0.06
	      else
	        throw new Error("Unknown Segment "+SymbolTable(rContext))
	}//dropProb	//TODO - word-specific probabilities  */
	
	
	def setBoundary(pos: Int, b: Boundary): Unit = 
	  boundaries(pos)= b
	
	/**
	 * the noise function
	 * 
	 * P(s|u), but read: probability of realizing u as s, hence the order
	 */
	def R(u: WordType, s: WordType, rWord: WordType): Double = {
	  val res = SymbolTable(u.get(u.size-1)) match {
	  case DROPSYMBOL =>
	    val rContext = rWord.get(rWord.size()-1)
	    if (u==s)
	      (1-dropProb(u,rContext))
	    else
	      if (u.subList(0,u.size-1)==s)
	        dropProb(u,rContext)
	      else
	        0.0
	  case _ =>
	    if (u==s)
	      1.0
	    else
	      0.0
	  }
	  res
	}
	
	
	
	
	def _findBoundary(op: Int=>Int)(cPos: Int): Int = boundaries(cPos) match {
    	case WBoundaryDrop | WBoundaryNodrop | UBoundaryDrop | UBoundaryNodrop => cPos
    	case NoBoundary =>  _findBoundary(op)(op(cPos))
	}
   

	def boundaryToLeft: (Int=>Int) = _findBoundary(_-1)
	def boundaryToRight: (Int=>Int) = _findBoundary(_+1)

	/**
	 * returns a triple (observed,underlying,withDrop)
	 */
	def getWordWithVar(sPos: Int, ePos: Int): (WordType,WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  val wD = suffix(word,DROPSEG)
	  boundaries(ePos) match {
	    case UBoundaryDrop | WBoundaryDrop =>
	      (word,wD,wD)
	    case _ =>
	      (word,word,wD)
	  }
	}
	
	/**
	 * returns a tuple (observed,underlying)
	 * 
	 * this may save the additional cost of creating a copy of the token with an additional dropsegment
	 */
	def getWord(sPos: Int, ePos: Int): (WordType,WordType) = {
	  val word = data.subList(sPos, ePos)
	  boundaries(ePos) match {
	    case UBoundaryDrop | WBoundaryDrop =>
	      (word,suffix(word,DROPSEG))
	    case _ =>
	      (word,word)
	  }
	}


	def getAnalysis: String = {
	  def inner(sPos: Int,cPos: Int,res: StringBuilder): String = 
	    if (cPos>=boundaries.size)
	      res.toString
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1,res)
	      	case WBoundaryDrop => {
 	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case WBoundaryNodrop => {
 	      	  res.append(wToS(data.subList(sPos-1, cPos))+"::")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryDrop => {
	      	  res.append(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	      	case UBoundaryNodrop => {
 	      	  res.append(wToS(data.subList(sPos-1, cPos))+"\n")
 	      	  inner(cPos+1,cPos+1,res)
	      	}
	    }
	  inner(1,1,new StringBuilder)
	}
	
	def printAnalysis(out: PrintStream = System.out,sep: String=" ") = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  out.print(
 	      	      wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
 	      	  out.print(wToS(data.subList(sPos-1, cPos))+sep)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  out.print(wToS(suffix(data.subList(sPos-1, cPos),DROPSEG))+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryNodrop => {
 	      	  out.print(wToS(data.subList(sPos-1, cPos))+"\n")
 	      	  inner(cPos+1,cPos+1)
	      	}
	    }
	  inner(1,1)
	}
	
	def evaluate: Result = {
		var totalBoundaries = 0;	//boundaries the learner predicts
		var trueBoundaries = 0;		//boundaries in the gold
		var correctBoundaries = 0;	//boundaries the learner gets correct
		var totalTokens = 0;		//tokens the learner predicts
		var trueTokens = 0;			//tokens in the gold
		var correctTokens = 0;		//tokens the learner gets correct
		var totalDrops = 0;
		var trueDrops = 0;
		var correctDrops = 0;
//		HashMap<ImmutableList<Short>,Integer> proposedLexicon = new HashMap<ImmutableList<Short>,Integer>();	//words in the proposed segmentation
//		ashMap<ImmutableList<Short>,Integer> trueLexicon = new HashMap<ImmutableList<Short>, Integer>();		//words in the true segmentation
		var trueStartPos=0
		var predStartPos=0		
		for (i <- 1 to boundaries.size-1) {
		  boundaries(i) match {
		    case NoBoundary => {
		      goldBoundaries(i) match {
		        case WBoundaryDrop =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i
		          trueDrops+=1
		        case WBoundaryNodrop =>
		          trueBoundaries+=1
		          trueTokens+=1
		          trueStartPos=i		          
		        case _ =>
		      }
		    }
		    case WBoundaryDrop => {
		      totalBoundaries+=1
		      totalTokens+=1
		      totalDrops+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop | WBoundaryNodrop => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          if (predStartPos==trueStartPos && goldBoundaries(i)==boundaries(i)) correctTokens+=1
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop) {
		            trueDrops+=1
		            correctDrops+=1
		          }
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case WBoundaryNodrop => {
		      totalBoundaries+=1
		      totalTokens+=1
		      goldBoundaries(i) match {
		        case WBoundaryDrop | WBoundaryNodrop => {
		          trueBoundaries+=1
		          correctBoundaries+=1
		          trueTokens+=1
		          if (predStartPos==trueStartPos) correctTokens+=1
		          trueStartPos=i
		          if (goldBoundaries(i)==WBoundaryDrop) {
		            trueDrops+=1
		          }		          
		        }
		        case _ =>
		      }
		      predStartPos=i
		    }
		    case UBoundaryDrop | UBoundaryNodrop => {
		      totalTokens+=1
		      trueTokens+=1
		      if (boundaries(i)==UBoundaryDrop) totalDrops+=1
		      if (goldBoundaries(i)==UBoundaryDrop) {
		        trueDrops+=1
		        correctDrops+= {if (goldBoundaries(i)==boundaries(i)) 1 else 0}
		      }
		      if (predStartPos==trueStartPos  && goldBoundaries(i)==boundaries(i)) correctTokens+=1
		      predStartPos=i
		      trueStartPos=i
		    }
		  }
		}
		new Result(correctTokens.toFloat/totalTokens,correctTokens.toFloat/trueTokens,
		           correctBoundaries.toFloat/totalBoundaries,correctBoundaries.toFloat/trueBoundaries,
		           correctDrops.toFloat/totalDrops,correctDrops.toFloat/trueDrops)
	}
	
}