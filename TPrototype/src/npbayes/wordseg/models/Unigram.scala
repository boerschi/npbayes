package npbayes.wordseg.models
/**
 * Goldwater-style Unigram model
 */


import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.data.WBoundaryNodrop
import scala.util.Random.shuffle

class Unigram(val corpusName: String,concentration: Double,discount: Double=0,val assumption: HEURISTIC = EXACT,val dropProb: Double =0.0) {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName,dropProb,"*","t")
	val pypUni = new CRP[Word](concentration,discount,new Monkey(data.symbolTable.nSymbols,0.5),assumption)
	var nUtterances = 0

	// local aliases
	def boundaries = data.boundaries
	def nTokens = pypUni._oCount
	def update(obs: Word) = {
//	  println("add "+obs+" before: "+pypUni._oCount(obs))
	  pypUni.update(obs)
//	  println("add "+obs+" after: "+pypUni._oCount(obs))
	}
	def remove(obs: Word) = {
//	  println("remove "+obs+" before: "+pypUni._oCount(obs))
	  pypUni.remove(obs)
//	  println("remove "+obs+" after: "+pypUni._oCount(obs))
	}
	def toSurface(underlying: Word, surface: Word) = data.R(underlying, surface)
	def symbolTable = data.symbolTable
	def DROPSYMBOL = data.DROPSYMBOL
	def _phoneSeq = data.data
	
	
	/**
	 * returns the probability for generating an utterance final word
	 */
	def _predBoundary(add: Int=0) = {
//	  println("utts: "+nUtterances+"\ntoks: "+nTokens)
	  (nUtterances+betaUB/2.0)/(nTokens+betaUB+add)
	}
	  
	
	/**
	 * initializes the CRP with the counts
	 */
	def init(gold:Boolean = false) = {
	  def inner(remData: Vector[Boundary],sPos: Int,cPos: Int): Unit = 
	    if (remData.size==0)
	      Unit
	    else 
	      remData.head match {
	      	case NoBoundary => inner(remData.tail,sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  update(Word(_phoneSeq.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))))
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
	      	  update(Word(_phoneSeq.slice(sPos-1, cPos)))
 	      	  inner(remData.tail,cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  update(Word(_phoneSeq.slice(sPos-1, cPos).:+(symbolTable(DROPSYMBOL))))
 	      	  inner(remData.tail,cPos+1,cPos+1)
 	      	  nUtterances+=1
	      	}
	      	case UBoundaryNodrop => {
	      	  update(Word(_phoneSeq.slice(sPos-1, cPos)))
 	      	  inner(remData.tail,cPos+1,cPos+1)
 	      	  nUtterances+=1
	      	}
	  }
	  if (gold)
	    data.boundaries=data.goldBoundaries
	  inner(data.boundaries.tail,1,1)
	}	
	
	/**
	 * returns a distribution over all possible ways to resample
	 * a boundary position
	 */
	def _calcHypotheses(context: Context): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  val isFinalWord = context.right==Word(data.DROPSYMBOL)
	  
	  val contProb1Word = 
	    if (isFinalWord)
	      _predBoundary(0)
	    else
	      (1-_predBoundary(0))  
	  
	  val contProb2Words = (1-_predBoundary(0))
	  val endProb2Words =
	    if (isFinalWord)
	      _predBoundary(1)
	    else
	      (1-_predBoundary(1))
	  
//	  println("pNoBound="+pypUni(context.w1Observed+context.w2Underlying)+"*"+contProb1Word+"*"+
//	    toSurface(context.w1Observed+context.w2Underlying,context.w1Observed+context.w2Observed))
//	  println(context.w1Observed.toString+context.w2Underlying.toString+"-->"+context.w1Observed.toString+context.w2Observed.toString)
	  val probNoBoundary =
	    pypUni(context.w1Observed+context.w2Underlying)*contProb1Word*
	    toSurface(context.w1Observed+context.w2Underlying,context.w1Observed+context.w2Observed)
	  val probBoundaryDrop =
	    pypUni(context.w1Observed+data.DROPSYMBOL)*contProb2Words*
	    pypUni(context.w2Underlying)*endProb2Words*
	    toSurface(context.w1Observed+data.DROPSYMBOL,context.w1Observed)*
	    toSurface(context.w2Underlying,context.w2Observed)
	  val probBoundaryNodrop =
	    pypUni(context.w1Observed)*contProb2Words*
	    pypUni(context.w2Underlying)*endProb2Words*
	    toSurface(context.w1Observed,context.w1Observed)*
	    toSurface(context.w2Underlying,context.w2Observed)
	  res.add(NoBoundary,probNoBoundary)
	  res.add(WBoundaryDrop,probBoundaryDrop)
	  res.add(WBoundaryNodrop,probBoundaryNodrop)
	  assert(res.partition>0)
	  return res
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: Context) = {
	  data.setBoundary(pos, b)
	  if (context.right==Word(data.UBOUNDARYSYMBOL))
	    nUtterances+=1
	  b match {
	    case NoBoundary => update(context.w1Observed+context.w2Underlying)
	    case WBoundaryDrop => {
	      update(context.w1Observed+data.DROPSYMBOL)
	      update(context.w2Underlying)
	    }
	    case WBoundaryNodrop => {
	      update(context.w1Observed)
	      update(context.w2Underlying)
	    }
	  }
	}
	
	def resample(pos: Int): Unit = {  
	  boundaries(pos) match {
	    case UBoundaryDrop | UBoundaryNodrop => Unit
	    case _ => {
	      val context = data.context(pos)
	      if (context.right==Word(data.UBOUNDARYSYMBOL))
	      	nUtterances-=1	      
	      boundaries(pos) match {
	        case WBoundaryDrop | WBoundaryNodrop => {
	          remove(context.w1Underlying)
	          remove(context.w2Underlying)
	        }
	        case _ => {
	          remove(context.w1Observed+context.w2Underlying)
	        }  
	      }
	      val result = _calcHypotheses(context)
	      updateBoundary(pos, result.sample,context)
	    }
	  }}
	
	def gibbsSweep(): Double = {
	  for (i: Int <- shuffle(0 until boundaries.size)) 
		  resample(i)
	  pypUni.logProb
	}
}