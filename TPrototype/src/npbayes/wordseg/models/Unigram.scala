package npbayes.wordseg.models
/**
 * Goldwater-style Unigram model
 */


import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle
import com.google.common.collect.ImmutableList.Builder

class Unigram(val corpusName: String,concentration: Double,discount: Double=0,val assumption: HEURISTIC = EXACT,val dropProb: Double =0.0) {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName,dropProb,"KRLK","KLRK")
	val pypUni = new CRP[WordType](concentration,discount,new Monkey(SymbolTableString.nSymbols,0.5),assumption)
	var nUtterances = 0

	def boundaries = data.boundaries
	def nTokens = pypUni._oCount

	def update: (WordType=>Double) = pypUni.update

	def remove: (WordType=>Double)= pypUni.remove

	def toSurface: ((WordType,WordType)=>Double) = data.R
	def DROPSYMBOL = data.DROPSYMBOL
	def _phoneSeq = data.data
	
	
	/**
	 * returns the probability for generating an utterance final word
	 */
	def _predBoundary(add: Int=0) = {
	  (nUtterances+betaUB/2.0)/(nTokens+betaUB+add)
	}
	  
	
	/**
	 * initializes the CRP with the counts
	 */
	def init(gold:Boolean = false) = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.size)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop => {
 	      	  update(new Builder[Int].addAll(_phoneSeq.subList(sPos-1, cPos)).add(data.DROPSEG).build)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
	      	  update(_phoneSeq.subList(sPos-1, cPos))
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  update(new Builder[Int].addAll(_phoneSeq.subList(sPos-1, cPos)).add(data.DROPSEG).build)
 	      	  nUtterances+=1
	      	  inner(cPos+1,cPos+1)
 	      	  
	      	}
	      	case UBoundaryNodrop => {
	      	  update(_phoneSeq.subList(sPos-1, cPos))
 	      	  nUtterances+=1
	      	  inner(cPos+1,cPos+1)
	      	}
	  }
	  if (gold)
	    data.boundaries=data.goldBoundaries.clone
	  inner(1,1)
	}	
	
	
	def _noBoundary(w1w2Under: WordType, w1w2Obs: WordType, isFinal: Boolean) = 
	  pypUni(w1w2Under)*
	  toSurface(w1w2Under,w1w2Obs)*
	  {if (isFinal) _predBoundary() else (1-_predBoundary())}
	
	    
	/**
	 * whether or not a drop occured is handled fully by what you pass
	 */
	def _boundary(w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, isFinal: Boolean) =
	  pypUni(w1Under)*
	  toSurface(w1Under,w1Obs)*
	  (1-_predBoundary())*
	  pypUni(w2Under,List(w1Under))*
	  toSurface(w2Under,w2Obs)*
	  {if (isFinal) _predBoundary(1) else (1-_predBoundary(1))}

	
	/**
	 * returns a distribution over all possible ways to resample
	 * a boundary position
	 */
	def _calcHypotheses(context: Context): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  val isFinalWord = context.right==data.UBOUNDARYWORD
	  res.add(NoBoundary,
	      _noBoundary(context.w1w2Underlying,context.w1w2Observed,isFinalWord))
	  res.add(WBoundaryDrop,
	      _boundary(context.w1WithDrop,context.w2Underlying,context.w1Observed,context.w2Observed,isFinalWord))
	  res.add(WBoundaryNodrop,
	      _boundary(context.w1Observed,context.w2Underlying,context.w1Observed,context.w2Observed,isFinalWord))
	  assert(res.partition>0)
	  return res
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: Context) = {
	  data.setBoundary(pos, b)
	  if (context.right==data.UBOUNDARYWORD)
	    nUtterances+=1
	  b match {
	    case NoBoundary => update(context.w1w2Underlying)
	    case WBoundaryDrop => {
	      update(context.w1WithDrop)
	      update(context.w2Underlying)
	    }
	    case WBoundaryNodrop => {
	      update(context.w1Observed)
	      update(context.w2Underlying)
	    }
	  }
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {  
	  boundaries(pos) match {
	    case UBoundaryDrop | UBoundaryNodrop => Unit
	    case _ => {
	      val context = data.context(pos)
	      if (context.right==data.UBOUNDARYWORD)
	      	nUtterances-=1	      
	      boundaries(pos) match {
	        case WBoundaryDrop | WBoundaryNodrop => {
	          remove(context.w1Underlying)
	          remove(context.w2Underlying)
	        }
	        case _ => {
	          remove(context.w1w2Underlying)
	        }  
	      }
	      val result = _calcHypotheses(context)
	      if (anneal==1.0)
	        updateBoundary(pos, result.sample,context)
	      else
	        updateBoundary(pos, result.sample(anneal),context)
	    }
	  }}
	
	def gibbsSweep(anneal: Double=1.0): Double = {
	  for (i: Int <- shuffle(0 until boundaries.size)) 
		  resample(i,anneal)
	  pypUni.logProb
	}
}