package npbayes.wordseg.models
/**
 * Goldwater-style Unigram model
 */


import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle


abstract class UContext

case class UnigramMedialContext(val w1O: WordType, val w1U: WordType, val w1D: WordType,
					  val w2O: WordType, val w2U: WordType,
					  val w1w2O: WordType, val w1w2U: WordType,
					  val isFinal: Boolean) extends UContext

case class UnigramFinalContext(val wO: WordType, val wU: WordType, val wD: WordType) extends UContext					  
					  

class Unigram(val corpusName: String,concentration: Double,discount: Double=0,val assumption: HEURISTIC = EXACT,val dropProb: Double =0.0) {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName,dropProb,"KRLK","KLRK")
	val pypUni = new CRP[WordType](concentration,discount,new Monkey(SymbolTable.nSymbols,0.5),assumption)
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
 	      	  update(suffix(_phoneSeq.subList(sPos-1, cPos),data.DROPSEG))
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case WBoundaryNodrop => {
	      	  update(_phoneSeq.subList(sPos-1, cPos))
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop => {
	      	  update(suffix(_phoneSeq.subList(sPos-1, cPos),data.DROPSEG))
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
	
	
	/**
	 * context utterance medial ==> w1, w2, w1w2, isFinal-information
	 */
	def medialContext(pos: Int) = {
	  assert(pos>0 && boundaries(pos)!=UBoundaryDrop && boundaries(pos)!=UBoundaryNodrop && pos<(boundaries.length-1))
	  val (w1O,w1U,w1D) = data.getLeftWord(pos)
	  val (w2O,w2U,isFinal) = data.getRightWord(pos)
	  val w1w2O = concat(w1O,w2O)
	  val w1w2U = concat(w1O,w2U)
	  new UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,isFinal)
	}
	
	/**
	 * context utterance final ==> w1
	 */
	def finalContext(pos: Int) = {
	  assert(pos>0 && (boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop))
	  val (w1O,w1U,w1D) = data.getLeftWord(pos)
	  new UnigramFinalContext(w1O,w1U,w1D)
	}
	

	/**
	 * 
	 */
	def boundaryContext(pos: Int): UContext = boundaries(pos) match {
	  case UBoundaryDrop | UBoundaryNodrop => finalContext(pos)
	  case _ => medialContext(pos)
	}
	
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


	def _noBoundary(w1w2Under: WordType, w1w2Obs: WordType, isFinal: Boolean) = 
	  pypUni(w1w2Under)*
	  toSurface(w1w2Under,w1w2Obs)*
	  {if (isFinal) _predBoundary() else (1-_predBoundary())}
	
	def _ubProb(w1U: WordType, w1O: WordType) =
	  pypUni(w1U)*
	  toSurface(w1U,w1O)
	
	/**
	 * returns a distribution over all possible ways to resample
	 * a boundary position
	 */
	def _calcMedialHypotheses(w1O: WordType,w1D: WordType,
					  w2O: WordType,w2U: WordType,
					  w1w2O: WordType, w1w2U: WordType, isFinal: Boolean): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(NoBoundary,
	      _noBoundary(w1w2U,w1w2O,isFinal))
	  res.add(WBoundaryDrop,
	      _boundary(w1D,w2U,w1O,w2O,isFinal))
	  res.add(WBoundaryNodrop,
	      _boundary(w1O,w2U,w1O,w2O,isFinal))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(w1O: WordType, w1D: WordType): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(UBoundaryDrop,
	      _ubProb(w1D, w1O))
	  res.add(UBoundaryNodrop,
	      _ubProb(w1O,w1O))
	  assert(res.partition>0)
	  res
	}
	
	def _calcHypotheses(context: UContext): Categorical[Boundary] = {
		context match {
		  case UnigramFinalContext(w1O,w1U,w1D) => _calcFinalHypotheses(w1O,w1D)
		  case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,isFinal) => 
		    _calcMedialHypotheses(w1O,w1D,w2O,w2U,w1w2O,w1w2U,isFinal)
		}
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: UContext) = {
	  data.setBoundary(pos, b)
	  context match {
	    case UnigramMedialContext(w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,isFinal) =>
	    	b match {
	    	  case NoBoundary => update(w1w2U)
	    	  case WBoundaryDrop => 
	    	    update(w1D)
	    	    update(w2U)
	    	  case WBoundaryNodrop =>
	    	    update(w1O)
	    	    update(w2U)
	    	}
	    	if (isFinal) nUtterances+=1
	    case UnigramFinalContext(w1O,w1U,w1D) =>
	      b match {
	        case UBoundaryDrop =>
	          update(w1D)
	        case UBoundaryNodrop =>
	          update(w1O)
	      }
	  }
	}
	
	def removeAssociatedObservations(context: UContext, hasBoundary: Boolean) =
	  context match {
	  case UnigramMedialContext(_,w1U,_,_,w2U,_,w1w2U,isFinal) =>
	    if (isFinal) nUtterances-=1
	    if (hasBoundary) {
	      remove(w1U)
	      remove(w2U)
	    } else
	      remove(w1w2U)
	  case UnigramFinalContext(_,w1U,_) =>
	    remove(w1U)
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {  
		val context = boundaryContext(pos)
		removeAssociatedObservations(context,boundaries(pos)==WBoundaryDrop || boundaries(pos)==WBoundaryNodrop)
		val result = _calcHypotheses(context)
		if (anneal==1.0)
		  updateBoundary(pos, result.sample,context)
		else
		  updateBoundary(pos, result.sample(anneal),context)
	}
	
	def gibbsSweep(anneal: Double=1.0): Double = {
	  for (i: Int <- shuffle(1 until boundaries.length)) 
		  resample(i,anneal)
	  pypUni.logProb
	}
}