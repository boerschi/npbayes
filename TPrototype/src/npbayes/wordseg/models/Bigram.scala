package npbayes.wordseg.models

import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle
import scala.collection.mutable.HashMap
import npbayes.utils.Histogram
abstract class BContext

case class BigramMedialContext(val leftU: WordType, val w1O: WordType, val w1U: WordType, val w1D: WordType,
					  val w2O: WordType, val w2U: WordType,
					  val w1w2O: WordType, val w1w2U: WordType,
					  val rightO: WordType, val rightU: WordType) extends BContext {
  override def toString = {
    wToS(leftU)+","+wToS(w1O)+","+wToS(w2O)+","+wToS(rightU)
  }
}

case class BigramFinalContext(val leftU: WordType, val wO: WordType, val wU: WordType, val wD: WordType) extends BContext					  



class Bigram(val corpusName: String,concentrationUni: Double,discountUni: Double=0,concentrationBi: Double, discountBi: Double=0,val assumption: HEURISTIC = EXACT,val dropProb: Double =0.0) {
	require(0<=discountUni && discountUni<1)
	require(if (discountUni==0) concentrationUni>0 else concentrationUni>=0)
	val data = new VarData(corpusName,dropProb,"KRLK","KLRK")
	val pypUni = 
	  new CRP[WordType](concentrationUni,discountUni,new MonkeyBigram(SymbolTable.nSymbols-2,0.5,data.UBOUNDARYWORD,0.5))//,EXACT)
//	val biEmpty = new CRP[WordType](concentrationBi,discountBi,pypUni,assumption)
	val pypBis: HashMap[WordType,CRP[WordType]] = new HashMap
//	var nUtterances = 0

	val debugCounts: HashMap[WordType,HashMap[WordType,Int]] = new HashMap
	
	var lost: Histogram =new Histogram
	var changed = 0
	var boundToNo = 0
	var noToBound = 0
	var sampled = 0
	
	def boundaries = data.boundaries
	def nTokens = pypUni._oCount

	def update(precedingW: WordType, word: WordType): Double = {
//	  debugCounts.getOrElseUpdate(precedingW, new HashMap)(word)=debugCounts(precedingW).getOrElse(word,0)+1
	  pypBis.getOrElseUpdate(precedingW, new CRP[WordType](concentrationBi,discountBi,pypUni)).update(word)
	}

	def removeWrap(precedingW: WordType, word: WordType) = {
/*	  debugCounts(precedingW)(word)-=1
	  if (debugCounts(precedingW)(word)==0) {
	    debugCounts(precedingW).remove(word)
	    if (debugCounts(precedingW).isEmpty)
	      debugCounts.remove(precedingW)
	  }*/
	  pypBis(precedingW).remove(word)
	  if (pypBis(precedingW).isEmpty) {
	    pypBis.remove(precedingW)
//	    println("drop "+wToS(precedingW)+"-Restaurant")
	  }
	    
	}
	
	def sanity: Boolean = {
	  /*{for (r <- pypBis.values.toList)
	    yield r.hmTableCounts.values.sum}.sum == pypUni._oCount &&*/
	  /*{for (w1 <- debugCounts.keySet.toList; w2 <- debugCounts(w1).keySet.toList)
		  	yield debugCounts(w1)(w2)==pypBis(w1)._oCount(w2)}.foldLeft(true)(_&&_)
	      
	  */true  }
	//}

	def toSurface(u: WordType, o:WordType): Double = data.R(u,o)
	def DROPSYMBOL = data.DROPSYMBOL
	def _phoneSeq = data.data
	
	
	
	def medialContext(pos: Int): BigramMedialContext = {
	  assert(pos>0 && boundaries(pos)!=UBoundaryDrop && boundaries(pos)!=UBoundaryNodrop && pos<(boundaries.length-1))
	  val leftWordStart = data.boundaryToLeft(pos-1)
	  val rightWordEnd = data.boundaryToRight(pos+1) 
	  val (w1O,w1U,w1D) = data.getWordWithVar(leftWordStart, pos)
	  val (w2O,w2U) = data.getWord(pos,rightWordEnd)
	  val w1w2O = concat(w1O,w2O)
	  val w1w2U = concat(w1O,w2U)
	  val lU = boundaries(leftWordStart) match {
	    case UBoundaryDrop | UBoundaryNodrop => data.UBOUNDARYWORD
	    case _ => data.getWord(data.boundaryToLeft(leftWordStart-1), leftWordStart)._2
	  }
	  val (rO,rU: WordType) = boundaries(rightWordEnd) match {
	    case UBoundaryDrop | UBoundaryNodrop => (data.UBOUNDARYWORD,data.UBOUNDARYWORD)
	    case _ => data.getWord(rightWordEnd, data.boundaryToRight(rightWordEnd+1))
	  }
	  new BigramMedialContext(lU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rO,rU)
	}
	
	def finalContext(pos: Int): BigramFinalContext = {
	  assert(pos>0 && (boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop))
	  val leftWordStart = data.boundaryToLeft(pos-1)
	  val (wO,wU,wD) = data.getWordWithVar(leftWordStart, pos)
	  val lU = boundaries(leftWordStart) match {
	    case UBoundaryDrop | UBoundaryNodrop => data.UBOUNDARYWORD
	    case _ => data.getWord(data.boundaryToLeft(leftWordStart-1), leftWordStart)._2
	  }
	  new BigramFinalContext(lU,wO,wU,wD)
	}
	
	def boundaryContext(pos: Int): BContext = boundaries(pos) match {
	  case UBoundaryDrop | UBoundaryNodrop => finalContext(pos)
	  case _ => medialContext(pos)
	}	
	/**
	 * initializes the CRP with the counts
	 */
	def init(gold:Boolean = false) = {
	  def inner(sPos: Int,cPos: Int): Unit = 
	    if (cPos>=boundaries.length)
	      Unit
	    else 
	      boundaries(cPos) match {
	      	case NoBoundary => inner(sPos,cPos+1)
	      	case WBoundaryDrop | WBoundaryNodrop => {
	      	  val context = medialContext(cPos)
//	      	  val context = data.context(cPos)
 	      	  update(context.leftU,context.w1U)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop | UBoundaryNodrop => {
	      	  val context = finalContext(cPos)
	      	  update(context.leftU,context.wU)
	      	  update(context.wU,data.UBOUNDARYWORD)
	      	  inner(cPos+1,cPos+1)
	      	}
	  }
	  if (gold)
	    data.boundaries=data.goldBoundaries.clone
	  inner(1,1)
	}	
	
	
	def predictive(word: WordType, w2: WordType): Double = pypBis.getOrElse(word,pypUni).predProb(w2)
	
	def _noBoundary(left: WordType,w1w2Under: WordType, w1w2Obs: WordType, right: WordType) = {
	  var res = predictive(left,w1w2Under)*toSurface(w1w2Under,w1w2Obs)
	  update(left,w1w2Under)
	  res = res * predictive(w1w2Under,right)
	  removeWrap(left,w1w2Under)
	  res
	}
	
	    
	/**
	 * whether or not a drop occured is handled fully by what you pass
	 */
	def _boundary(left: WordType, w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, right: WordType) = {
	  var res = predictive(left,w1Under)*toSurface(w1Under,w1Obs)
	  update(left,w1Under)
	  res = res * predictive(w1Under,w2Under) * toSurface(w2Under,w2Obs)
	  update(w1Under,w2Under)
	  res = res * predictive(w2Under,right)
	  removeWrap(left, w1Under)
	  removeWrap(w1Under,w2Under)
	  res
	}
	
	  
	def _ubProb(left: WordType, wU: WordType, wO: WordType) =
	  predictive(left,wU)*
	  toSurface(wU,wO)*
	  predictive(wU,data.UBOUNDARYWORD)
	
	/**
	 * returns a distribution over all possible ways to resample
	 * an utterance medial boundary position
	 */
	def _calcMedialHypotheses(context: BigramMedialContext): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(NoBoundary,
	      _noBoundary(context.leftU,context.w1w2U,context.w1w2O,context.rightU))
//	  res.add(WBoundaryDrop,
//	      _boundary(context.leftU,context.w1D,context.w2U,context.w1O,context.w2O,context.rightU))
	  res.add(WBoundaryNodrop,
	      _boundary(context.leftU,context.w1O,context.w2U,context.w1O,context.w2O,context.rightU))
	  assert(res.partition>0)
	  res
	}
	
	def _calcFinalHypotheses(context: BigramFinalContext): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(UBoundaryDrop,
	      _ubProb(context.leftU,context.wD,context.wO))
	  res.add(UBoundaryNodrop,
	      _ubProb(context.leftU,context.wO,context.wO))
	  assert(res.partition>0)
	  res
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: BContext) = {
  	  val hasChanged = boundaries(pos)!=b
	  data.setBoundary(pos, b)
	  context match {
	    case BigramMedialContext(leftU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rightO,rightU) =>
	      b match {
	        case WBoundaryDrop =>
	          update(leftU,w1D)
	          update(w1D,w2U)
	          update(w2U,rightU)
	          if (hasChanged) {
	    	      changed+=1
	    	      noToBound+=1
	    	  }
	        case WBoundaryNodrop =>
	          update(leftU,w1O)
	          update(w1O,w2U)
	          update(w2U,rightU)
	          if (hasChanged) {
	    	      changed+=1
	    	      noToBound+=1
	    	  }
	        case NoBoundary =>
	          update(leftU,w1w2U)
	          update(w1w2U,rightU)
	          if (hasChanged) {
	    	      changed+=1
	    	      boundToNo+=1
//	    	      lost.incr((wToS(w1O),wToS(w2O)))
	    	    }
	      }
	    case BigramFinalContext(leftU,wO,wU,wD) =>
	      b match {
	        case UBoundaryDrop =>
	          update(leftU,wD)
	          update(wD,data.UBOUNDARYWORD)
	        case UBoundaryNodrop =>
	          update(leftU,wO)
	          update(wO,data.UBOUNDARYWORD)
	      }
	  }
	}
	
	def removeAssociatedObservations(context: BContext, boundary: Boundary) = context match {
	  case BigramMedialContext(lU,_,w1U,_,_,w2U,_,w1w2U,_,rU) =>
	    boundary match {
	      case WBoundaryDrop | WBoundaryNodrop =>
	        removeWrap(lU, w1U)
	        removeWrap(w1U,w2U)
	        removeWrap(w2U,rU)
	      case NoBoundary =>
	        removeWrap(lU,w1w2U)
	        removeWrap(w1w2U,rU)
	    }
	  case BigramFinalContext(lU,_,wU,_) =>
	    removeWrap(lU,wU)
	    removeWrap(wU,data.UBOUNDARYWORD)
	}
	
	def _calcHypotheses(context: BContext): Categorical[Boundary] = context match {
	  case c: BigramMedialContext => _calcMedialHypotheses(c)
	  case c: BigramFinalContext => _calcFinalHypotheses(c)
	}
	
	def resample(pos: Int, anneal: Double=1.0): Unit = {
//	  if (boundaries(pos)==UBoundaryDrop || boundaries(pos)==UBoundaryNodrop)
//	    return
	  val context = boundaryContext(pos)
	  removeAssociatedObservations(context, boundaries(pos))
	  val result = _calcHypotheses(context)
	  var newBound: Boundary = null
	  if (anneal==1.0)
		newBound =  result.sample
	  else
		newBound = result.sample(anneal)
	  updateBoundary(pos, newBound,context)
	}
	
	def logProb: Double = 
	  (for (pypW <- pypBis.values.toList)
	    yield {
		assert(pypW.sanityCheck)
	    pypW.logProbSeating}).reduce(_+_)+{assert(pypUni.sanityCheck); 
	    pypUni.logProb}
	
	def gibbsSweep(anneal: Double=1.0): Double = {
	  lost = new Histogram
	  changed = 0
	  noToBound = 0
	  boundToNo = 0
	  sampled = 0
//	  for (i: Int <- shuffle(1 until boundaries.length)) {
	  for (i: Int <- 1 until boundaries.length) {
		  resample(i,anneal)
		  sampled+=1
	  }
	  logProb
	}
	
	def resampleWords(pos: Int, anneal: Double) = {
	  boundaries(pos) match {
	    case NoBoundary => Unit
	    case UBoundaryDrop | UBoundaryNodrop => Unit
	    case _ =>
	    val context = boundaryContext(pos)
	    context match {
	    	case BigramMedialContext(leftU,w1O,w1U,w1D,w2O,w2U,w1w2O,w1w2U,rightO,rightU) =>
	    		removeWrap(leftU,w1U)
	    		removeWrap(w1U,w2U)
	    		removeWrap(w2U,rightU)
	    		update(leftU,w1U)
	    		update(w1U,w2U)
	    		update(w2U,rightU)
	    	case BigramFinalContext(leftU,wO,wU,wD) =>
	    	  removeWrap(leftU,wU)
	    	  removeWrap(wU,data.UBOUNDARYWORD)
	    	  update(leftU,wU)
	    	  update(wU,data.UBOUNDARYWORD)
	  }	      
	  }

	}
	
	def gibbsSweepWords(anneal: Double=1.0): Double = {
	  lost = new Histogram
	  changed = 0
	  noToBound = 0
	  boundToNo = 0
	  sampled = 0
//	  for (i: Int <- shuffle(1 until boundaries.length)) {
	  for (i: Int <- 1 until boundaries.length) {
		  resampleWords(i,anneal)
		  sampled+=1
	  }
	  logProb
	}
}