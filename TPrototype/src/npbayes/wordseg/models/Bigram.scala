package npbayes.wordseg.models

import npbayes.distributions._
import npbayes.wordseg.data._
import npbayes.wordseg.lexgens._
import scala.util.Random.shuffle
import scala.collection.mutable.HashMap

abstract class BContext

case class BigramMedialContext(val leftU: WordType, val w1O: WordType, val w1U: WordType, val w1D: WordType,
					  val w2O: WordType, val w2U: WordType,
					  val w1w2O: WordType, val w1w2U: WordType,
					  val rightU: WordType) extends BContext

case class BigramFinalContext(val wO: WordType, val wU: WordType, val wD: WordType) extends BContext					  



class Bigram(val corpusName: String,concentrationUni: Double,discountUni: Double=0,concentrationBi: Double, discountBi: Double=0,val assumption: HEURISTIC = EXACT,val dropProb: Double =0.0) {
	require(0<=discountUni && discountUni<1)
	require(if (discountUni==0) concentrationUni>0 else concentrationUni>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName,dropProb,"KRLK","KLRK")
	val pypUni = new CRP[WordType](concentrationUni,discountUni,new Monkey(SymbolTable.nSymbols,0.5,data.UBOUNDARYWORD,0.5),assumption)
	val biEmpty = new CRP[WordType](concentrationBi,discountBi,pypUni,assumption)
	val pypBis: HashMap[WordType,CRP[WordType]] = new HashMap
	var nUtterances = 0

	def boundaries = data.boundaries
	def nTokens = pypUni._oCount

	def update(precedingW: WordType): (WordType=>Double) = 
	  pypBis.getOrElseUpdate(precedingW, new CRP[WordType](concentrationBi,discountBi,pypUni,assumption)).update

	def removeWrap(precedingW: WordType, word: WordType) = {
	  def remove(precedingW: WordType): (WordType=>Double)= 
	    pypBis(precedingW).remove
	  remove(precedingW)(word)
	  if (pypBis(precedingW).isEmpty)
	    pypBis.remove(precedingW)
	}

	def toSurface: ((WordType,WordType)=>Double) = data.R
	def DROPSYMBOL = data.DROPSYMBOL
	def _phoneSeq = data.data
	
	  
	
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
	      	case WBoundaryDrop | WBoundaryNodrop => {
	      	  val context = data.context(cPos)
 	      	  update(context.left)(context.w1Underlying)
 	      	  inner(cPos+1,cPos+1)
	      	}
	      	case UBoundaryDrop | UBoundaryNodrop => {
	      	  val context = data.contextLeft(cPos)
	      	  update(context.left)(context.w1Underlying)
	      	  update(context.w1Underlying)(data.UBOUNDARYWORD)
 	      	  nUtterances+=1
	      	  inner(cPos+1,cPos+1)
	      	}
	  }
	  if (gold)
	    data.boundaries=data.goldBoundaries.clone
	  inner(1,1)
	}	
	
	
	def pypW(word: WordType): CRP[WordType] = pypBis.getOrElse(word,biEmpty)
	
	def _noBoundary(left: WordType,w1w2Under: WordType, w1w2Obs: WordType, right: WordType) = 
	  pypW(left)(w1w2Under)*
	  toSurface(w1w2Under,w1w2Obs)*
	  pypW(w1w2Under)(right)
	
	    
	/**
	 * whether or not a drop occured is handled fully by what you pass
	 */
	def _boundary(left: WordType, w1Under: WordType,w2Under: WordType,w1Obs: WordType,w2Obs: WordType, right: WordType) =
	  pypW(left)(w1Under)*
	  toSurface(w1Under,w1Obs)*
	  pypW(w1Under)(w2Under)*
	  toSurface(w2Under,w2Obs)*
	  pypW(w2Under)(right)

	
	/**
	 * returns a distribution over all possible ways to resample
	 * a boundary position
	 */
	def _calcHypotheses(context: Context): Categorical[Boundary] = {
	  val res: Categorical[Boundary] = new Categorical
	  res.add(NoBoundary,
	      _noBoundary(context.left,context.w1w2Underlying,context.w1w2Observed,context.right))
	  res.add(WBoundaryDrop,
	      _boundary(context.left,context.w1WithDrop,context.w2Underlying,context.w1Observed,context.w2Observed,context.right))
	  res.add(WBoundaryNodrop,
	      _boundary(context.left,context.w1Observed,context.w2Underlying,context.w1Observed,context.w2Observed,context.right))
	  assert(res.partition>0)
	  return res
	}
	
	def updateBoundary(pos: Int, b: Boundary, context: Context) = {
	  data.setBoundary(pos, b)
	  b match {
	    case NoBoundary => {
	      update(context.left)(context.w1w2Underlying)
	      update(context.w1w2Underlying)(context.right)
	    }
	    case WBoundaryDrop => {
	      update(context.left)(context.w1WithDrop)
	      update(context.w1WithDrop)(context.w2Underlying)
	    }
	    case WBoundaryNodrop => {
	      update(context.left)(context.w1Observed)
	      update(context.w1Observed)(context.w2Underlying)
	      update(context.w2Underlying)(context.right)
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
	          removeWrap(context.left,context.w1Underlying)
	          removeWrap(context.w1Underlying,context.w2Underlying)
	          removeWrap(context.w2Underlying,context.right)
	        }
	        case _ => {
	          removeWrap(context.left,context.w1w2Underlying)
	          removeWrap(context.w1w2Underlying,context.right)
	        }  
	      }
	      val result = _calcHypotheses(context)
	      if (anneal==1.0)
	        updateBoundary(pos, result.sample,context)
	      else
	        updateBoundary(pos, result.sample(anneal),context)
	    }
	  }}
	
	def logProb: Double = 
	  (for (pypW <- pypBis.values.toList)
	    yield pypW.logProbSeating).reduce(_+_)+pypUni.logProb
	
	def gibbsSweep(anneal: Double=1.0): Double = {
	  for (i: Int <- shuffle(0 until boundaries.size)) 
		  resample(i,anneal)
	  logProb
	}
}