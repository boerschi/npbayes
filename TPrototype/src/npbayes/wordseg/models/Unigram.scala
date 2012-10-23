package npbayes.wordseg.models
/**
 * Goldwater-style Unigram model
 */

import npbayes.distributions._
import npbayes.wordseg.data._

class Unigram(val corpusName: String,concentration: Double,discount: Double=0) {
	require(0<=discount && discount<1)
	require(if (discount==0) concentration>0 else concentration>=0)
	val betaUB = 2.0
	val data = new VarData(corpusName)
	val pypUni = new CRP[Word](concentration,discount,new Monkey(data.symbolTable.nSymbols,0.5))
	var nUtterances = 0

	// local aliases
	def boundaries = data.boundaries
	def nTokens = pypUni._oCount
	def update(obs: Word) = pypUni.update(obs)
	def remove(obs: Word) = pypUni.remove(obs)
	def symbolTable = data.symbolTable
	def DROPSYMBOL = data.DROPSYMBOL
	def _phoneSeq = data.data
	
	/**
	 * returns the probability for generating an utterance final word
	 */
	def _predBoundary =
	  (nUtterances+betaUB/2.0)/(nTokens)
	
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
	
	def resample(pos: Int): Unit = 
	  boundaries(pos) match {
	    case UBoundaryDrop | UBoundaryNodrop => Unit
	    case _ => {
	      val context = data.context(pos)
	      boundaries(pos) match {
	        case WBoundaryDrop | WBoundaryNodrop => {
	          remove(context.w1)
	          remove(context.w2)
	        }
	        case _ => {
	          remove(context.w1+context.w2)
	        }  
	      }
	      
	    }
	  }

}