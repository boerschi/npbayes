package npbayes.wordseg

import scala.collection.mutable.HashMap

package object wordseg {
	def seatAt(seating: HashMap[Int,Int],tableSize: Int) = {
	  if (tableSize==0)
	    seating(1)=seating.getOrElse(1,0)+1
	  else {
		  val oneLessTS = seating(tableSize)-1
		  if (oneLessTS==0)
			seating.remove(tableSize)
		  else
		    seating(tableSize)=oneLessTS
		  seating(tableSize+1)=seating.getOrElse(tableSize+1,0)+1
	  }
	}
	
	def removeFrom(seating: HashMap[Int,Int],tableSize: Int): Boolean = {
	  val oneLessTS = seating(tableSize)-1
	  if (oneLessTS==0) {
	    seating.remove(tableSize)
	  }
	  else {
	    seating(tableSize)=oneLessTS
	  }
	  if (tableSize>1) {
	    seating(tableSize-1)=seating.getOrElse(tableSize-1,0)+1
	    false
	  } else
		true
	}
}