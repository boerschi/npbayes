package npbayes.utils
/**
 * convenience functions
 */

import scala.collection.mutable.HashMap

class Histogram {
  val counts: HashMap[Any,Int] = new HashMap
  var total: Double = 0
  
  def incr(obs: Any) = {
    counts(obs)=counts.getOrElse(obs, 0)+1
    total += 1
  }
  
  override def toString = {
    val res = new StringBuilder
    for ((obs,count) <- counts.toList.sortBy(x=>(-x._2)))
      res.append(obs+": "+count/total+"\n")
    res.toString
  }
  
}

object Utils {
  
    /**
     * decrements the integer-value of key by step (default=1) and
     * drops key from the map if the value drops to or below 0
     */
	def decr[T](hm: HashMap[T,Int], key: T, step: Int = 1): Unit = {
	  val old = hm(key)
	  if (old-step<=0)
	    hm.remove(key)
	  else
	    hm(key) = old-step
	}
	
	/**
	 * increments the integer-value of key by step (default=1) and
	 * ensures that a new key-value pair is added if key is not
	 * yet present
	 */
	def incr[T](hm: HashMap[T,Int], key: T, step: Int = 1): Unit = 
	  hm(key) = hm.getOrElse(key, 0)+step
	
}