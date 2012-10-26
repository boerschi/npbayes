package npbayes
/**
 * convenience functions
 */

import scala.collection.mutable.HashMap

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