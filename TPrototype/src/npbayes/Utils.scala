package npbayes

import scala.collection.mutable.HashMap

object Utils {
	def decr[T](hm: HashMap[T,Int], key: T) = {
	  val old = hm(key)
	  if (old-1==0)
	    hm.remove(key)
	  else
	    hm(key) = old-1
	}
	
	def incr[T](hm: HashMap[T,Int], key: T) = {
	  val old = hm.getOrElse(key, 0)
	  hm(key) = old+1
	}
}