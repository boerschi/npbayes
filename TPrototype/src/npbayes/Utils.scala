package npbayes
/**
 * convenience functions
 */

import scala.collection.mutable.HashMap

/**
 * Jason Naradowsky's Argument parser
 * added a toString-method
 */
class ArgParser(argArray: Array[String]) {
	
	var args = argArray
	
	def contains(str: String): Boolean = args.contains(str)
	
	def getInt(arg: String): Int = getString(arg).toInt

	def getInt(arg: String, default: Int): Int = getString(arg, default.toString).toInt
	
	def getDouble(arg: String): Double = getString(arg).toDouble

	def getDouble(arg: String, default: Double): Double = getString(arg, default.toString).toDouble
	
	def getString(arg: String): String = getString(arg, null.asInstanceOf[String])	
	
	def getString(arg: String, default: String): String = {
		if (args.contains(arg)) {
			return args(args.indexOf(arg)+1)
		}
		else {
			return default
		}
	}	
	
	def getBoolean(arg: String, default: Boolean=false): Boolean = {
		if (args.contains(arg)) {
			return args(args.indexOf(arg)+1).toLowerCase == "true"
		}
		else {
			return default
		}			
	}
	
	override def toString = {
	  val res: StringBuffer = new StringBuffer
	  for (i <- 0 until argArray.length by 2)
	    res.append("# "+argArray(i)+" => "+argArray(i+1)+"\n")
	  res.toString
	}
	  
	
	def addOption(arg: String, value: String) = {
	  args = (Array(arg, value) ++ args)
	}
}

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