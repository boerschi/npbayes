package npbayes.wordseg.data

import npbayes.distributions.Observation
import scala.collection.mutable.WeakHashMap

class Word(val label: String, val phones: Vector[Short]) extends Observation {
  def + (that: Word) =
    Word(this.phones.++(that.phones))
  override def toString = label
  def size = phones.size
  
  def equals(obj: Word) = 
      phones==obj.phones
  
  override def hashCode = phones.hashCode
}

object Word {
  val symbolTable = npbayes.wordseg.data.SymbolTableString
  implicit def getWordOrConstruct(label: String) =  {
    var res = _Scache.getOrElse(label,null)
    if (res==null) {
      res = new Word(label,Vector.empty.++(label.split(" ").map((x: String)=>symbolTable(x))))
      _Scache(label)=res
      _Vcache(res.phones)=res
    }
    res
  }
  
  implicit def getWordOrConstruct(label: Vector[Short]): Word = {
    var res = _Vcache.getOrElse(label,null)
    if (res==null) {
    	res = new Word(label.map((x: Short) => symbolTable(x)).mkString(" "), label)
    	_Vcache(label) = res
    	_Scache(res.label) = res
    }
    res
  }
  
  
  val _Scache: WeakHashMap[String,Word] = new WeakHashMap
  val _Vcache: WeakHashMap[Vector[Short],Word] = new WeakHashMap
  def apply(label: Vector[Short]): Word = {
    val res = _Vcache.getOrElseUpdate(label, getWordOrConstruct(label))
    _Scache(res.label) = res
    res
  }
  def apply(label: String): Word = {
    val res =_Scache.getOrElseUpdate(label, getWordOrConstruct(label))
    _Vcache(res.phones)=res
    res
  }
}
