package npbayes.wordseg.data

import npbayes.distributions.Observation
import scala.collection.mutable.WeakHashMap
import scala.collection.immutable.VectorBuilder

object `package` {
  val symbolTable = npbayes.wordseg.data.SymbolTableString
}

class WordType(val phones: Vector[Short]) extends Observation {
  override def toString = phones.map((x: Short) => symbolTable(x)).mkString(" ") 
  def + (that: WordType) =
    WordType(phones.++(that.phones))

  def size = phones.size
  
  def equals(obj: WordType) = 
      phones==obj.phones
  
  override def hashCode = phones.hashCode
}

object WordType {
  def str2Vec(label: String): Vector[Short] = {
    val res = new VectorBuilder[Short]
	label.split(" ").map(c=>res+=(symbolTable(c)))
	res.result
  }
  
  implicit def getWordOrConstruct(label: Vector[Short]): WordType = 
    _Vcache.getOrElseUpdate(label, new WordType(label))

   
  val _Vcache: WeakHashMap[Vector[Short], WordType] = new WeakHashMap
  
  def apply(label: Vector[Short]): WordType = _Vcache.getOrElseUpdate(label, getWordOrConstruct(label))

  def apply(label: String): WordType = WordType(str2Vec(label))
}
