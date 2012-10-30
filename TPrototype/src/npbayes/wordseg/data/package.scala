package npbayes.wordseg.data

import com.google.common.collect.ImmutableList


object `package` {
  type WordType = ImmutableList[Int]
  def wToS (w: WordType): String = {
    val res = new StringBuilder
    for (i: Int <- (0 to w.size-1)){ 
      res.append(SymbolTableString(w.get(i)))
    }
    res.result
  }


} 