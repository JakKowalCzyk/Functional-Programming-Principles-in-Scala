import forcomp.Anagrams.Occurrences

Map


def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val yMap = y.map(occ => occ._1 -> occ._2).toMap
  x.foldLeft(List[(Char, Int)]()){case (acc, (xChar, xInt)) =>
    if(yMap.contains(xChar)) {
      val newOcc = xInt - yMap(xChar)
      if(newOcc > 0)  (xChar, newOcc) :: acc
      else acc
    }
    else (xChar, xInt)  :: acc
  }.sortBy(_._1)
}



val x = List(('a', 1), ('d', 4), ('l', 1), ('r', 3))
val y = List(('r', 1),('a', 1), ('d', 2))

subtract(x, y)