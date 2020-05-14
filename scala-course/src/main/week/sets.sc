abstract class IntSet{
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other: IntSet) : IntSet
}

object EmptySet extends IntSet{
  override def incl(x: Int) = new NonEmpty(x, EmptySet, EmptySet)

  override def contains(x: Int) = false

  override def union(other: IntSet) = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
  override def incl(x: Int) : NonEmpty = {
    if(x< elem) new NonEmpty(elem, left incl x, right)
    else if (x> elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  override def contains(x: Int) = {
    if(x< elem) left.contains(x)
    else if(x> elem) right.contains(x)
    else true
  }

  override def union(other: IntSet) = (left.union(right)).union(other).incl(elem)

}
