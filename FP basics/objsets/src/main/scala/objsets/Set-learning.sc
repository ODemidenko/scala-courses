abstract class IntSet{
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet
}

class Empty extends IntSet {
  def incl(x:Int):IntSet ={
    new NonEmpty(x,new Empty,new Empty)
  }

  def contains(x:Int):Boolean=false

  def union(other:IntSet):IntSet=other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x==elem) true
    else if (x<elem) left contains x
    else right contains x

  def incl(x:Int):IntSet=
    if (x==elem) this
    else if (x<elem) new NonEmpty(elem,left.incl(x),right)
    else new NonEmpty(elem,left,right.incl(x))

  def union(other: IntSet): IntSet =
    if (other.elem>elem) new NonEmpty(elem,left,right.union(other))

  override def toString: String = "{"+left+elem+right+"}"
}
