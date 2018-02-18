sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
    Exercise 3.1
    List (1,2,3,4,5) matches x, y, 3, 4, 5 first => 1 + 2
  */

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }
  /*
  Exercise 3.2 Answer
  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }
  */

  // Exercise 3.3
  def setHead[A](l: List[A], h2: A): List[A] =
    l match {
      case Nil => Cons(h2, Nil)
      case Cons(h, t) => Cons(h2, t)
    }
  /*
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }
  */
  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }
  /*
  def drop[A](l: List[A], n: Int): List[A] = 
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case Cons(_,t) => drop(t, n-1) 
  }
  */

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
    }
  }
  /*
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
  l match {
    case Cons(h,t) if f(h) => dropWhile(t, f) 
    case _ => l
  }
  */

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }
  /*
  def init[A](l: List[A]): List[A] = 
  l match { 
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
  */


  // New dropwhile
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => as }

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    println(drop(list, 3))
    println(dropWhile(list, (x: Int) => x < 3))
    println(init(list))
  }
}
