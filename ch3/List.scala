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
  // def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
  //   case Cons(h,t) if f(h) => dropWhile(t)(f)
  //   case _ => as }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)((x, y) => x * y)

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }

  /*
  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,acc) => acc + 1)
  */

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  /*
    @annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  */

  // Exercise 3.11
  def sumFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)((x, y) => x + y)
  def productFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((x, y) => x * y)
  def lengthFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((x, y) => x + 1)

  /*
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)
  */


  // Exercise 3.12
  //def reverseFoldLeft[A](l: List[A]): List[A] =
  //  foldLeft(l, Nil)((x, y) => Cons(y, x))
  def reverseFoldLeft[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((x, y) => Cons(y, x))
  /*
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  */

  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    val dList = List(1.0, 2.0, 3.0, 4.0)
    println(drop(list, 3))
    println(dropWhile(list, (x: Int) => x < 3))
    println(init(list))
    println(length(list))
    println(sumFoldLeft(list))
    println(productFoldLeft(dList))
    println(lengthFoldLeft(list))
    println(reverseFoldLeft(list))
  }
}
