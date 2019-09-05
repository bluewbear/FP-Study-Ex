false && { println("!!"); true }
true || { println("!!"); false }

def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

val a = 30
if2(a < 22, () => println("a"), () => println("b"))

def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

val b = 2
//if3(b < 22, sys.error("fail"), 3)
if3(b < 22, println("A"), 3)

def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
val c = maybeTwice(true, { println("hi"); 1 + 41 })
println(c)

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}
println(maybeTwice2(true, { println("hi"); 1 + 41 }))

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    this match {
      case Empty      => Nil
      case Cons(h, t) => List(h()) ::: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) =>
        if (n == 1) Cons(() => h(), () => Empty)
        else Cons(() => h(), () => t().take(n - 1))
      case _ => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n == 0) Cons(() => h(), () => t())
        else t().drop(n - 1)
    }
  }

  def takeWhile_old(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) Cons(() => h(), () => t().takeWhile(p))
        else t().takeWhile(p)
    }
  }
  
  def takeWhile(p: A => Boolean): Stream[A] = 
  	foldRight(this)((a, b) => if (p(a)) Cons(() => a, () => b.takeWhile(p)) else b.takeWhile(p))
  
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def forAll(p: A => Boolean): Boolean =
  	foldRight(true)((a, b) => p(a) && b)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

val d = Stream(1, 2, 3).take(2).toList
println(d)
val e = Stream(1, 2, 3, 4).drop(2).toList
println(e)
val f = Stream(1, 2, 3, 4, 5, 6).takeWhile((x: Int) => x%2 == 0).toList
println(f)
val g = Stream(1, 2, 3, 9).forAll((x: Int) => x < 5)
println(g)
