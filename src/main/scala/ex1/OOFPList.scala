package ex1

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = flatMap(a => (a, value) :: Nil()) //foldLeft(Nil())((b, a) => b append (a, value) :: Nil())
  def length(): Int = foldLeft(0)((b, a) => b + 1)
  def zipWithIndex: List[(A, Int)] = foldRight(Nil())((a, b) => (a, this.length() - b.length() - 1) :: Nil() append b)
  def partition(predicate: A => Boolean): (List[A], List[A]) = foldRight((Nil(), Nil()))((a, b) => 
    if predicate(a) then (a :: Nil() append b._1, b._2) else (b._1, a :: Nil() append b._2)) //(filter(predicate), filter(a => !predicate(a)))
  def span(predicate: A => Boolean): (List[A], List[A]) = foldRight((Nil(), Nil()))((a, b) => 
    if (predicate(a) && b._2.length() > 1) then (a :: Nil() append b._1, b._2) else (b._1, a :: Nil() append b._2))
  def takeRight(n: Int): List[A] = foldLeft((Nil(): List[A], Nil(): List[A]))((b, a) => 
    if(this.length() - b._2.length() == n) then (b._1 append a :: Nil(), b._2) else (b._1, b._2 append a :: Nil()))._1
    /*this match
    case h :: t if t.length() == n => t
    case h :: t => t.takeRight(n)
    case _ => Nil()*/

  def collect(predicate: PartialFunction[A, A]): List[A] = filter(predicate.isDefinedAt(_)).map(predicate)
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*
  val reference = List(1, 2, 3, 4)
  println(reference)
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  //println(reference.reduce(_ + _)) // 10
  //println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)
  println(reference.collect { case x if x % 2 != 0 => x * 2 }) // List(2, 6)