package de.codecentric.first

import cats.Monoid

sealed trait First[+A] {
  def toOption: Option[A]
}

case object Never extends First[Nothing] {
  override def toOption: Option[Nothing] = None
}

case class Found[A](value: A) extends First[A] {
  override def toOption: Option[A] = Some(value)
}

object First {
  sealed abstract class FirstLiftPfPartiallyApplied[A] {
    def apply[B](pf: PartialFunction[A, B]): A => First[B] = pf.lift.andThen(fromOption)
  }

  def found[A](value: A): First[A] = Found(value)
  val never: First[Nothing] = Never

  def fromOption[A](option: Option[A]): First[A] = option.fold[First[A]](Never)(Found(_))

  def liftPf[A]: FirstLiftPfPartiallyApplied[A] = new FirstLiftPfPartiallyApplied[A] {}

  implicit def firstMonoid[A]: Monoid[First[A]] = new Monoid[First[A]] {
    override def empty: First[A] = Never

    override def combine(x: First[A], y: First[A]): First[A] = x match {
      case Found(_) => x
      case Never    => y
    }
  }
}