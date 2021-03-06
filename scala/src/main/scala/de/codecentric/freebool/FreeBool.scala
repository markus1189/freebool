package de.codecentric.freebool

import cats.data.Const
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Monad, Monoid, Traverse}
import de.codecentric.first.First
import spire.algebra.Bool
import spire.std.boolean._
import spire.syntax.heyting._

import scala.annotation.tailrec
import scala.language.higherKinds

sealed abstract class FreeBool[+A]
case object One extends FreeBool[Nothing]
case object Zero extends FreeBool[Nothing]
case class And[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
case class Or[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
case class Complement[A](value: FreeBool[A]) extends FreeBool[A]
case class Inject[A](value: A) extends FreeBool[A]

object FreeBool {

  def one: FreeBool[Nothing] = One
  def zero: FreeBool[Nothing] = Zero
  def and[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = And(lhs, rhs)
  def or[A](lhs: FreeBool[A], rhs: FreeBool[A]): FreeBool[A] = Or(lhs, rhs)
  def complement[A](value: FreeBool[A]): FreeBool[A] = Complement(value)
  def inject[A](value: A): FreeBool[A] = Inject(value)

  implicit val freeBoolMonad: Monad[FreeBool] = new Monad[FreeBool] {
    override def pure[A](x: A): FreeBool[A] = Inject(x)

    override def map[A, B](fa: FreeBool[A])(f: A => B): FreeBool[B] = fa match {
      case One               => One
      case Zero              => Zero
      case And(lhs, rhs)     => And(map(lhs)(f), map(rhs)(f))
      case Or(lhs, rhs)      => Or(map(lhs)(f), map(rhs)(f))
      case Complement(value) => Complement(map(value)(f))
      case Inject(value)     => Inject(f(value))
    }

    override def flatMap[A, B](fa: FreeBool[A])(
        f: A => FreeBool[B]): FreeBool[B] = fa match {
      case One               => One
      case Zero              => Zero
      case And(lhs, rhs)     => And(flatMap(lhs)(f), flatMap(rhs)(f))
      case Or(lhs, rhs)      => Or(flatMap(lhs)(f), flatMap(rhs)(f))
      case Complement(value) => Complement(flatMap(value)(f))
      case Inject(value)     => f(value)
    }

    override def tailRecM[A, B](a: A)(
        f: A => FreeBool[Either[A, B]]): FreeBool[B] = ???
  }

  implicit val freeBoolTraverse: Traverse[FreeBool] = new Traverse[FreeBool] {
    override def traverse[G[_], A, B](fa: FreeBool[A])(f: A => G[B])(
        implicit A: Applicative[G]): G[FreeBool[B]] = fa match {
      case One           => A.pure(One)
      case Zero          => A.pure(Zero)
      case And(lhs, rhs) => (traverse(lhs)(f), traverse(rhs)(f)).mapN(_ & _)
      case Or(lhs, rhs)  => (traverse(lhs)(f), traverse(rhs)(f)).mapN(_ | _)
      case Complement(p) => traverse(p)(f).map(~_)
      case Inject(a)     => f(a).map(Inject(_))
    }

    override def foldLeft[A, B](fa: FreeBool[A], b: B)(f: (B, A) => B): B =
      fa match {
        case One  => b
        case Zero => b
        case And(lhs, rhs) =>
          val lhs_ = foldLeft(lhs, b)(f)
          foldLeft(rhs, lhs_)(f)
        case Or(lhs, rhs) =>
          val lhs_ = foldLeft(lhs, b)(f)
          foldLeft(rhs, lhs_)(f)
        case Complement(value) => foldLeft(value, b)(f)
        case Inject(value)     => f(b, value)
      }

    override def foldRight[A, B](fa: FreeBool[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case One  => lb
      case Zero => lb
      case And(lhs, rhs) =>
        val lhs_ = foldRight(lhs, lb)(f)
        foldRight(rhs, lhs_)(f)
      case Or(lhs, rhs) =>
        val lhs_ = foldRight(lhs, lb)(f)
        foldRight(rhs, lhs_)(f)
      case Complement(c) => foldRight(c, lb)(f)
      case Inject(value) => f(value, lb)
    }
  }

  implicit def freeBoolIsBool[A]: Bool[FreeBool[A]] = new Bool[FreeBool[A]] {
    override def and(a: FreeBool[A], b: FreeBool[A]): FreeBool[A] = And(a, b)
    override def or(a: FreeBool[A], b: FreeBool[A]): FreeBool[A] = Or(a, b)
    override def complement(a: FreeBool[A]): FreeBool[A] = Complement(a)
    override def zero: FreeBool[A] = Zero
    override def one: FreeBool[A] = One
  }

  def all[A](as: FreeBool[A]*): FreeBool[A] =
    as.foldLeft[FreeBool[A]](One)((acc, a) => acc & a)

  def any[A](as: FreeBool[A]*): FreeBool[A] =
    as.foldLeft[FreeBool[A]](Zero)((acc, a) => acc | a)

  /////////////////////////////////////////////////////////////////////////////////////

  def run[A, B: Bool, F[_]: Applicative](p: FreeBool[A])(
      interpret: A => F[B]): F[B] = p.traverse(interpret).map(p => collapse(p))

  private[this] def optimize1[A](p: FreeBool[A]): FreeBool[A] = {
    p match {
      case One                       => One
      case Zero                      => Zero
      case And(Zero, rhs)            => Zero
      case And(lhs, Zero)            => Zero
      case And(lhs, rhs)             => And(optimize1(lhs), optimize1(rhs))
      case Or(One, rhs)              => One
      case Or(lhs, One)              => One
      case Or(lhs, rhs)              => Or(optimize1(lhs), optimize1(rhs))
      case Complement(Complement(v)) => optimize1(v)
      case Complement(value)         => Complement(optimize1(value))
      case Inject(value)             => p
    }
  }

  @tailrec def optimize[A](p: FreeBool[A]): FreeBool[A] = {
    val result = optimize1(p)

    if (result == p) {
      result
    } else {
      optimize(result)
    }
  }

  def compile[A](p: FreeBool[Nothing])(implicit bool: Bool[A]): A = {
    p match {
      case One               => bool.one
      case Zero              => bool.zero
      case And(lhs, rhs)     => bool.and(compile[A](lhs), compile[A](rhs))
      case Or(lhs, rhs)      => bool.or(compile[A](lhs), compile[A](rhs))
      case Complement(value) => bool.complement(compile[A](value))
      case Inject(value)     => throw new NotImplementedError() // not possible!
    }
  }

  // easily compose "partial" evaluators generated from PF on Lift.fromPf using monoids
  def partial[A](p: FreeBool[A])(f: A => First[FreeBool[A]]): FreeBool[A] = {
    p.flatMap(x => f(x).toOption.getOrElse(Monad[FreeBool].pure(x)))
  }

  // works, but not compositional (A => First[B]  is better)
  def partialWithPf[A](p: FreeBool[A])(pf: PartialFunction[A, FreeBool[A]]): FreeBool[A] = {
    p.flatMap(x => if (pf.isDefinedAt(x)) pf(x) else Inject(x))
  }

  def analyze[A, B: Monoid](p: FreeBool[A])(interpret: A => B): B =
    run(p)(x => Const.of[Boolean](interpret(x))).getConst

  def withdraw[A](p: FreeBool[A])(
      f: A => FreeBool[Nothing]): FreeBool[Nothing] =
    p match {
      case One               => One
      case Zero              => Zero
      case And(lhs, rhs)     => And(withdraw(lhs)(f), withdraw(rhs)(f))
      case Or(lhs, rhs)      => Or(withdraw(lhs)(f), withdraw(rhs)(f))
      case Complement(value) => Complement(withdraw(value)(f))
      case Inject(value)     => f(value)
    }

  def collapse[A: Bool](p: FreeBool[A]): A = {
    p match {
      case One               => Bool[A].one
      case Zero              => Bool[A].zero
      case And(lhs, rhs)     => collapse(lhs) & collapse(rhs)
      case Or(lhs, rhs)      => collapse(lhs) | collapse(rhs)
      case Complement(value) => ~collapse(value)
      case Inject(value) =>
        if (value == Bool[A].one) {
          Bool[A].one
        } else {
          Bool[A].zero
        }
    }
  }
}
