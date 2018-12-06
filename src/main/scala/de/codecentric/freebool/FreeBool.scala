package de.codecentric.freebool

import java.time.ZonedDateTime

import cats.instances.vector._
import cats.syntax.foldable._
import scala.collection.immutable.Seq
import cats.data.Const
import cats.instances.int._
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Eval, Monoid, Traverse}
import spire.algebra.Bool
import spire.std.boolean._
import spire.syntax.heyting._

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.language.higherKinds

sealed abstract class FreeBool[+A]
case object One extends FreeBool[Nothing]
case object Zero extends FreeBool[Nothing]
case class And[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
case class Or[A](lhs: FreeBool[A], rhs: FreeBool[A]) extends FreeBool[A]
case class Complement[A](value: FreeBool[A]) extends FreeBool[A]
case class Inject[A](value: A) extends FreeBool[A]

object FreeBool {

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

  def partially[A, B: Bool](p: FreeBool[A])(
      interpret: A => Either[B, FreeBool[A]]): Either[B, FreeBool[A]] =
    run(p)(interpret)

  def partiallyAll[A, B: Bool](p: FreeBool[A])(fs: Vector[A => Either[B, FreeBool[A]]]): Either[B, FreeBool[A]] = {
    fs.foldLeftM(p)((acc, f) => partially[A, B](acc)(f))
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