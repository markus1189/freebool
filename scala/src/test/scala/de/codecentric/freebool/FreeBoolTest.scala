package de.codecentric.freebool

import org.scalacheck.Prop.BooleanOperators
import de.codecentric.first.syntax._
import cats.{Monoid, MonoidK}
import cats.data.Nested
import cats.implicits._
import de.codecentric.first.First
import org.scalacheck.{Gen, Prop, Shrink}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import spire.std.boolean._

class FreeBoolTest
    extends FlatSpec
    with Checkers
    with Matchers
    with TypeCheckedTripleEquals {
  behavior of "FreeBool"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 10, maxSize = 20, minSuccessful = 500)

  it should "not change the result during optimization" in {

    check(Prop.forAll(freeBoolGen(Gen.oneOf(true, false))) { p =>
      val notOptimized = FreeBool.collapse(p)
      val optimized = FreeBool.collapse(FreeBool.optimize(p))

      notOptimized == optimized
    })
  }

  trait FooBar extends Product with Serializable
  case object Foo extends FooBar
  case object Bar extends FooBar

  it should "do partial evaluation" in {

    check(Prop.forAll(freeBoolGen(Gen.oneOf(Foo, Bar))) { program =>
      val p1 = First.liftPf[FooBar] { case Foo => FreeBool.one }
      val p2 = First.liftPf[FooBar] { case Bar => FreeBool.zero }

      val programPartial = FreeBool.partial(program)(p1 |+| p2)

      val resultPartial = FreeBool.run(programPartial)(x => Option(x == Foo))
      val resultNormal = FreeBool.run(program)(x => Option(x == Foo))

      s"programPartial = $programPartial" |: s"resultPartial = $resultPartial" |: s"resultNormal = $resultNormal" |: resultPartial == resultNormal
    })
  }

  implicit def freeBoolShrink[A: Shrink]: Shrink[FreeBool[A]] = Shrink {
    case One            => Stream()
    case Zero           => Stream()
    case Complement(p2) => Shrink.shrink(p2) ++ Shrink.shrink(p2).map(FreeBool.complement)
    case Inject(i)      => Shrink.shrink(i).map(Inject(_))
    case And(lhs, rhs)  => Shrink.shrink(lhs) ++ Shrink.shrink(rhs) ++ (Shrink.shrink(lhs), Shrink.shrink(rhs)).zipped.map(And(_,_))
    case Or(lhs, rhs)   => Shrink.shrink(lhs) ++ Shrink.shrink(rhs) ++ (Shrink.shrink(lhs), Shrink.shrink(rhs)).zipped.map(Or(_,_))
  }

  def freeBoolGen[A](genA: Gen[A]): Gen[FreeBool[A]] = Gen.sized { size =>
    val pair =
      for {
        lhs <- Gen.sized(s => Gen.resize(s / 2, freeBoolGen[A](genA)))
        rhs <- Gen.sized(s => Gen.resize(s / 2, freeBoolGen[A](genA)))
      } yield (lhs, rhs)

    val oneGen: Gen[FreeBool[A]] = Gen.const(One)

    val zeroGen: Gen[FreeBool[A]] = Gen.const(Zero)

    val andGen: Gen[FreeBool[A]] = pair.map {
      case (lhs, rhs) => And(lhs, rhs)
    }

    val orGen: Gen[FreeBool[A]] = pair.map {
      case (lhs, rhs) => Or(lhs, rhs)
    }

    val complementGen: Gen[FreeBool[A]] =
      Gen.sized(s => Gen.resize(s - 1, freeBoolGen[A](genA).map(Complement(_))))

    val injectGen: Gen[FreeBool[A]] =
      Gen.sized(s => Gen.resize(s - 1, genA.map(Inject(_))))

    if (size <= 0) {
      Gen.oneOf(oneGen, zeroGen)
    } else {
      Gen.oneOf(complementGen, injectGen, andGen, orGen)
    }
  }
}
