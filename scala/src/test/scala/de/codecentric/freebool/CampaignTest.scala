package de.codecentric.freebool

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, FunSuite}
import org.scalatest.prop.PropertyChecks
import spire.std.boolean._

class CampaignTest extends FlatSpec with PropertyChecks {
  import Gens._

  behavior of "Campaign"

  it should "not change meaning during optimization" in {
    println(freeBoolGen[Int](Gen.posNum[Int]).sample)
    forAll(freeBoolGen[Int](Gen.posNum[Int])) { fb: FreeBool[Int] =>
      println('.')
      def withdrawal(fbi: FreeBool[Int]): FreeBool[Nothing] = FreeBool.withdraw(fbi)(i => if (i % 2 == 0) One else Zero)

      FreeBool.collapse[Boolean](withdrawal(FreeBool.optimize(fb))) === FreeBool.collapse[Boolean](withdrawal(fb))
    }
  }


}


object Gens {
  def freeBoolGen[A](gen: Gen[A]): Gen[FreeBool[A]] = Gen.sized[FreeBool[A]](freeBoolGenSized(_)(gen))

  private[this] def freeBoolGenSized[A](size: Int)(gen: Gen[A]): Gen[FreeBool[A]] = {
    println(size)
    if (size <= 0) {
      Gen.oneOf(One, Zero)
    } else {
      val andGen: Gen[FreeBool[A]] = for {
        l <- freeBoolGenSized[A](size-1)(gen)
        r <- freeBoolGenSized[A](size-1)(gen)
      } yield And(l,r)

      val orGen: Gen[FreeBool[A]] = for {
        l <- freeBoolGenSized[A](size-1)(gen)
        r <- freeBoolGenSized[A](size-1)(gen)
      } yield Or(l,r)

      val complementGen: Gen[FreeBool[A]] = for {
        fb <- freeBoolGenSized[A](size - 1)(gen)
      } yield Complement(fb)

      val injectGen: Gen[FreeBool[A]] = for {
        injected <- gen
      } yield Inject(injected)

      Gen.oneOf(andGen, orGen, complementGen, injectGen, Gen.const(One), Gen.const(Zero))
    }
  }
}