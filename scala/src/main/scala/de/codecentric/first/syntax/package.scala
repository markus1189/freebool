package de.codecentric.first

package object syntax {
  implicit class PartialFunctionLifter[A,B](pf: PartialFunction[A, B]) {
    def liftFirst: A => First[B] = pf.lift.andThen(First.fromOption)
  }
}
