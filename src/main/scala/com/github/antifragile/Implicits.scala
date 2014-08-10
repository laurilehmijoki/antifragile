package com.github.antifragile

object Implicits {
  object RightBias {
    private type ?[A] = Either[ErrorReport[_], A]
    implicit def rightBias[A](x: ?[A]): Either.RightProjection[ErrorReport[_], A] = x.right
  }
}