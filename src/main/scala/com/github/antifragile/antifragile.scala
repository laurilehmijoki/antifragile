package com.github.antifragile

import com.github.antifragile.Unsafe.{ErrOrOk, ExceptionTranslator}

import scala.util.{Try, Success, Failure}

sealed trait ErrorReport[T] {
  def report: String
  def internalReport: Option[T]
}

case class InternalErrorWithException(report: String = "Internal error", internalReport: Option[Throwable]) extends ErrorReport[Throwable]

sealed abstract class Unsafe[T, C] {
  def operation: T

  private var recoveryPartial: PartialFunction[Throwable, Try[C]] = {
    case _ if false => ??? // this partial function is never defined
  }

  private var exceptionTranslator: ExceptionTranslator = {
    case _ if false => ??? // this partial function is never defined
  }

  def retryAfterRecoveringWith(op: PartialFunction[Throwable, Try[C]]) = {
    recoveryPartial = op
    this
  }

  def translateExceptionWith(op: ExceptionTranslator) = {
    exceptionTranslator = op
    this
  }

  def run: ErrOrOk[T] =
    Try(operation) recoverWith recoveryPartial flatMap (_ => Try(operation)) match {
      case Success(resultFromUnsafe) =>
        Right(resultFromUnsafe)

      case Failure(unsafeError) if exceptionTranslator isDefinedAt unsafeError =>
        Left(InternalErrorWithException(internalReport = Some(exceptionTranslator apply unsafeError)))

      case Failure(unsafeError) =>
        Left(InternalErrorWithException(internalReport = Some(unsafeError)))
    }
}

object Unsafe {
  def apply[T, C](op: => T): Unsafe[T, C] = new Unsafe[T, C] { def operation = op }

  type ErrOrOk[T] = Either[ErrorReport[_], T]

  type ExceptionTranslator = PartialFunction[Throwable, Throwable]
}