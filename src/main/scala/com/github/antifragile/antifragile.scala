package com.github.antifragile

import scala.util.{Try, Success, Failure}

sealed trait ErrorReport[T] {
  def report: String
  def internalReport: Option[T]
}

case class InternalErrorWithException(report: String = "Internal error", internalReport: Option[Throwable]) extends ErrorReport[Throwable]

object Unsafe {
  type ErrOrOk[T] = Either[ErrorReport[_], T]

  type ExceptionTranslator = PartialFunction[Throwable, Throwable]

  def runUnsafe[T, C]
    (unsafeOperation: => T, recoverWith: Option[PartialFunction[Throwable, Try[C]]] = None)
    (implicit exceptionTranslator: ExceptionTranslator = noExceptionTranslation):
    ErrOrOk[T] =
    {
      def tryOperation = Try(unsafeOperation)
      val firstTry = tryOperation
      recoverWith.fold(firstTry)(
        partialRecovery => firstTry.recoverWith(partialRecovery).flatMap( _ => tryOperation )
      )
    } match {
      case Success(resultFromUnsafe) =>
        Right(resultFromUnsafe)

      case Failure(unsafeError) if exceptionTranslator isDefinedAt unsafeError =>
        Left(InternalErrorWithException(internalReport = Some(exceptionTranslator apply unsafeError)))

      case Failure(unsafeError) =>
        Left(InternalErrorWithException(internalReport = Some(unsafeError)))
    }

  private val noExceptionTranslation: ExceptionTranslator = {
    case e if false => ???
  }
}