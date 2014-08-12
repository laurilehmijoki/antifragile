package com.github.antifragile

import com.github.antifragile.Unsafe._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Try, Success, Failure}

sealed trait ErrorReport[T] {
  def report: String
  def internalReport: Option[T]
}

case class UserError(report: String, internalReport: Option[String] = None) extends ErrorReport[String]
case class InternalError(report: String = "Internal error", internalReport: Option[String]) extends ErrorReport[String]
case class InternalErrorWithException(report: String = "Internal error", internalReport: Option[Throwable]) extends ErrorReport[Throwable]

case class MultipleErrors(errors: Seq[ErrorReport[_]]) extends ErrorReport[String] {
  def report = errors.map(_.report).mkString(" | ")

  def internalReport: Option[String] = {
    val errs = errors.collect {
      case e if e.internalReport.isDefined => e.internalReport.get
    }
    Some(s"Multiple errors:\n$errs")
  }
}

sealed class Unsafe[T, C](
                           operation: => T,
                           recoveryPartial: PartialRecovery[C] = undefinedPartialRecovery,
                           exceptionTranslator: ExceptionTranslator[T] = undefinedExceptionTranslator
                           ) {

  def retryAfterRecoveringWith(op: PartialFunction[Throwable, Try[C]]) = new Unsafe(operation, recoveryPartial = op, exceptionTranslator)

  def translateExceptionWith(op: ExceptionTranslator[T]) = new Unsafe(operation, recoveryPartial, exceptionTranslator = op)

  def run: ErrOrOk[T] = {
    val firstTry = Try(operation)
    val retryApplied = firstTry recoverWith {
      case e: Throwable if recoveryPartial isDefinedAt e =>
        recoveryPartial apply e
        Try(operation)
    }
    retryApplied match {
      case Success(resultFromUnsafe) =>
        Right(resultFromUnsafe)

      case Failure(unsafeError) if exceptionTranslator isDefinedAt unsafeError =>
        exceptionTranslator apply unsafeError

      case Failure(unsafeError) =>
        Left(InternalErrorWithException(internalReport = Some(unsafeError)))
    }
  }
}

object Unsafe {
  def apply[T, C](op: => T): Unsafe[T, C] = new Unsafe[T, C](op)

  type ErrOrOk[T] = Either[ErrorReport[_], T]

  private type PartialRecovery[C] = PartialFunction[Throwable, Try[C]]

  type ExceptionTranslator[T] = PartialFunction[Throwable, ErrOrOk[T]]

  private[antifragile] def undefinedPartialRecovery[T]: PartialRecovery[T] = {
    case _ if false => ???
  }

  private[antifragile] def undefinedExceptionTranslator[T]: ExceptionTranslator[T] = {
    case _ if false => ???
  }

  def runUnsafeFuture[T](unsafeOperation: => Future[T])(implicit executionContext: ExecutionContextExecutor): Future[ErrOrOk[T]] =
    unsafeOperation map (Right(_)) recover {
      case e: Throwable => Left(InternalErrorWithException(internalReport = Some(e)))
    }
}