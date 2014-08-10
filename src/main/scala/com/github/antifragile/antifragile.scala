package com.github.antifragile

import com.github.antifragile.Unsafe.{ErrOrOk, ExceptionTranslator}

import scala.concurrent.Future
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

  def runUnsafeFuture[T](unsafeOperation: => Future[T])(implicit executionContext: : Future[ErrOrOk[T]] =
    unsafeOperation map (Right(_)) recover {
      case e: Throwable => Left(InternalErrorWithException(internalReport = Some(e)))
    }
}