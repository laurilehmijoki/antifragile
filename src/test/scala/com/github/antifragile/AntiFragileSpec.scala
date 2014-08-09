package com.github.antifragile

import com.amazonaws.services.s3.model.AmazonS3Exception
import com.github.antifragile.Unsafe._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

import scala.util.Try

class AntiFragileSpec extends Specification {

  "runUnsafe" should {
    "run the recoverWith operation if the unsafe operation fails" in {
      var recoverWithRan = 0
      runUnsafe({ throw new RuntimeException }, recoverWith = Some({
        case e: RuntimeException => recoverWithRan += 1; Try(())
      }: PartialFunction[Throwable, Try[Unit]]))
      recoverWithRan must beEqualTo(1)
    }

    "run the unsafe operation again after running the recoverWith operation" in {
      var unsafeRan = 0
      runUnsafe({ unsafeRan += 1; throw new RuntimeException }, recoverWith = Some({
        case e: RuntimeException => Try(())
      }: PartialFunction[Throwable, Try[Unit]]))
      unsafeRan must beEqualTo(2)
    }

    "return the value produced by the second unsafe call, if the first unsafe call failed" in {
      var unsafeCallCount = 0
      runUnsafe({
        unsafeCallCount += 1
        if (unsafeCallCount == 1) throw new RuntimeException
        else unsafeCallCount
      }, recoverWith = Some({
        case e: RuntimeException => Try(())
      }: PartialFunction[Throwable, Try[Unit]])) must equalTo(Right(2))
    }

    "translate the exception if the user provides a translator" in {
      runUnsafe(uploadToS3)(awsUnauthorizedTranslator) must beLike ({
        case Left(InternalErrorWithException(_, Some(e: InsufficientPermissions))) =>
          e.hint must startWith("Login to the Amazon AWS console")
      }: PartialFunction[Either[_, _], MatchResult[_]])
    }
  }

  class InsufficientPermissions(val originalMessage: String, val hint: String) extends Exception

  val awsUnauthorizedTranslator: ExceptionTranslator = {
    case e: AmazonS3Exception if e.getStatusCode == 403 =>
      new InsufficientPermissions(e.getMessage, "Login to the Amazon AWS console and add the user to the group 's3-users'")
  }

  def uploadToS3 = throw s3UnauthorizedException
  
  val s3UnauthorizedException = {
    val e = new AmazonS3Exception("Insufficient permissions")
    e.setErrorCode("AccountProblem") // http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html
    e.setStatusCode(403)
    e
  }
}
