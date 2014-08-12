package com.github.antifragile

import com.amazonaws.services.s3.model.AmazonS3Exception
import com.github.antifragile.Unsafe._
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.util.Try

class AntiFragileSpec extends Specification {

  "Unsafe" should {
    "retry after recovering from an exception" in new EmptyS3Bucket {
      val unsafe = Unsafe { uploadToNonExistentS3Bucket } retryAfterRecoveringWith {
        case e: RuntimeException => Try(createS3Bucket)
      }
      unsafe.run
      bucket.exists must beTrue
    }

    "it runs the operation only once, the retry is not defined" in {
      var runCount = 0
      val unsafe = Unsafe(runCount += 1).run
      runCount must beEqualTo(1)
    }

    "run the operation and wrap the successful result in Right" in {
      Unsafe(Unit).run must beEqualTo(Right(Unit))
    }

    "run the operation and wrap the failure in Left" in {
      Unsafe(throw new AmazonS3Exception("S3 failed")).run must haveClass[Left[ErrorReport[_], _]]
    }

    "translate the exception if the user provides a translator" in {
      val unsafe = Unsafe(unauthorizedS3Upload) translateExceptionWith awsUnauthorizedTranslator
      unsafe.run must beLike {
        case Left(InternalError(_, Some(hint))) =>
          hint must startWith("Login to the Amazon AWS console")
      }
    }
  }

  trait EmptyS3Bucket extends Scope {
    implicit val bucket: S3Bucket = S3Bucket()
  }

  class InsufficientPermissions(val originalMessage: String, val hint: String) extends Exception

  val awsUnauthorizedTranslator: ExceptionTranslator[_] = {
    case e: AmazonS3Exception if e.getStatusCode == 403 =>
      Left(InternalError(internalReport = Some("Login to the Amazon AWS console and add the user to the group 's3-users'")))
  }

  def createS3Bucket(implicit bucket: S3Bucket) = bucket.exists = true

  def uploadToExistingS3Bucket = Unit

  def uploadToNonExistentS3Bucket(implicit bucket: S3Bucket) =
    if (bucket.exists) ()
    else throw noSuchS3Bucket

  def unauthorizedS3Upload = throw s3UnauthorizedException
  
  val s3UnauthorizedException = {
    val e = new AmazonS3Exception("Insufficient permissions")
    e.setErrorCode("AccountProblem") // http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html
    e.setStatusCode(403)
    e
  }

  case class S3Bucket(var exists: Boolean = false)

  val noSuchS3Bucket = {
    val e = new AmazonS3Exception("Bucket does not exist")
    e.setErrorCode("NoSuchBucket") // http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html
    e.setStatusCode(404)
    e
  }
}
