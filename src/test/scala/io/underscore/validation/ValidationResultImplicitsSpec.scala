package io.underscore.validation

import org.specs2.mutable._

class ValidationResultImplicitsSpec extends Specification {
  "ValidationResultImplicits" should {
    val UnknwonErrorCode = "UNKNOWN"
    val ErrorMessage = "error message"
    val ErrorPath = "error path"
    val WarningMessage = "warning message"
    val WarningPath = "warning path"

    val results = Seq(
      ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = ErrorPath :: PNil),
      ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = WarningPath :: PNil)
    )

    "allow the user to identify errors and warnings" in {
      results.hasErrors           mustEqual true
      results.hasWarnings         mustEqual true
      results.take(1).hasErrors   mustEqual true
      results.take(1).hasWarnings mustEqual false
      results.drop(1).hasErrors   mustEqual false
      results.drop(1).hasWarnings mustEqual true
    }

    "allow the user to extract errors and warnings" in {
      results.errors   mustEqual results.take(1)
      results.warnings mustEqual results.drop(1)
    }

    "allow the user to convert errors and warnings" in {
      results.toErrors mustEqual Seq(
        ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = ErrorPath :: PNil),
        ValidationError(code = UnknwonErrorCode, message = WarningMessage, path = WarningPath :: PNil)
      )
      results.toWarnings mustEqual Seq(
        ValidationWarning(code = UnknwonErrorCode, message = ErrorMessage, path = ErrorPath :: PNil),
        ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = WarningPath :: PNil)
      )
    }

    "allow the user to prefix results" in {
      results.prefix("field") mustEqual Seq(
        ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = "field" :: ErrorPath :: PNil),
        ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = "field" :: WarningPath :: PNil)
      )

      results.prefix(1234567) mustEqual Seq(
        ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = 1234567 :: ErrorPath :: PNil),
        ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = 1234567 :: WarningPath :: PNil)
      )
    }

    "allow the user to create a Validated" in {
      results.withValue("foo") mustEqual Validated("foo", results)
    }
  }
}