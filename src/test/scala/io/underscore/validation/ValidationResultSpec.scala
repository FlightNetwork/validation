package io.underscore.validation

import org.specs2.mutable._

class ValidationResultSpec extends Specification {
  "a validation error" should {
    val UnknwonErrorCode = "UNKNOWN"
    val ErrorMessage = "error message"

    val error = ValidationError(code = UnknwonErrorCode, message = ErrorMessage)

    "contain a code and a message and a path" in {
      error.code mustEqual "UNKNOWN"
      error.message mustEqual "error message"
      error.path mustEqual PNil
    }

    "be an error and not a warning" in {
      error.hasCode mustEqual true
      error.isError mustEqual true
      error.isWarning mustEqual false
    }

    "be prefixable" in {
      error.prefix("field") mustEqual
        ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = "field" :: PNil)
      error.prefix(1234567) mustEqual
        ValidationError(code = UnknwonErrorCode, message = ErrorMessage, path = 1234567 :: PNil)
    }

    "be convertable to a warning" in {
      error.toError mustEqual error
      error.toWarning mustEqual ValidationWarning(code = UnknwonErrorCode, message = ErrorMessage)
    }
  }

  "a validation warning" should {
    val UnknwonErrorCode = "UNKNOWN"
    val WarningMessage = "warning message"

    val warning = ValidationWarning(code = UnknwonErrorCode, message = WarningMessage)

    "contain a message and a path" in {
      warning.code mustEqual "UNKNOWN"
      warning.message mustEqual "warning message"
      warning.path mustEqual PNil
    }

    "be a warning and not an error" in {
      warning.hasCode mustEqual true
      warning.isError mustEqual false
      warning.isWarning mustEqual true
    }

    "be prefixable" in {
      warning.prefix("field") mustEqual
        ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = "field" :: PNil)
      warning.prefix(1234567) mustEqual
        ValidationWarning(code = UnknwonErrorCode, message = WarningMessage, path = 1234567 :: PNil)
    }

    "be convertable to an error" in {
      warning.toError mustEqual ValidationError(code = UnknwonErrorCode, message = WarningMessage)
      warning.toWarning mustEqual warning
    }
  }
}