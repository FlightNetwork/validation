package io.underscore.validation

import org.specs2.mutable._

class ValidatedSpec extends Specification {
  "map and flatMap" should {
    "propagate the value in a success" in {
      (for {
        a <- Validated(0)
        b <- Validated(a + 1)
        c <- Validated(b + 1)
        d <- Validated(c + 1)
      } yield d) mustEqual Validated(3)
    }

    "propagate warnings in a success" in {
      val UnknwonErrorCode = "UNKNOWN"
      val FailMessage = "failed at c"

      (for {
        a <- Validated(0)
        b <- Validated(a + 1)
        c <- Validated(b + 1, Seq(ValidationWarning(code = UnknwonErrorCode, message = FailMessage)))
        d <- Validated(c + 1)
      } yield d) mustEqual Validated(3, Seq(ValidationWarning("UNKNOWN", "failed at c")))
    }

    "shortcut in an error" in {
      val UnknwonErrorCode = "UNKNOWN"
      val FailMessage = "failed at c"

      (for {
        a <- Validated(0)
        b <- Validated(a + 1)
        c <- Validated(b + 1, Seq(ValidationError(code = UnknwonErrorCode ,message = "failed at c")))
        d <- Validated(c + 1)
      } yield d) mustEqual Validated(2, Seq(ValidationError("UNKNOWN", "failed at c")))
    }
  }
}