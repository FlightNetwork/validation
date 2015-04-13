package io.underscore.validation

import org.specs2.mutable._

class ValidationResultImplicitsSpec extends Specification {
  "ValidationResultImplicits" should {
    val results = Seq(
      ValidationError(message = "message1", path = "field1" :: PNil),
      ValidationWarning(message = "message2", path = "field2" :: PNil)
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
        ValidationError(message = "message1", path = "field1" :: PNil),
        ValidationError(message = "message2", path = "field2" :: PNil)
      )
      results.toWarnings mustEqual Seq(
        ValidationWarning(message = "message1", path = "field1" :: PNil),
        ValidationWarning(message = "message2", path = "field2" :: PNil)
      )
    }

    "allow the user to previc results" in {
      results.prefix("field") mustEqual Seq(
        ValidationError(message = "message1", path = "field" :: "field1" :: PNil),
        ValidationWarning(message = "message2", path = "field" :: "field2" :: PNil)
      )

      results.prefix(1234567) mustEqual Seq(
        ValidationError(message = "message1", path = 1234567 :: "field1" :: PNil),
        ValidationWarning(message = "message2", path = 1234567 :: "field2" :: PNil)
      )
    }

    "allow the user to create a Validated" in {
      results.withValue("foo") mustEqual Validated("foo", results)
    }
  }
}