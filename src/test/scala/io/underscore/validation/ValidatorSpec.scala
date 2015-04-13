package io.underscore.validation

import org.specs2.mutable._

class ValidatorSpec extends Specification {
  "pass helper" should {
    "create an empty list of results" in {
      pass mustEqual Seq.empty
    }
  }

  "fail helper" should {
    "create a single error" in {
      fail(message = "message") mustEqual
        Seq(ValidationError(code = Validators.DefaultValidationCode, message = "message"))
    }
  }

  "warn helper" should {
    "create a single warning" in {
      warn(message = "message") mustEqual
        Seq(ValidationWarning(code = Validators.DefaultValidationCode, message = "message"))
    }
  }

  "validate[A]" >> {
    val validator = validate[Int]
    validator(+1) mustEqual pass
    validator(-1) mustEqual pass
  }

  val NotEvenErrorCode = "NOT_EVEN_NUMBER"
  val NotEvenErrorMessage = "Value must be even"
  def isEven: Validator[Int] = validate[Int](code = NotEvenErrorCode, message = NotEvenErrorMessage)(i => i % 2 == 0)

  "validate(message)(test)" >> {
    val validator = isEven
    validator(0) mustEqual pass
    validator(1) mustEqual fail(code = NotEvenErrorCode, message = NotEvenErrorMessage)
  }

  "warn constructor" >> {
    val validator = warn(isEven)
    validator(0) mustEqual pass
    validator(1) mustEqual warn(code = NotEvenErrorCode, message = NotEvenErrorMessage)
  }

  "optional" >> {
    val validator = optional(isEven)
    validator(None)    mustEqual pass
    validator(Some(0)) mustEqual pass
    validator(Some(1)) mustEqual fail(code = NotEvenErrorCode, message = NotEvenErrorMessage)
  }

  "required" >> {
    val validator = required(isEven)
    validator(None)    mustEqual fail(code = Validators.ValueRequired, message = "Value is required")
    validator(Some(0)) mustEqual pass
    validator(Some(1)) mustEqual fail(code = NotEvenErrorCode, message = NotEvenErrorMessage)
  }

  "eql" >> {
    val validator = eql(0)
    validator(0)  mustEqual pass
    validator(1)  mustEqual fail(code = Validators.MustBeEqual, message = "Must be 0")
    validator(-1) mustEqual fail(code = Validators.MustBeEqual, message = "Must be 0")
  }

  "neq" >> {
    val validator = neq(0)
    validator(0)  mustEqual fail(code = Validators.MustBeNotEqual, message = "Must not be 0")
    validator(1)  mustEqual pass
    validator(-1) mustEqual pass
  }

  "lt" >> {
    val validator = lt(0)
    validator(0)  mustEqual fail(code = Validators.MustBeLt, message = "Must be less than 0")
    validator(1)  mustEqual fail(code = Validators.MustBeLt, message = "Must be less than 0")
    validator(-1) mustEqual pass
  }

  "lte" >> {
    val validator = lte(0)
    validator(0)  mustEqual pass
    validator(1)  mustEqual fail(code = Validators.MustBeLte, message = "Must be 0 or less")
    validator(-1) mustEqual pass
  }

  "gt" >> {
    val validator = gt(0)
    validator(0)  mustEqual fail(code = Validators.MustBeGt, message = "Must be greater than 0")
    validator(1)  mustEqual pass
    validator(-1) mustEqual fail(code = Validators.MustBeGt, message = "Must be greater than 0")
  }

  "gte" >> {
    val validator = gte(0)
    validator(0)  mustEqual pass
    validator(1)  mustEqual pass
    validator(-1) mustEqual fail(code = Validators.MustBeGte, message = "Must be 0 or higher")
  }
  
  "nonEmpty" >> {
    val validator: Validator[String] = nonEmpty(code = Validators.NonEmpty, message = "Must not be empty")
    validator("")  mustEqual fail(code = Validators.NonEmpty, message = "Must not be empty")
    validator(" ") mustEqual pass
  }

  "nonEmpty non String" >> {
    val validator: Validator[Seq[Int]] = nonEmpty
    validator(List():List[Int])  mustEqual fail(code = Validators.NonEmpty, message = "Must not be empty")
    validator(List(1,2,3,4,5,6)) mustEqual pass
  }  
  
  "lengthLt"  >> {
    val validator: Validator[Seq[Int]] = lengthLt(6)
    validator(List(1,2,3,4,5,6))   mustEqual fail(code = Validators.LengthLt, message = "Length must be less than 6")
    validator(List():List[Int])    mustEqual pass
  }
  
  "lengthLte" >> {
    val validator: Validator[Seq[Int]] = lengthLte(6)
    validator(List(1,2,3,4,5,6))     mustEqual pass
    validator(List(1,2,3,4,5,6,7))   mustEqual fail(code = Validators.LengthLte, message = "Length must be at most 6")
  }  
  
  "lengthGt"  >> {
    val validator: Validator[Seq[Int]] = lengthGt(6)
    validator(List(1,2,3,4,5,6))    mustEqual fail(code = Validators.LengthGt, message = "Length must be more than 6")
    validator(List(1,2,3,4,5,6,7))  mustEqual pass     
  }  
  
  "lengthGte" >> {
    val validator: Validator[Seq[Int]] = lengthGte(6)
    validator(List(1,2,3,4,5))    mustEqual fail(code = Validators.LengthGte, message = "Length must be at least 6")
    validator(List(1,2,3,4,5,6))  mustEqual pass     
  }

  "matchesRegex" >> {
    val validator = matchesRegex(regex = "^[^@]+@[^@]+$".r, message = "Must be an email")
    validator("dave@example.com")  mustEqual pass
    validator("dave@")             mustEqual fail(code = Validators.INVALID_REGEX, message = "Must be an email")
    validator("@example.com")      mustEqual fail(code = Validators.INVALID_REGEX, message = "Must be an email")
    validator("dave@@example.com") mustEqual fail(code = Validators.INVALID_REGEX, message = "Must be an email")
  }

  "notContainedIn" >> {
    val validator = notContainedIn(List(1, 2, 3))
    validator(0) mustEqual pass
    validator(1) mustEqual fail(code = Validators.NotContainedIn, message = "Must not be one of the values 1, 2, 3")
    validator(2) mustEqual fail(code = Validators.NotContainedIn, message = "Must not be one of the values 1, 2, 3")
    validator(3) mustEqual fail(code = Validators.NotContainedIn, message = "Must not be one of the values 1, 2, 3")
    validator(4) mustEqual pass
  }

  "containedIn" >> {
    val validator = containedIn(List(1, 2, 3))
    validator(0) mustEqual fail(code = Validators.ContainedIn, message = "Must be one of the values 1, 2, 3")
    validator(1) mustEqual pass
    validator(2) mustEqual pass
    validator(3) mustEqual pass
    validator(4) mustEqual fail(code = Validators.ContainedIn, message = "Must be one of the values 1, 2, 3")
  }
}