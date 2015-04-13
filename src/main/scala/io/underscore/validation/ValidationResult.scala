package io.underscore.validation

sealed trait ValidationResult {
  def code: String
  def message: String
  def path: ValidationPath

  def prefix[A: ValidationPathPrefix](prefix: A): ValidationResult

  def isError: Boolean
  def isWarning: Boolean

  def toError: ValidationError
  def toWarning: ValidationWarning
}

case class ValidationError(code: String = ValidationResult.DefaultValidationErrorCode,
                           message: String,
                           path: ValidationPath = PNil) extends ValidationResult {
  def prefix[A: ValidationPathPrefix](prefix: A) =
    this.copy(path = prefix :: path)

  val isError   = true
  val isWarning = false

  def toError   = this
  def toWarning = ValidationWarning(code, message, path)
}

case class ValidationWarning(code: String = ValidationResult.DefaultValidationErrorCode,
                             message: String,
                             path: ValidationPath = PNil) extends ValidationResult {
  def prefix[A: ValidationPathPrefix](prefix: A) =
    this.copy(path = prefix :: path)

  val isError   = false
  val isWarning = true

  def toError   = ValidationError(code, message, path)
  def toWarning = this
}

object ValidationResult {
  val DefaultValidationErrorCode = "UNKNOWN"
}
