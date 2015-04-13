package io.underscore.validation

sealed trait ValidationResult {
  def code: String

  def message: String

  def path: ValidationPath

  def prefix[A: ValidationPathPrefix](prefix: A): ValidationResult

  def hasCode: Boolean

  def isError: Boolean

  def isWarning: Boolean

  def toError: ValidationError

  def toWarning: ValidationWarning
}

case class ValidationError(code: String,
                           message: String,
                           path: ValidationPath = PNil) extends ValidationResult {
  def prefix[A: ValidationPathPrefix](prefix: A) =
    this.copy(path = prefix :: path)

  def hasCode = !code.isEmpty

  val isError = true
  val isWarning = false

  def toError = this

  def toWarning = ValidationWarning(code, message, path)
}

case class ValidationWarning(code: String,
                             message: String,
                             path: ValidationPath = PNil) extends ValidationResult {
  def prefix[A: ValidationPathPrefix](prefix: A) =
    this.copy(path = prefix :: path)

  def hasCode = !code.isEmpty

  val isError = false
  val isWarning = true

  def toError = ValidationError(code, message, path)

  def toWarning = this
}
