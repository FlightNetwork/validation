package io.underscore.validation

import scala.math.Ordering
import scala.util.matching.Regex

object Validators extends Validators {
  val DefaultValidationCode = "UNKNOWN"
  val NonEmpty = "NON_EMPTY"
  val ValueRequired = "VALUE_REQUIRED"
  val MustBeNotEqual = "MUST_BE_NOT_EQUAL"
  val MustBeEqual = "MUST_BE_EQUAL"
  val MustBeLt = "MUST_BE_LESS_THAN"
  val MustBeLte = "MUST_BE_LESS_THAN_OR_EQUAL_TO"
  val MustBeGt = "MUST_BE_GREATER_THAN"
  val MustBeGte = "MUST_BE_GREATER_THAN_OR_EQUAL_TO"
  val LengthLt = "LENGTH_MUST_BE_LESS_THAN"
  val LengthLte = "LENGTH_MUST_BE_LESS_THAN_OR_EQUAL_TO"
  val LengthGt = "LENGTH_MUST_BE_GREATER_THAN"
  val LengthGte = "LENGTH_MUST_BE_GREATER_THAN_OR_EQUAL_TO"
  val INVALID_REGEX = "INVALID_REGEX"
  val NotContainedIn = "MUST_NOT_BE_ONE_OF_VALUES"
  val ContainedIn = "MUST_BE_ONE_OF_VALUES"
}

trait Validators {
  def pass: Seq[ValidationResult] =
    Seq.empty

  def fail(code: => String = Validators.DefaultValidationCode, message: => String): Seq[ValidationResult] =
    Seq(ValidationError(code, message))

  def warn(code: => String = Validators.DefaultValidationCode, message: => String): Seq[ValidationResult] =
    Seq(ValidationWarning(code, message))

  def validate[A]: Validator[A] =
    Validator[A] { in => pass }

  def validate[A](code: => String, message: => String)(func: A => Boolean): Validator[A] =
    Validator[A] { in => if (func(in)) pass else fail(code, message) }

  def warn[A](rule: Validator[A]) =
    Validator[A] { in => rule(in).toWarnings }

  def optional[A](rule: Validator[A]): Validator[Option[A]] =
    Validator[Option[A]] { in => in map rule getOrElse pass }

  def required[A](rule: Validator[A]): Validator[Option[A]] =
    required(Validators.ValueRequired, "Value is required", rule)

  def required[A](code: => String, message: => String, rule: Validator[A]): Validator[Option[A]] =
    Validator[Option[A]] { in => in map rule getOrElse fail(code, message) }

  def eql[A](comp: A): Validator[A] =
    eql(comp, Validators.MustBeEqual, s"Must be $comp")

  def eql[A](comp: A, code: => String, message: => String): Validator[A] =
    Validator[A] { in => if (in == comp) pass else fail(code, message) }

  def neq[A](comp: A): Validator[A] =
    neq(comp, Validators.MustBeNotEqual, s"Must not be $comp")

  def neq[A](comp: A, code: => String, message: => String): Validator[A] =
    Validator[A] { in => if (in == comp) fail(code, message) else pass }

  def lt[A](comp: A)(implicit order: Ordering[A]): Validator[A] =
    lt(comp, Validators.MustBeLt, s"Must be less than $comp")

  def lt[A](comp: A, code: => String, message: => String)(implicit order: Ordering[A]): Validator[A] =
    Validator[A] { in => if (order.lt(in, comp)) pass else fail(code, message) }

  def lte[A](comp: A)(implicit order: Ordering[A]): Validator[A] =
    lte(comp, Validators.MustBeLte, s"Must be $comp or less")

  def lte[A](comp: A, code: => String, message: => String)(implicit order: Ordering[A]): Validator[A] =
    Validator[A] { in => if (order.lteq(in, comp)) pass else fail(code, message) }

  def gt[A](comp: A)(implicit order: Ordering[A]): Validator[A] =
    gt(comp, Validators.MustBeGt, s"Must be greater than $comp")

  def gt[A](comp: A, code: => String, message: => String)(implicit order: Ordering[A]): Validator[A] =
    Validator[A] { in => if (order.gt(in, comp)) pass else fail(code, message) }

  def gte[A](comp: A)(implicit order: Ordering[A]): Validator[A] =
    gte(comp, Validators.MustBeGte, s"Must be $comp or higher")

  def gte[A](comp: A, code: => String, message: => String)(implicit order: Ordering[A]): Validator[A] =
    Validator[A] { in => if (order.gteq(in, comp)) pass else fail(code, message) }

  def nonEmpty[E, S <% Seq[E]]: Validator[S] =
    nonEmpty(Validators.NonEmpty, "Must not be empty")

  def nonEmpty[E, S <% Seq[E]](code: => String, message: => String): Validator[S] =
    Validator[S] { in => if (in.isEmpty) fail(code, message) else pass }

  def lengthLt[E, S <% Seq[E]](comp: Int): Validator[S] =
    lengthLt(comp, Validators.LengthLt, s"Length must be less than $comp")

  def lengthLt[E, S <% Seq[E]](comp: Int, code: => String, message: => String): Validator[S] =
    Validator[S] { in => if (in.length < comp) pass else fail(code, message) }

  def lengthLte[E, S <% Seq[E]](comp: Int): Validator[S] =
    lengthLte(comp, Validators.LengthLte, s"Length must be at most $comp")

  def lengthLte[E, S <% Seq[E]](comp: Int, code: => String, message: => String): Validator[S] =
    Validator[S] { in => if (in.length <= comp) pass else fail(code, message) }

  def lengthGt[E, S <% Seq[E]](comp: Int): Validator[S] =
    lengthGt(comp, Validators.LengthGt, s"Length must be more than $comp")

  def lengthGt[E, S <% Seq[E]](comp: Int, code: => String, message: => String): Validator[S] =
    Validator[S] { in => if (in.length > comp) pass else fail(code, message) }

  def lengthGte[E, S <% Seq[E]](comp: Int): Validator[S] =
    lengthGte(comp, Validators.LengthGte, s"Length must be at least $comp")

  def lengthGte[E, S <% Seq[E]](comp: Int, code: => String, message: => String): Validator[S] =
    Validator[S] { in => if (in.length >= comp) pass else fail(code, message) }

  def matchesRegex(regex: Regex, code: => String = Validators.INVALID_REGEX, message: => String): Validator[String] =
    Validator[String] { in => if (regex.findFirstIn(in).isDefined) pass else fail(code, message) }

  def notContainedIn[A](values: => Seq[A]): Validator[A] =
    notContainedIn(values, Validators.NotContainedIn, s"Must not be one of the values ${values.mkString(", ")}")

  def notContainedIn[A](values: => Seq[A], code: => String, message: => String): Validator[A] =
    Validator[A] { in => if (values contains in) fail(code, message) else pass }

  def containedIn[A](values: => Seq[A]): Validator[A] =
    containedIn(values, Validators.ContainedIn, s"Must be one of the values ${values.mkString(", ")}")

  def containedIn[A](values: => Seq[A], code: => String, message: => String): Validator[A] =
    Validator[A] { in => if (values contains in) pass else fail(code, message) }
}
