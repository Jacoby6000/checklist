package checklist

import cats.data.NonEmptyList

trait MessageSyntax {
  def error(str: String): ErrorMessages =
    NonEmptyList(ErrorMessage(str), Nil)

  def warning(str: String): WarningMessages =
    NonEmptyList(WarningMessage(str), Nil)
}
