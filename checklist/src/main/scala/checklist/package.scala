import cats.data.NonEmptyList

package object checklist {
  type ErrorMessages   = NonEmptyList[ErrorMessage]
  type WarningMessages = NonEmptyList[WarningMessage]
  type Rule1[A]   = Rule[A, A]

  type CheckedRule[A] = Checked[ErrorMessage, WarningMessage, A]
}
