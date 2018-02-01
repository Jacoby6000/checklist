package checklist

import cats.{Eq, Order}
import cats.data.NonEmptyList

object Message {
  def errors[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): ErrorMessages =
    NonEmptyList.of(head, tail : _*).map(promoter.toError)

  def warnings[A](head: A, tail: A *)(implicit promoter: ToMessage[A]): WarningMessages =
    NonEmptyList.of(head, tail : _*).map(promoter.toWarning)
}

final case class ErrorMessage(text: String, path: Path = PNil)
object ErrorMessage {
  implicit def orderChecklistErrorMessage: Order[ErrorMessage] = Order.by[ErrorMessage, Path](_.path)

  implicit def eqChecklistErrorMessage: Eq[ErrorMessage] =
    Eq.fromUniversalEquals[ErrorMessage]

}

final case class WarningMessage(text: String, path: Path = PNil)

object WarningMessage {
  implicit def orderChecklistWarningMessage: Order[WarningMessage] = Order.by[WarningMessage, Path](_.path)

  implicit def eqChecklistWarningMessage: Eq[WarningMessage] =
    Eq.fromUniversalEquals[WarningMessage]
}

