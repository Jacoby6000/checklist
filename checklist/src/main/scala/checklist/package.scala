import cats.data.{Kleisli, Ior, NonEmptyList}
import checklist.MessageSyntax
import cats._

package object checklist {
  type Messages   = NonEmptyList[Message]
  type Checked[A] = Messages Ior A
  type CheckedF[F[_], A] = F[Checked[A]]
  type RuleF[F[_], A, B] = Kleisli[F, A, Checked[B]]
  type Rule[A, B] = RuleF[Id, A, B]
  type Rule1[A]   = Rule[A, A]
}
