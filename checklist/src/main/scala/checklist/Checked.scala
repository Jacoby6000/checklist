package checklist

import cats.implicits._
import cats.{Semigroup, Monad, Monoid}
import cats.data.{NonEmptyList, Ior, Validated}

sealed trait Checked[E, W, R] {
  def widen[EE >: E, WW >: W, RR >: R]: Checked[EE, WW, RR] = this.asInstanceOf[Checked[EE, WW, RR]]
  def widenErrors[EE >: E]: Checked[EE, W, R] = this.asInstanceOf[Checked[EE, W, R]]
  def widenWarnings[WW >: W]: Checked[E, WW, R] = this.asInstanceOf[Checked[E, WW, R]]
  def widenResult[RR >: R]: Checked[E, W, RR] = this.asInstanceOf[Checked[E, W, RR]]

  def fold[A](err: (NonEmptyList[E], List[W]) => A, warn: (NonEmptyList[W], R) => A, succ: R => A): A =
    this match {
      case Errored(e, w) => err(e, w)
      case Warned(w, r) => warn(w, r)
      case Succeeded(r) => succ(r)
    }

  def recover(func: (NonEmptyList[E], List[W]) => R): Checked[E, W, R] = recoverWith((e, w) => Succeeded(func(e, w)))

  def recoverWith(func: (NonEmptyList[E], List[W]) => Checked[E, W, R]): Checked[E, W, R] =
    this match {
      case Errored(errs, warns) => func(errs, warns)
      case other => other
    }

  def combine(other: Checked[E, W, R])(implicit semigroup: Semigroup[R]): Checked[E, W, R] = combineWith(other)(semigroup.combine)

  def combineWith[S, T](other: Checked[E, W, S])(f: (R, S) => T): Checked[E, W, T] =
    this match {
      case Errored(errs, warns) =>
        other.fold(
          (otherErrs, otherWarns) => Errored(errs |+| otherErrs, warns ++ otherWarns),
          (otherWarns, _) => Errored(errs, warns ++ otherWarns.toList),
          _ => Errored(errs, warns)
        )
      case Warned(warns, result) =>
        other.fold(
          (otherErrs, otherWarns) => Errored(otherErrs, warns.toList ++ otherWarns),
          (otherWarns, otherResult) => Warned(warns |+| otherWarns, f(result, otherResult)),
          otherResult => Warned(warns, f(result, otherResult))
        )
      case Succeeded(result) =>
        other.fold(
          Errored.apply _,
          (warns, otherResult) => Warned(warns, f(result, otherResult)),
          otherResult => Succeeded(f(result, otherResult))
        )
    }

  def map[S](f: R => S): Checked[E, W, S] =
    fold(
      Errored.apply _,
      (w, r) => Warned(w, f(r)),
      r => Succeeded(f(r))
    )

  def mapWarnings[X](f: W => X): Checked[E, X, R] =
    fold(
      (e, ws) => Errored(e, ws.map(f)),
      (ws, r) => Warned(ws.map(f), r),
      Succeeded(_)
    )

  def mapErrors[F](f: E => F): Checked[F, W, R] =
    fold(
      (es, w) => Errored(es.map(f), w),
      Warned(_, _),
      Succeeded(_)
    )

  def flatMap[S](f: R => Checked[E, W, S]): Checked[E, W, S] =
    fold(
      Errored(_, _),
      (_, result) => combineWith(f(result))((_, s) => s),
      result => combineWith(f(result))((_, s) => s)
    )

  def value: Option[R] = fold((_, _) => None, (_, r) => Some(r), Some(_))
  def warnings: List[W] = fold((_, w) => w, (w, _) => w.toList, _ => Nil)
  def errors: List[E] = fold((errs, _) => errs.toList, (_, _) => Nil, _ => Nil)

  def iorValidated: List[W] Ior Validated[NonEmptyList[E], R] =
    fold(
      (es, ws) => Ior.both(ws, Validated.invalid(es)),
      (ws, result) => Ior.both(ws.toList, Validated.valid(result)),
      result => Ior.right(Validated.valid(result))
    )

  def destructured: (List[E], List[W], Option[R]) =
    fold(
      (errs, warns) => (errs.toList, warns, None),
      (warns, result) => (Nil, warns.toList, Some(result)),
      result => (Nil, Nil, Some(result))
    )

  def isValid: Boolean = fold((_, _) => false, (_, _) => false, _ => true)
  def hasAValue: Boolean = fold((_, _) => false, (_, _) => true, _ => true)
  def hasErrors: Boolean = fold((_, _) => true, (_, _) => false, _ => false)
  def hasNoErrors: Boolean = fold((_, _) => false, (_, _) => true, _ => true)
  def hasWarnings: Boolean = fold((_, w) => w.nonEmpty, (_, _) => true, _ => false)
  def hasNoWarnings: Boolean = fold((_, w) => w.isEmpty, (_, _) => false, _ => true)
}

object Checked {
  def succeeded[E, W, R](result: R): Checked[E, W, R] = Succeeded(result)

  def errored[E, W, R](errors: NonEmptyList[E]): Checked[E, W, R] = Errored(errors, Nil)
  def erroredWithWarnings[E, W, R](errors: NonEmptyList[E], warnings: List[W]): Checked[E, W, R] = Errored(errors, warnings)

  def warned[E, W, R](warnings: NonEmptyList[W], result: R): Checked[E, W, R] = Warned(warnings, result)

  implicit def checkedMonad[E, W]: Monad[Checked[E, W, ?]] =
    new Monad[Checked[E, W, ?]] {
      def ap[A, B](ff: Checked[E, W, A => B])(fa: Checked[E, W, A]): Checked[E, W, B] = ff.combineWith(fa)(_ apply _)
      def pure[A](a: A): Checked[E, W, A] = Succeeded(a)
      def map[A, B](fa: Checked[E, W, A])(f: A => B): Checked[E, W, B] = fa.map(f)
      def flatMap[A, B](fa: Checked[E, W, A])(f: A => Checked[E, W, B]): Checked[E, W, B] = fa.flatMap(f)
    }

  implicit def checkedMonoid[E, W, A](implicit monoid: Monoid[A]): Monoid[Checked[E, W, A]] =
    new Monoid[Checked[E, W, A]] {
      def empty: Checked[E, W, A] = Succeeded(monoid.empty)
      def combine(l: Checked[E, W, A], r: Checked[E, W, A]): Checked[E, W, A] = l.combine(r)(monoid)
    }

}

case class Errored[E, W, R](errors: NonEmptyList[E], warnings: List[W]) extends Checked[E, W, R]
case class Warned[E, W, R](warnings: NonEmptyList[W], result: R) extends Checked[E, W, R]
case class Succeeded[E, W, R](result: R) extends Checked[E, W, R]

trait CheckedSyntax {
  implicit class CheckedOps[A](value: A) {
    def succeeded[E, W]: Checked[E, W, A] = Succeeded(value)
  }
}
