package checklist

import cats._
import cats.data.{Ior, Kleisli}
import cats.implicits._
import monocle.{PLens, Lens}
import scala.language.higherKinds
import scala.util.matching.Regex
import Message.{errors, warnings}

trait RuleSyntax {

  implicit class AnyRuleOps[A](value: A) {
    def validate(implicit rule: Rule[A, A]): Checked[A] =
      rule(value)
  }

  implicit class RuleIdOps[A, B](rule: Rule[A, B]) {
    def lift[F[_]: Monad] = rule andThenCheck Monad[F[_]].pure
  }

  implicit class CheckedFOps[F[_], A](checked: CheckedF[F, A]) {
    def mapResult[B](f: A => B)(implicit M: Functor[F]): CheckedF[F, B] = checked.map(_.map(f))
    def leftMapResult[B](f: Messages => B)(implicit M: Functor[F]): F[B Ior A] = checked.map(_.leftMap(f))
    def rightMapResult[B](f: A => B)(implicit M: Functor[F]): CheckedF[F, B] = mapResult(f)

    def unlift(implicit M: Comonad[F]): Checked[A] = M.extract(checked)
  }

  implicit class RuleOps[F[_], A, B](self: RuleF[F, A, B]) {
    def mapResult[C](f: B => C)(implicit M: Functor[F]) = self.map(_.map(f))

    def zip[C](that: RuleF[F, A, C])(implicit flatMap: FlatMap[F]): RuleF[F, A, (B, C)] =
      Rule.pure { a =>
        self(a) flatMap {
          case Ior.Left(msg1) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
              case Ior.Both(msg2, c) => Ior.left(msg1 concat msg2)
              case Ior.Right(c)      => Ior.left(msg1)
            }
          case Ior.Both(msg1, b) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg1 concat msg2)
              case Ior.Both(msg2, c) => Ior.both(msg1 concat msg2, (b, c))
              case Ior.Right(c)      => Ior.both(msg1, (b, c))
            }
          case Ior.Right(b) =>
            that(a) match {
              case Ior.Left(msg2)    => Ior.left(msg2)
              case Ior.Both(msg2, c) => Ior.both(msg2, (b, c))
              case Ior.Right(c)      => Ior.right((b, c))
            }
        }
      }

    def flatMapResult[C](f: B => CheckedF[F, C])(implicit M: Monad[F]): RuleF[F, A, C] =
      self.flatMap(f)

    def andThenCheck[F[_]: FlatMap, C](that: RuleF[F, B, C])(implicit M: Monad[F]): RuleF[F, A, C] =
      self.flatMap(out => out.flatTraverse(that.apply))

    def seq[S[_]: Indexable: Traverse](implicit M: Applicative[F]): RuleF[F, S[A], S[B]] =
      Rule.sequence(self)

    def opt(implicit M: Monad[F]): RuleF[F, Option[A], Option[B]] =
      Rule.optional(self)

    def req(implicit M: Monad[F]): RuleF[F, Option[A], B] =
      Rule.required(self)

    def prefix[P: PathPrefix](prefix: P)(implicit M: Monad[F]): RuleF[F, A, B] =
      Rule.pure(value => self(value) leftMapResult (_ map (_.prefix(prefix))))

    def composeLens[S, T](lens: PLens[S, T, A, B])(implicit M: Functor[F]): RuleF[F, S, T] =
      Rule.pure(value => self(lens.get(value)) mapResult (lens.set(_)(value)))

    def at[P: PathPrefix, S, T](prefix: P, lens: PLens[S, T, A, B])(implicit M: Monad[F]): RuleF[F, S, T] =
      self composeLens lens prefix prefix
  }


  implicit class Rule1Ops[A](self: Rule[A, A]) {
    def field[B](path: Path, lens: Lens[A, B])(implicit rule: Rule[B, B]): Rule[A, A] =
      self andThenCheck rule.at(path, lens)

    def field[B](accessor: A => B)(implicit rule: Rule[B, B]): Rule[A, A] =
      macro RuleMacros.field[A, B]

    def fieldWith[B](path: Path, lens: Lens[A, B])(implicit builder: A => Rule[B, B]): Rule[A, A] =
      self andThenCheck Rule.pureId[A, A](value => builder(value).at(path, lens).apply(value))

    def fieldWith[B](accessor: A => B)(implicit builder: A => Rule[B, B]): Rule[A, A] =
      macro RuleMacros.fieldWith[A, B]
  }
}

object Rule extends BaseRules
  with ConverterRules
  with PropertyRules
  with CollectionRules
  with RuleInstances

trait BaseRules {
  def apply[A]: Rule[A, A] =
    pureId(Ior.right)

  def pure[F[_], A, B](func: A => F[Checked[B]]): RuleF[F, A, B] =
    Kleisli(func)

  def pureId[A, B](func: A => Checked[B]): Rule[A, B] = pure[Id, A, B](func)

  def pass[A]: Rule[A, A] =
    pureId(Ior.right)

  def fail[A](messages: Messages): Rule[A, A] =
    pureId(Ior.both(messages, _))
}

/** Rules that convert one type to another. */
trait ConverterRules {
  self: BaseRules =>

  val parseInt: Rule[String, Int] =
    parseInt(errors("Must be a whole number"))

  def parseInt(messages: Messages): Rule[String, Int] =
    pureId(value => util.Try(value.toInt).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val parseDouble: Rule[String, Double] =
    parseDouble(errors("Must be a number"))

  def parseDouble(messages: Messages): Rule[String, Double] =
    pureId(value => util.Try(value.toDouble).toOption.map(Ior.right).getOrElse(Ior.left(messages)))

  val trimString: Rule[String, String] =
    pureId(value => Ior.right(value.trim))
}

/** Rules that test a property of an existing value. */
trait PropertyRules {
  self: BaseRules =>

  def test[A](messages: => Messages)(func: A => Boolean): Rule[A, A] =
    pure(value => if(func(value)) Ior.right(value) else Ior.both(messages, value))

  def eql[A](comp: A): Rule[A, A] =
    eql(comp, errors(s"Must be ${comp}"))

  def eql[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ == comp)

  def neq[A](comp: A): Rule[A, A] =
    neq[A](comp: A, errors(s"Must not be ${comp}"))

  def neq[A](comp: A, messages: Messages): Rule[A, A] =
    test(messages)(_ != comp)

  def gt[A](comp: A)(implicit ord: Ordering[A]): Rule[A, A] =
    gt(comp, errors(s"Must be greater than ${comp}"))

  def gt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gt(_, comp))

  def lt[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lt(comp, errors(s"Must be less than ${comp}"))

  def lt[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lt(_, comp))

  def gte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    gte(comp, errors(s"Must be greater than or equal to ${comp}"))

  def gte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.gteq(_, comp))

  def lte[A](comp: A)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    lte(comp, errors(s"Must be less than or equal to ${comp}"))

  def lte[A](comp: A, messages: Messages)(implicit ord: Ordering[_ >: A]): Rule[A, A] =
    test(messages)(ord.lteq(_, comp))

  def nonEmpty[S <% Seq[_]]: Rule[S, S] =
    nonEmpty(errors(s"Must not be empty"))

  def nonEmpty[S <% Seq[_]](messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).nonEmpty)

  def lengthLt[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthLt(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLt[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length < comp)

  def lengthGt[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthGt(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGt[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length > comp)

  def lengthLte[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthLte(comp, errors(s"Must be length ${comp} or greater"))

  def lengthLte[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length <= comp)

  def lengthGte[S <% Seq[_]](comp: Int): Rule[S, S] =
    lengthGte(comp, errors(s"Must be length ${comp} or shorter"))

  def lengthGte[S <% Seq[_]](comp: Int, messages: Messages): Rule[S, S] =
    test(messages)(value => (value : Seq[_]).length >= comp)

  def matchesRegex(regex: Regex): Rule[String, String] =
    matchesRegex(regex, errors(s"Must match the pattern '${regex}'"))

  def matchesRegex(regex: Regex, messages: Messages): Rule[String, String] =
    test(messages)(regex.findFirstIn(_).isDefined)

  def containedIn[A](values: Seq[A]): Rule[A, A] =
    containedIn(values, errors(s"Must be one of the values ${values.mkString(", ")}"))

  def containedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => values contains value)

  def notContainedIn[A](values: Seq[A]): Rule[A, A] =
    notContainedIn(values, errors(s"Must not be one of the values ${values.mkString(", ")}"))

  def notContainedIn[A](values: Seq[A], messages: Messages): Rule[A, A] =
    test(messages)(value => !(values contains value))
}

trait CollectionRules {
  self: BaseRules =>

  import syntax._

  def optional[F[_], A, B](rule: RuleF[F, A, B])(implicit M: Monad[F]): RuleF[F, Option[A], Option[B]] =
    self.pure {
      case Some(value) => rule(value) mapResult (Some(_))
      case None        => Monad[F].pure(Ior.right(None))
    }

  def required[F[_]: Monad, A, B](rule: RuleF[F, A, B]): RuleF[F, Option[A], B] =
    required(rule, errors("Value is required"))

  def required[F[_]: Monad, A, B](rule: RuleF[F, A, B], messages: Messages): RuleF[F, Option[A], B] =
    self.pure[F, Option[A], B] {
      case Some(value) => rule(value)
      case None        => Monad[F].pure(Ior.left(messages))
    }

  def sequence[F[_]: Applicative, S[_] : Indexable : Traverse, A, B](rule: RuleF[F, A, B]): RuleF[F, S[A], S[B]] =
    self.pure { values =>
      Indexable[S].zipWithIndex(values).traverse {
        case (value, index) =>
          rule(value) leftMapResult (_ map (_ prefix index))
      }.map(_.sequenceU)
    }

  def mapValue[A: PathPrefix, B](key: A): Rule[Map[A, B], B] =
    mapValue[A, B](key, errors(s"Value not found"))

  def mapValue[A: PathPrefix, B](key: A, messages: Messages): Rule[Map[A, B], B] =
    self.pure[Id, Map[A, B], B](map => (map.get(key).map(Ior.right).getOrElse(Ior.left(messages map (_ prefix key)))))

  def mapValues[F[_]: Monad, A: PathPrefix, B, C](rule: RuleF[F, B, C]): RuleF[F, Map[A, B], Map[A, C]] =
    self.pure { in: Map[A, B] =>
      in.toList.traverse { case (key, value) =>
        rule.prefix(key).apply(value).map(_.map(key -> _))
      }.map(_.sequenceU.map(_.toMap))
    }
}

/** Type class instances for Rule */
trait RuleInstances {
  self: BaseRules =>

  import syntax._

  implicit def ruleApplicative[F[_]: Monad, A]: Applicative[RuleF[F, A, ?]] =
    new Applicative[RuleF[F, A, ?]] {
      def pure[B](value: B): RuleF[F, A, B] =
        Rule.pure(_ => Monad[F].pure(Ior.right(value)))

      def ap[B, C](funcRule: RuleF[F, A, B => C])(argRule: RuleF[F, A, B]): RuleF[F, A, C] =
        (funcRule zip argRule) map { _ map { case (func, arg) => func(arg) } }

      override def map[B, C](rule: Rule[A, B])(func: B => C): Rule[A, C] =
        rule map (_ map func)

      override def product[F[_]: FlatMap, B, C](rule1: RuleF[F, A, B], rule2: RuleF[F, A, C]): RuleF[F, A, (B, C)] =
        rule1 zip rule2
    }
}
