package example

import io.chymyst.ch._
import cats.Functor
import cats.evidence.Is
import cats.syntax.functor._
import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/*
If a PTVF p1 is defined for all the same types as p2 and also for some other types,
we say that p2 requires p1.
We want to express this. Also, we want to use both p1 and p2 on types for which p2 is defined,
by just using a p2 type class constraint, without explicitly adding a p1 type class constraint.

scalaz and cats use traits that extend each other - and then you can't add syntax, and have to override the functions in the trait. This looks suspicious.

There is another way: use abstract class instead of trait, and require superclass constraint.
 */

abstract class FilterableWithFilter[F[_]](implicit val functor: Functor[F]) {
  // For functors that implement `withFilter` directly.
  def withFilter[A](p: A ⇒ Boolean)(fa: F[A]): F[A]
}

// A `Functor` instance is required for creating a `Filterable` instance.
abstract class Filterable[F[_]](implicit val functor: Functor[F]) {
  // The `deflate` is the easiest type signature to implement.
  def deflate[A](fa: F[Option[A]]): F[A]
}

// Syntax for PTVFs.
object Filterable {

  // Helper function to transform a Boolean isomorphically into 1 + 1.
  implicit class BooleanToOptionUnit(val b: Boolean) extends AnyVal {
    def toOptionUnit: Option[Unit] = if (b) Some(()) else None
  }

  // Transform A ⇒ Boolean isomorphically into A ⇒ 1 + A.
  def bop[A](predicate: A ⇒ Boolean): A ⇒ Option[A] = {
    x ⇒ predicate(x).toOptionUnit.map(_ ⇒ x)
  }

  // Flip arguments of curried functions.
  def flip[A, B, C]: (A ⇒ B ⇒ C) ⇒ (B ⇒ A ⇒ C) = implement

  implicit class CurriedFlip[A, B, C](val f: A ⇒ B ⇒ C) extends AnyVal {
    def flip: B ⇒ A ⇒ C = ofType[B ⇒ A ⇒ C](f)
  }

  // Define `.deflate` syntax.
  implicit class Syntax1[F[_], A](fa: F[Option[A]])(implicit ev: Filterable[F]) {
    def deflate: F[A] = ev.deflate(fa)
  }

  // Define `.mapOption`, `.filter` and `.withFilter` syntax if we already have `.deflate` syntax.
  implicit class Syntax2[F[_], A](fa: F[A])(implicit ev: Filterable[F]) {
    implicit val functor: Functor[F] = ev.functor

    def mapOption[B](f: A ⇒ Option[B]): F[B] = fa.map(f).deflate

    def filter(p: A ⇒ Boolean): F[A] = mapOption(a ⇒ Some(a).filter(p))

    def withFilter(p: A ⇒ Boolean): F[A] = filter(p)
  }

  // Define `.mapOption`, `.withFilter`, `.filter`, and `.deflate` syntax if we already have `withFilter`.
  implicit class Syntax3[F[_], A](fa: F[A])(implicit ev: FilterableWithFilter[F]) {
    implicit val functor: Functor[F] = ev.functor

    def mapOption[B](f: A ⇒ Option[B]): F[B] = fa.map(f).deflate

    def withFilter(p: A ⇒ Boolean): F[A] = ev.withFilter(p)(fa)

    def filter(p: A ⇒ Boolean): F[A] = withFilter(p)

    def deflate[B](implicit aOpt: A Is Option[B]): F[B] =
      aOpt.substitute(fa).withFilter(_.nonEmpty).map { case Some(x) ⇒ x }
  }

}

trait FilterableLawChecking extends Matchers with GeneratorDrivenPropertyChecks {
  // Check the four laws for `filter`.
  def checkFilterableLawsWithFilter[F[_] : FilterableWithFilter, A, B](fcEqual: (F[B], F[B]) ⇒ Assertion = (x: F[B], y: F[B]) ⇒ x shouldEqual y)(implicit
    ff: cats.Functor[F],
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[B]],
    abEv: Arbitrary[A ⇒ B],
    aEv: Arbitrary[A ⇒ Boolean],
    bEv: Arbitrary[B ⇒ Boolean]
  ): Assertion = {
    import Filterable._

    // Naturality law.
    forAll { (f: A ⇒ B, p: B ⇒ Boolean, fa: F[A]) ⇒
      fcEqual(fa.map(f).filter(p), fa.filter(f andThen p).map(f))
    }

    // Conjunction law.
    forAll { (p1: B ⇒ Boolean, p2: B ⇒ Boolean, fa: F[B]) ⇒
      fcEqual(fa.filter(p1).filter(p2), fa.filter(b ⇒ p1(b) && p2(b)))
    }

    // Identity law.
    forAll { (fb: F[B]) ⇒ fcEqual(fb.filter(_ ⇒ true), fb) }

    // Partial function law.
    forAll { (f: A ⇒ B, p: A ⇒ Boolean, fa: F[A]) ⇒
      fcEqual(fa.filter(p).map(f), fa.filter(p).map[B] { case x if p(x) ⇒ f(x) })
    }
  }

  // Check the two laws for `mapOption`.
  def checkFilterableLaws[F[_] : Filterable, A, B, C](fcEqual: (F[C], F[C]) ⇒ Assertion = (x: F[C], y: F[C]) ⇒ x shouldEqual y)(implicit
    faEv: Arbitrary[F[A]],
    fcEv: Arbitrary[F[C]],
    abEv: Arbitrary[A ⇒ Option[B]],
    bcEv: Arbitrary[B ⇒ Option[C]]
  ): Assertion = {
    import Filterable._

    // Identity law.
    forAll { (fc: F[C]) ⇒ fcEqual(fc.mapOption(Some.apply[C]), fc) }

    // Composition law.
    forAll { (f: A ⇒ Option[B], g: B ⇒ Option[C], fa: F[A]) ⇒
      fcEqual(fa.mapOption(f).mapOption(g), fa.mapOption(x ⇒ f(x) flatMap g))
    }
  }

}
