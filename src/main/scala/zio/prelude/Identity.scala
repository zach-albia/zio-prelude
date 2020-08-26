package zio.prelude

import zio.prelude.coherent.EqualIdentity
import zio.test.TestResult
import zio.test.laws.{ Lawful, Laws }

/**
 * The `Identity` type class describes an associative binary operator for a
 * type `A` that also has an identity element. Combining any value with the
 * identity element on either the left or the right must return the original
 * value unchanged. For example, zero is an identity element for integer
 * addition and the empty string is an identity element for string
 * concatenation.
 *
 * Operators with an identity element are useful because the identity element
 * provides a sensible default value when combining values of a type and no
 * values exist.
 */
trait Identity[A] extends Associative[A] {

  /**
   * The identity element.
   */
  def identity: A
}

object Identity extends Lawful[EqualIdentity] {

  /**
   * The left identity law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * identity * a === a
   * }}}
   */
  val leftIdentityLaw: Laws[EqualIdentity] =
    new Laws.Law1[EqualIdentity]("leftIdentityLaw") {
      def apply[A](a: A)(implicit I: EqualIdentity[A]): TestResult =
        (I.identity <> a) <-> a
    }

  /**
   * The left identity law states that for some binary operator `*`, for all
   * values `a`, the following must hold:
   *
   * {{{
   * a * identity === a
   * }}}
   */
  val rightIdentityLaw: Laws[EqualIdentity] =
    new Laws.Law1[EqualIdentity]("rightIdentityLaw") {
      def apply[A](a: A)(implicit I: EqualIdentity[A]): TestResult =
        (a <> I.identity) <-> a
    }

  /**
   * The set of all laws that instances of `Identity` must satisfy.
   */
  val laws: Laws[EqualIdentity] =
    leftIdentityLaw + rightIdentityLaw

  /**
   * Summons an implicit `Identity[A]`.
   */
  def apply[A](implicit Identity: Identity[A]): Identity[A] = Identity

  /**
   * Constructs an `Identity` instance from an associative binary operator and
   * an identity element.
   */
  def make[A](identity0: A, op: (A, A) => A): Identity[A] =
    new Identity[A] {
      def identity: A                  = identity0
      def combine(l: => A, r: => A): A = op(l, r)
    }

  /**
   * Derives an `Identity[F[A]]` given a `Derive[F, Identity]` and an
   * `Identity[A]`.
   */
  implicit def DeriveIdentity[F[_], A](implicit derive: Derive[F, Identity], identity: Identity[A]): Identity[F[A]] =
    derive.derive(identity)

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple2Identity[A: Identity, B: Identity]: Identity[(A, B)] =
    new Identity[(A, B)] {
      def identity: (A, B) = (Identity[A].identity, Identity[B].identity)
      def combine(l: => (A, B), r: => (A, B)): (A, B) =
        (l._1 <> r._1, l._2 <> r._2)
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple3Identity[A: Identity, B: Identity, C: Identity]: Identity[(A, B, C)] =
    new Identity[(A, B, C)] {
      def identity: (A, B, C) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity)
      def combine(l: => (A, B, C), r: => (A, B, C)): (A, B, C) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3)
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple4Identity[A: Identity, B: Identity, C: Identity, D: Identity]: Identity[(A, B, C, D)] =
    new Identity[(A, B, C, D)] {
      def identity: (A, B, C, D) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity)
      def combine(l: => (A, B, C, D), r: => (A, B, C, D)): (A, B, C, D) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4)
    }

  implicit def Tuple5Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity]
    : Identity[(A, B, C, D, E)] =
    new Identity[(A, B, C, D, E)] {
      def identity: (A, B, C, D, E) =
        (Identity[A].identity, Identity[B].identity, Identity[C].identity, Identity[D].identity, Identity[E].identity)
      def combine(l: => (A, B, C, D, E), r: => (A, B, C, D, E)): (A, B, C, D, E) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5)
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple6Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity]
    : Identity[(A, B, C, D, E, F)] =
    new Identity[(A, B, C, D, E, F)] {
      def identity: (A, B, C, D, E, F) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity
        )
      def combine(l: => (A, B, C, D, E, F), r: => (A, B, C, D, E, F)): (A, B, C, D, E, F) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6)
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple7Identity[A: Identity, B: Identity, C: Identity, D: Identity, E: Identity, F: Identity, G: Identity]
    : Identity[(A, B, C, D, E, F, G)] =
    new Identity[(A, B, C, D, E, F, G)] {
      def identity: (A, B, C, D, E, F, G) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity
        )
      def combine(l: => (A, B, C, D, E, F, G), r: => (A, B, C, D, E, F, G)): (A, B, C, D, E, F, G) =
        (l._1 <> r._1, l._2 <> r._2, l._3 <> r._3, l._4 <> r._4, l._5 <> r._5, l._6 <> r._6, l._7 <> r._7)
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple8Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity
  ]: Identity[(A, B, C, D, E, F, G, H)] =
    new Identity[(A, B, C, D, E, F, G, H)] {
      def identity: (A, B, C, D, E, F, G, H) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity
        )
      def combine(l: => (A, B, C, D, E, F, G, H), r: => (A, B, C, D, E, F, G, H)): (A, B, C, D, E, F, G, H) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple9Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I)] =
    new Identity[(A, B, C, D, E, F, G, H, I)] {
      def identity: (A, B, C, D, E, F, G, H, I) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity
        )
      def combine(l: => (A, B, C, D, E, F, G, H, I), r: => (A, B, C, D, E, F, G, H, I)): (A, B, C, D, E, F, G, H, I) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple10Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J)] {
      def identity: (A, B, C, D, E, F, G, H, I, J) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J),
        r: => (A, B, C, D, E, F, G, H, I, J)
      ): (A, B, C, D, E, F, G, H, I, J) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple11Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K),
        r: => (A, B, C, D, E, F, G, H, I, J, K)
      ): (A, B, C, D, E, F, G, H, I, J, K) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple12Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L)
      ): (A, B, C, D, E, F, G, H, I, J, K, L) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple13Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple14Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple15Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple16Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple17Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple18Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple19Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple20Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple21Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity,
          Identity[U].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21
        )
    }

  /**
   * Derives an `Identity` for a product type given an `Identity` for each
   * element of the product type.
   */
  implicit def Tuple22Identity[
    A: Identity,
    B: Identity,
    C: Identity,
    D: Identity,
    E: Identity,
    F: Identity,
    G: Identity,
    H: Identity,
    I: Identity,
    J: Identity,
    K: Identity,
    L: Identity,
    M: Identity,
    N: Identity,
    O: Identity,
    P: Identity,
    Q: Identity,
    R: Identity,
    S: Identity,
    T: Identity,
    U: Identity,
    V: Identity
  ]: Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new Identity[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] {
      def identity: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          Identity[A].identity,
          Identity[B].identity,
          Identity[C].identity,
          Identity[D].identity,
          Identity[E].identity,
          Identity[F].identity,
          Identity[G].identity,
          Identity[H].identity,
          Identity[I].identity,
          Identity[J].identity,
          Identity[K].identity,
          Identity[L].identity,
          Identity[M].identity,
          Identity[N].identity,
          Identity[O].identity,
          Identity[P].identity,
          Identity[Q].identity,
          Identity[R].identity,
          Identity[S].identity,
          Identity[T].identity,
          Identity[U].identity,
          Identity[V].identity
        )
      def combine(
        l: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V),
        r: => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
      ): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) =
        (
          l._1 <> r._1,
          l._2 <> r._2,
          l._3 <> r._3,
          l._4 <> r._4,
          l._5 <> r._5,
          l._6 <> r._6,
          l._7 <> r._7,
          l._8 <> r._8,
          l._9 <> r._9,
          l._10 <> r._10,
          l._11 <> r._11,
          l._12 <> r._12,
          l._13 <> r._13,
          l._14 <> r._14,
          l._15 <> r._15,
          l._16 <> r._16,
          l._17 <> r._17,
          l._18 <> r._18,
          l._19 <> r._19,
          l._20 <> r._20,
          l._21 <> r._21,
          l._22 <> r._22
        )
    }
}

trait IdentitySyntax {

  /**
   * Provides infix syntax for combining two values with an associative
   * operation.
   */
  implicit class IdentityOps[A](l: A) {

    /**
     * Returns the identity element associated with values of this type.
     */
    def identity(implicit id: Identity[A]): A =
      id.identity
  }
}
