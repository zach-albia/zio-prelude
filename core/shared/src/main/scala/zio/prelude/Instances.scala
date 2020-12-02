package zio.prelude

import zio.prelude.classic.Applicative
import zio.prelude.newtypes.{ BothF, NestedF }

object Instances {

  object Applicative {
    def apply[F[+_]](implicit F: Applicative[F]): Applicative[F] = F

    implicit def nestedF[F[+_], G[+_]](implicit
      covariant0: Covariant[({ type lambda[+A] = NestedF[F, G, A] })#lambda],
      identityBoth0: IdentityBoth[({ type lambda[+A] = NestedF[F, G, A] })#lambda]
    ): Applicative[({ type lambda[+A] = NestedF[F, G, A] })#lambda] = implicitly

    implicit def bothF[F[+_], G[+_]](implicit
      covariant0: Covariant[({ type lambda[+A] = BothF[F, G, A] })#lambda],
      identityBoth0: IdentityBoth[({ type lambda[+A] = BothF[F, G, A] })#lambda]
    ): Applicative[({ type lambda[+A] = BothF[F, G, A] })#lambda] = implicitly
  }

  trait ApplicativeDeriveEqual[F[+_]] extends Covariant[F] with IdentityBoth[F] with DeriveEqual[F]

  object ApplicativeDeriveEqual {
    implicit def derive[F[+_]](implicit
      covariant0: Covariant[F],
      identityBoth0: IdentityBoth[F],
      deriveEqual0: DeriveEqual[F]
    ): ApplicativeDeriveEqual[F] =
      new ApplicativeDeriveEqual[F] {
        def any: F[Any]                                     =
          identityBoth0.any
        def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)] =
          identityBoth0.both(fa, fb)
        def derive[A: Equal]: Equal[F[A]]                   =
          deriveEqual0.derive
        def map[A, B](f: A => B): F[A] => F[B]              =
          covariant0.map(f)
      }
  }
}
