import cats.data.EitherNel
import cats.syntax.all._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.refineV

object NewtypeRefinedOps {
  import io.estatico.newtype.Coercible
  import io.estatico.newtype.ops._

  final class NewtypeRefinedPartiallyApplied[A] {
    def apply[T, P](raw: T)(implicit c: Coercible[Refined[T, P], A], v: Validate[T, P]): EitherNel[String, A] =
      refineV[P](raw).toEitherNel.map(_.coerce[A])
  }
  def validate[A]: NewtypeRefinedPartiallyApplied[A] =
    new NewtypeRefinedPartiallyApplied[A]
}
