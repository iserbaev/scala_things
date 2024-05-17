package category

import cats.MonadThrow
import cats.effect.std.Random
import cats.syntax.all._
import common.BusinessError.RandomError
import common.model.Category

trait Categories[F[_]] {
  def findAll: F[List[Category]]
}

object Categories {
  def make[F[_]: MonadThrow: Random]: Categories[F] =
    new Categories[F] {
      def findAll: F[List[Category]] =
        Random[F].nextInt.flatMap {
          case n if n > 100 =>
            List.empty[Category].pure[F]
          case _ =>
            RandomError.raiseError[F, List[Category]]

        }
    }
}
