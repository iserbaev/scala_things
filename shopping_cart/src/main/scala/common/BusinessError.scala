package common

import scala.util.control.NoStackTrace

sealed trait BusinessError extends NoStackTrace

object BusinessError {
  type RandomError = RandomError.type
  case object RandomError extends BusinessError
}