import NewtypeRefinedOps._
import cats.data.EitherNel
import cats.syntax.all._
import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.types.all.NonEmptyString
import io.estatico.newtype.macros._

object model {
  @newtype case class Brand(value: NonEmptyString)
  @newtype case class Category(value: String)

  type UserNameR = NonEmptyString
  object UserNameR extends RefinedTypeOps[UserNameR, String]

  type NameR = NonEmptyString
  object NameR extends RefinedTypeOps[NameR, String]

  type EmailR = String Refined Contains['@']
  object EmailR extends RefinedTypeOps[EmailR, String]

  @newtype case class UserName(value: UserNameR)
  @newtype case class Name(value: NameR)
  @newtype case class Email(value: EmailR)

  case class Person(userNameR: UserName, name: Name, email: Email)
  object Person {
    def mkPerson(u: String, n: String, e: String): EitherNel[String, Person] =
      (
        validate[UserName](u),
        validate[Name](n),
        validate[Email](e)
      ).parMapN(Person.apply)

  }
}
