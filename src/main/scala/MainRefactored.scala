/**
 * MainBeforeをEitherを利用してリファクタリングを行ったコード
 */
object MainRefactored {

  case class Address(id: Int, name: String, postalCode: Option[String])
  case class User(id: Int, name: String, addressId: Option[Int])

  val userDatabase: Map[Int, User] = Map (
    1 -> User(1, "太郎", Some(1)),
    2 -> User(2, "二郎", Some(2)),
    3 -> User(3, "プー太郎", None)
  )

  val addressDatabase: Map[Int, Address] = Map (
    1 -> Address(1, "渋谷", Some("150-0002")),
    2 -> Address(2, "国際宇宙ステーション", None)
  )

  // 4つの失敗パターンが存在する
  sealed abstract class PostalCodeResult
  case class Success(postalCode: String) extends PostalCodeResult
  abstract class Failure extends PostalCodeResult
  case class UserNotFound() extends Failure
  case class UserNotHasAddress() extends Failure
  case class AddressNotFound() extends Failure
  case class AddressNotHasPostalCode() extends Failure

  // 本質的に何をしているかわかりやすくリファクタリング
  def getPostalCodeResult(userId: Int): PostalCodeResult = {
    (for {
      user <- findUser(userId).right
      address <- findAddress(user).right
      postalCode <- findPostalCode(address).right
    } yield Success(postalCode)).merge
  }

  def findUser(userId: Int): Either[Failure, User] = {
    userDatabase.get(userId) match {
      case Some(user) => Right(user)
      case None => Left(UserNotFound())
    }
  }

  def findAddress(user: User): Either[Failure, Address] = {
    user.addressId match {
      case Some(addressId) =>
        addressDatabase.get(addressId) match {
          case Some(address) => Right(address)
          case None => Left(AddressNotFound())
        }
      case None => Left(UserNotHasAddress())
    }
  }

  def findPostalCode(address: Address): Either[Failure, String] = {
    address.postalCode match {
      case Some(postalCode) => Right(postalCode)
      case None => Left(AddressNotHasPostalCode())
    }
  }

  def main(args: Array[String]): Unit = {
    Console.println(getPostalCodeResult(1)) // Success(150-0002)
    Console.println(getPostalCodeResult(2)) // AddressNotHasPostalCode()
    Console.println(getPostalCodeResult(3)) // UserNotHasAddress()
    Console.println(getPostalCodeResult(4)) // UserNotFound()
  }
}
