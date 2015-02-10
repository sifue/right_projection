/**
 * ユーザーとアドレスがそれぞれデータベースに格納されており、
 * ユーザーのIDを利用してその人の住む住所の郵便番号を取得を試みる
 *
 * そして、どの段階まで取得できたのかの結果オブジェクトを取得するような例
 * 非常にmatch caseのネストが深くなってしまう問題がある
 */
object MainBefore {

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

  // 4つの失敗パターンが存在する (toStringのために敢えてcase class)
  sealed abstract class PostalCodeResult
  case class Success(postalCode: String) extends PostalCodeResult
  abstract class Failure extends PostalCodeResult
  case class UserNotFound() extends Failure
  case class UserNotHasAddress() extends Failure
  case class AddressNotFound() extends Failure
  case class AddressNotHasPostalCode() extends Failure

  // どこでNoneが生じたか取得しようとするとfor式がつかえず地獄のようなネストになる
  def getPostalCodeResult(userId: Int): PostalCodeResult = {
    findUser(userId) match {
      case Some(user) =>
        user.addressId match {
          case Some(addressId) =>
            findAddress(addressId) match {
              case Some(address) =>
                address.postalCode match {
                  case Some(postalCode) => Success(postalCode)
                  case None => AddressNotHasPostalCode()
                }
              case None => AddressNotFound()
            }
          case None => UserNotHasAddress()
        }
      case None => UserNotFound()
    }
  }

  def findUser(userId: Int): Option[User] = {
    userDatabase.get(userId)
  }

  def findAddress(addressId: Int): Option[Address] = {
    addressDatabase.get(addressId)
  }

  def main(args: Array[String]): Unit = {
    Console.println(getPostalCodeResult(1)) // Success(150-0002)
    Console.println(getPostalCodeResult(2)) // AddressNotHasPostalCode()
    Console.println(getPostalCodeResult(3)) // UserNotHasAddress()
    Console.println(getPostalCodeResult(4)) // UserNotFound()
  }
}
