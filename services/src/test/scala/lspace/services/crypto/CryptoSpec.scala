package lspace.services.crypto

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CryptoSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Crypto" should {
    ".." in {
      // For 256 to work, see http://stackoverflow.com/questions/6481627/java-security-illegal-key-size-or-default-parameters
      val genKey    = Crypto.generateSecretKey("AES", 256)
      val message   = "ABCD"
      val crypto    = Crypto.AES(genKey)
      val encrypted = crypto.encryptToBase64(message)
      val decrypted = crypto.decryptBase64ToUTF8String(encrypted)
      message shouldBe decrypted
    }
  }
}
