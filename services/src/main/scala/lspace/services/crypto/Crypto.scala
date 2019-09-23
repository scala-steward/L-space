package lspace.services.crypto

import java.util.Base64

import javax.crypto.{Cipher, KeyGenerator}
import javax.crypto.spec.SecretKeySpec

class Crypto(algorithm: String, b64secret: String) {

  lazy val secret                               = decodeBase64(b64secret)
  val decoder                                   = cipher(Cipher.DECRYPT_MODE, b64secret)
  val encoder                                   = cipher(Cipher.ENCRYPT_MODE, b64secret)
  def decodeBase64(string: String): Array[Byte] = Base64.getDecoder.decode(string)
  def encodeBase64(bytes: Array[Byte]): String  = Base64.getEncoder.encodeToString(bytes)

  private def cipher(mode: Int, b64secret: String): Cipher = {
    val encipher = Cipher.getInstance(algorithm + "/ECB/PKCS5Padding")
    encipher.init(mode, new SecretKeySpec(secret, algorithm))
    encipher
  }

  def encryptToBytes(message: String): Array[Byte]    = encryptToBytes(message.getBytes("UTF-8"))
  def encryptToBytes(bytes: Array[Byte]): Array[Byte] = encoder.doFinal(bytes)
  def encryptToBase64(message: String): String        = encodeBase64(encoder.doFinal(message.getBytes("UTF-8")))
  def encryptToBase64(bytes: Array[Byte]): String     = encodeBase64(encoder.doFinal(bytes))

  def decryptBase64(coded: String): Array[Byte]            = decoder.doFinal(decodeBase64(coded))
  def decryptBytes(bytes: Array[Byte]): Array[Byte]        = decoder.doFinal(bytes)
  def decryptBytesToUTF8String(bytes: Array[Byte]): String = new String(decoder.doFinal(bytes), "UTF-8")
  def decryptBase64ToUTF8String(coded: String): String     = new String(decoder.doFinal(decodeBase64(coded)), "UTF-8")
}

object Crypto {
  def apply(algorithm: String, b64secret: String): Crypto = new Crypto(algorithm, b64secret)
  object AES {
    def apply(b64secret: String): Crypto     = new Crypto("AES", b64secret)
    def generateSecretKey(size: Int): String = Crypto.generateSecretKey("AES", size)
  }
  object DES {
    def apply(b64secret: String): Crypto     = new Crypto("DES", b64secret)
    def generateSecretKey(size: Int): String = Crypto.generateSecretKey("DES", size)
  }

  def generateSecretKey(algorithm: String, size: Int): String =
    Base64.getEncoder.encodeToString({
      val generator = KeyGenerator.getInstance(algorithm)
      generator.init(size)
      generator.generateKey().getEncoded
    })

}
