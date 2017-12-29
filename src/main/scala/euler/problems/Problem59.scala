package euler.problems

import euler.utils.{Primes, Problem}

import scala.io.Source

/**
  * Problem 59
  *
  * Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code
  * for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
  *
  * A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given
  * value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the
  * cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
  *
  * For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random
  * bytes. The user would keep the encrypted message and the encryption key in different locations, and without both
  * "halves", it is impossible to decrypt the message.
  *
  * Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If
  * the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
  * The balance for this method is using a sufficiently long password key for security, but short enough to be
  * memorable.
  *
  * Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher.txt (right
  * click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain
  * text must contain common English words, decrypt the message and find the sum of the ASCII values in the original
  * text.
  *
  **/
object Problem59 extends Problem with App {

  type Key = Seq[Byte]

  def keys(): Stream[Key] = keys(Seq(0x00, 0x00, 0x00))

  def keys(key: Key): Stream[Key] = key #:: {
    if (key(2) < 0x7A) keys(Seq(key.head, key(1), (key(2) + 1).toByte))
    else if (key(1) < 0x7A) keys(Seq(key.head, (key(1) + 1).toByte, 0x61))
    else if (key.head < 0x7A) keys(Seq((key.head + 1).toByte, 0x61, 0x61))
    else Stream.empty
  }

  def keyStream(key: Key): Stream[Byte] = key.head #:: keyStream(key.tail :+ key.head)

  def xorMessage(message: Stream[Byte], key: Key): Stream[Byte] = {
    message.zip(keyStream(key))
      .map(a => (a._1 ^ a._2).toByte)
  }

  def stringToByte(a: String): Byte = a.toInt.toByte

  override def solution(): Any = {
    val message = Source.fromResource("problem_59.txt").mkString.trim().split(",").map(stringToByte).toStream

    val xoredMessage = keys()
      .map(key => (key, xorMessage(message, key)))
      .filter(key => key._2.forall(b => b >= 0x20 && b <= 0x7E))
      .map(key => (key._1, key._2.map(_.toChar).foldLeft("")(_ + _)))
      .filter(key => key._2.contains(" the "))
      .head._2

    xoredMessage.split("").map(_.charAt(0)).map(c => c.toByte.toLong).sum
  }

  println(solution())
}
