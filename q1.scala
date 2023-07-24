object q1 {

  def cipher(word: String, shift: Int, f: (String, Int) => String): String = {
    f(word, shift)
  }

  def encryption(word: String, shift: Int): String = {
    val encryptedWord = new StringBuilder

    for (i <- 0 until word.length) {
      val originalLetter = word.charAt(i)
      val newLetter =
        if (originalLetter.isLetter) {
          val base = if (originalLetter.isUpper)
          {'A'} else
          {'a'}
          val newLetterCode = (originalLetter - base + shift) % 26 + base
          newLetterCode.toChar
        } else
        {originalLetter}
      encryptedWord.append(newLetter)
    }
    encryptedWord.toString()
  }

  def decryption(word: String, shift: Int): String = {
    val decryptedWord = new StringBuilder

    for (i <- 0 until word.length) {
      val originalLetter = word.charAt(i)
      val newLetter =
        if (originalLetter.isLetter) {
          val baseValue = if (originalLetter.isUpper)
          {'A'} else
          {'a'}
          val newLetterCode = (originalLetter - baseValue - shift + 26) % 26 + baseValue
          newLetterCode.toChar
        } else
        {originalLetter}
      decryptedWord.append(newLetter)
    }
    decryptedWord.toString()
  }

  def main(args: Array[String]): Unit = {
    println("Enter the word: ")
    val word = scala.io.StdIn.readLine()

    println("Enter the shift value: ")
    val shift = scala.io.StdIn.readInt()

    val encryptedWord = cipher(word, shift, encryption)
    println("Encrypted Word: " + encryptedWord)

    val decryptedWord = cipher(word, shift, decryption)
    println("Decrypted Word: " +  decryptedWord)
  }
}