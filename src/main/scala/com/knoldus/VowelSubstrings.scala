package com.knoldus

object VowelSubstrings extends App {
  def subString(str: String): Int = {
    @scala.annotation.tailrec
    def subStringRecursive(str: String, sum: Int, index: Int): Int = {
      if (index > str.length - 1) {
        sum
      }
      else {
        if (str(index) == 'a' || str(index) == 'e' || str(index) == 'i' || str(index) == 'o' || str(index) == 'u') {
          subStringRecursive(str, sum + ((index + 1) * (str.length - index)), index + 1)
        }
        else {
          subStringRecursive(str, sum, index + 1)
        }

      }
    }

    subStringRecursive(str, 0, 0)
  }

  val str = scala.io.StdIn.readLine("Enter the string: ")
  println("Sum of vowels present in all the possible substrings of the string: " + subString(str.toLowerCase()))
}