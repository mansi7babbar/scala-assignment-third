package com.knoldus

object VowelSubstrings extends App {
  def subString(str: String): Int = {
    val sum = new Array[Int](1)

    @scala.annotation.tailrec
    def subStringRecursive(str: String, sum: Array[Int], index: Int): Array[Int] = {
      if (index > str.length - 1) {
        sum
      }
      else {
        if (str(index) == 'a' || str(index) == 'e' || str(index) == 'i' || str(index) == 'o' || str(index) == 'u') {
          sum(0) = sum(0) + (index + 1) + ((index + 1) * (str.length - 1 - index))
        }
        subStringRecursive(str, sum, index + 1)
      }
    }

    subStringRecursive(str, sum, 0)
    sum(0)
  }

  val str: String = scala.io.StdIn.readLine("Enter the string: ")
  println("Sum of vowels present in all the possible substrings of the string: " + subString(str))
}
