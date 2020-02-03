object VowelSubstrings extends App {
  def subString(str: String, n: Int): Int = {
    val sum = new Array[Int](1)

    def subStringRecursive(str: String, n: Int, sum: Array[Int], index: Int): Array[Int] = {
      if (index > n - 1) {
        sum
      }
      else {
        if (str(index) == 'a' || str(index) == 'e' || str(index) == 'i' || str(index) == 'o' || str(index) == 'u') {
          sum(0) = sum(0) + (index + 1) + ((index + 1) * (n - 1 - index))
        }
        subStringRecursive(str, n, sum, index + 1)
      }
    }

    subStringRecursive(str, n, sum, 0)
    sum(0)
  }

  val str: String = scala.io.StdIn.readLine("Enter the string: ")
  println("Sum of vowels present in all the possible substrings of the string: " + subString(str, str.length))
}
