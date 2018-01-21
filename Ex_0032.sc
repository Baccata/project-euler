import scala.annotation.tailrec

@tailrec
def digits(number : Int, acc : List[Int] = Nil) : List[Int] =
  if(number < 0 ) acc
  else if (number < 10) number :: acc
  else {
    val lastDigit = number % 10
    digits((number - lastDigit) / 10, lastDigit :: acc)
  }

def pandigital(digits : List[Int]) = digits.sorted == (1 to 9).toList

def pandigitalProduct(multiplicand : Int, multiplier : Int) : Option[(Int, Int, Int)] = {
  val product = multiplicand * multiplier
  val allDigits = digits(product) ++ digits(multiplicand) ++ digits(multiplier)
  if (pandigital(allDigits)) Some((multiplicand, multiplier, product)) else None
}

val result = (for (x <- 1 to 100; y <- 100 to 9999) yield pandigitalProduct(x, y)).collect {
  case Some((x, y, z)) => println(x,y,z) ; z
}.toSet.sum

println(result)

