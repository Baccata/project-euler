//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

def factorize(x: Int): List[Int] = {
  def foo(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a*a > x match {
    case false if x % a == 0 => foo(x / a, a    , a :: list)
    case false               => foo(x    , a + 1, list)
    case true                => x :: list
  }
  foo(x)
}

def factWithExp(n : Int) : List[(Int, Int)] = {
  val factorized = factorize(n)
  for {
    x <- factorized.distinct 
  } yield (x, factorized.count(_ == x))
}

val res = (2 to 20)
  .toList
  .flatMap(factWithExp)
  .groupBy(_._1)
  .map(_._2.sorted.last)
  .map{case (n, count) => Math.pow(n, count)}
  .reduce(_ * _)
  
println(res)
