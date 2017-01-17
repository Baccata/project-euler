//By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
//
//What is the 10 001st prime number?

lazy val primes : Stream[Int] = 2 #:: Stream.from(3).filter { i => 
  primes.takeWhile(x => x * x <= i).forall(i % _ > 0)
}

val res = primes.drop(10000.head)
println(res)
