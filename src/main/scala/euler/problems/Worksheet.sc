import euler.utils.AdvancedMath

def triangleNumbers(n: BigInt, acc: BigInt): Stream[BigInt] = (n + acc) #:: triangleNumbers(n + 1, n + acc)

val triangleStream = triangleNumbers(1, 0)
val first = triangleStream.map(AdvancedMath.primeFactors)

first.take(10).last