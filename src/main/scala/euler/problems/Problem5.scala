package euler.problems

import euler.utils.AdvancedMath

/**
  * @author guido
  */
object Problem5 extends App {

  private val factors = (BigInt(1) until BigInt(20)).flatMap(AdvancedMath.primeFactors)

  private def getMaxFactors(acc: Map[BigInt, BigInt], factor: (BigInt, BigInt)): Map[BigInt, BigInt] = {
    if(acc.getOrElse(factor._1, BigInt(0)) < factor._2) acc.updated(factor._1, factor._2)
    else acc
  }

  private val maxFactors: Map[BigInt, BigInt] = factors.foldLeft(Map.empty[BigInt, BigInt])(getMaxFactors)

  val max = maxFactors.foldLeft(BigInt(1))((acc: BigInt, factor: (BigInt, BigInt)) => acc * factor._1.^(factor._2))

  println(max)
}
