import euler.utils.AdvancedMath

def isAbundant(n: Int): Boolean = AdvancedMath.properDivisors(n).sum > n

val is12 = isAbundant(12)

val is13 = isAbundant(13)
val is14 = isAbundant(14)
val is15 = isAbundant(15)
val is16 = isAbundant(16)
val is24 = isAbundant(24)