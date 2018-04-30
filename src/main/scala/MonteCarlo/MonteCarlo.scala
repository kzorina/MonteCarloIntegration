package MonteCarlo

import org.scalameter
import org.scalameter._

import scala.util.Random
import MyUtils._
object MonteCarlo {

  def function(x: Double): Double = {
    x*x
  }

  def integrateSeq(a: Double, b: Double, func: Double => Double = function, numberOfPoints: Int = 10000000): Double = {
    val rndx = new Random()
    val rndy = new Random()
    var sum = 0.0
    def simulation(pointsGenerated: Int): Double =
      if (pointsGenerated >= numberOfPoints)
        sum
      else{
        val x = a + rndx.nextDouble()*(b-a)  //The random point in the interval [a,b]
        sum+=function(x)*(b-a)
        simulation(pointsGenerated+1)
      }
    simulation(0)/numberOfPoints
  }
  def integratePar1(a: Double, b: Double, func: Double => Double = function, numberOfPoints: Int = 10000000): Double = {
    val (sum1, sum2)  = parallel(integrateSeq(a, b, func, numberOfPoints/2), integrateSeq(a, b, func, numberOfPoints/2))
    (sum1 + sum2)/2
  }
  def integratePar2(a: Double, b: Double, func: Double => Double = function, numberOfPoints: Int = 10000000): Double = {
    val middle = a + (b - a)/2
    val (sum1, sum2)  = parallel(integrateSeq(a, middle, func, numberOfPoints), integrateSeq(middle, b, func, numberOfPoints))
    sum1 + sum2
  }
  def integratePar3(a: Double, b: Double, func: Double => Double = function, numberOfPoints: Int = 10000000): Double = {
    val (sum1, sum2, sum3, sum4)  = parallel(integrateSeq(a, b, func, numberOfPoints/4), integrateSeq(a, b, func, numberOfPoints/4),integrateSeq(a, b, func, numberOfPoints/4), integrateSeq(a, b, func, numberOfPoints/4))
    (sum1 + sum2 + sum3 + sum4)/4
  }
  def integratePar4(a: Double, b: Double, func: Double => Double = function, numberOfPoints: Int = 10000000): Double = {
    val middle1 = a + (b - a)/4
    val middle2 = a + 2*(b - a)/4
    val middle3 = a + 3*(b - a)/4
    val (sum1, sum2, sum3, sum4)  = parallel(integrateSeq(a, middle1, func, numberOfPoints), integrateSeq(middle1, middle2, func, numberOfPoints), integrateSeq(middle2, middle3, func, numberOfPoints), integrateSeq(middle3, b, func, numberOfPoints))
    sum1 + sum2 + sum3 + sum4
  }
  def main(args: Array[String]): Unit = {
    val numberOfPoints = 1000000
    val (a, b) = (0, 2)

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false) withWarmer new scalameter.Warmer.Default


    println(integrateSeq(a, b, numberOfPoints = numberOfPoints))
    println(integratePar1(a, b, numberOfPoints = numberOfPoints))
    println(integratePar2(a, b, numberOfPoints = numberOfPoints))
    println(integratePar3(a, b, numberOfPoints = numberOfPoints))
    println(integratePar4(a, b, numberOfPoints = numberOfPoints))
    val seqtime = standardConfig measure {
      integrateSeq(a, b, numberOfPoints = numberOfPoints)
    }
    val partime1 = standardConfig measure {
      // JUST DO IT
      integratePar1(a, b, numberOfPoints = numberOfPoints)
    }
    val partime2 = standardConfig measure {
      // JUST DO IT
      integratePar2(a, b, numberOfPoints = numberOfPoints)
    }
    val partime3 = standardConfig measure {
      // JUST DO IT
      integratePar3(a, b, numberOfPoints = numberOfPoints)
    }
    val partime4 = standardConfig measure {
      // JUST DO IT
      integratePar4(a, b, numberOfPoints = numberOfPoints)
    }
    println(s"sequential time: $seqtime")
    println()
    // BEST SPEEDUP - VAR 1
    println(s"parallel time (variant 1): $partime1")
    println(s"speedup: ${seqtime.value/partime1.value} ")
    println()
    // BEST ACCURACY - VAR 2 and 4
    println(s"parallel time (variant 2): $partime2")
    println(s"speedup: ${seqtime.value/partime2.value} ")
    println()
    println(s"parallel time (variant 3): $partime3")
    println(s"speedup: ${seqtime.value/partime4.value} ")
    println()
    println(s"parallel time (variant 4): $partime4")
    println(s"speedup: ${seqtime.value/partime4.value} ")
  }
}
