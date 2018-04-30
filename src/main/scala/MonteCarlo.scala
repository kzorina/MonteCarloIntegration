import org.scalameter
import org.scalameter._

import scala.util.Random

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
  def main(args: Array[String]): Unit = {
    val numberOfPoints = 100000
    val (a, b) = (0, 2)

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false) withWarmer new scalameter.Warmer.Default


    println(integrateSeq(a, b, numberOfPoints = numberOfPoints))
    val seqtime = standardConfig measure {
      integrateSeq(a, b, numberOfPoints = numberOfPoints)
    }
    val partime = standardConfig measure {
      // JUST DO IT
      integrateSeq(a, b, numberOfPoints = numberOfPoints)
    }
    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")

    println(s"speedup: ${seqtime.value/partime.value} ")
  }
}
