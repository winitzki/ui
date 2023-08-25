//#!/usr/bin/env -S scala-cli shebang -S 2.13.11

//> using scala 2.13.11
//> using dep com.gitlab.klamonte:jexer:1.6.0
//> using dep com.googlecode.lanterna:lanterna:3.1.1

import java.nio.file.{Files, Paths}

object MotionPlanning extends App {

  final case class Pos(x: Double, v: Double, a: Double)

  def stepCurve(pos0: Pos, j: Double, t: Double): Pos = {
    val Pos(x0, v0, a0) = pos0
    val x = j * math.pow(t, 3) / 6 + t * t / 2 * a0 + t * v0 + x0
    val v = j * t * t / 2 + a0 * t + v0
    val a = a0 + j * t
    Pos(x, v, a)
  }

  def lossFunction(pos0: Pos, pos1: Pos, T: Double, p: Double, q: Double): Double =
    math.pow(pos0.x - pos1.x, 2) + math.pow(pos0.v - pos1.v, 2) * T * T * p + math.pow(pos0.a - pos1.a, 2) * T * T * T * T * q

  def step(pos0: Pos, posT: Pos, T: Double, t1: Double, maxJ: Double, p: Double, q: Double): (Pos, Double, Double) = {
    val Pos(x0, v0, a0) = pos0
    val Pos(xT, vT, aT) = posT
    val eps = t1 / T
    // Determine the optimal value of J.
    /*
    J0 = -3*(T^2*a0*eps^4 + 6*T^2*a0*eps^2*p - 6*T*eps*p*vT + 2*eps^2*x0 - 2*eps^2*xT + 12*(T^2*a0 - T^2*aT)*q + 2*(T*eps^3 + 3*T*eps*p)*v0)/(T^3*eps^5 + 9*T^3*eps^3*p + 36*T^3*eps*q)
     
    val jOpt: Double = -3 * (math.pow(T, 2) * a0 * math.pow(eps, 4) + 6 * T * T * a0 * eps * eps * p - 6 * T * eps * p * vT + 2 * eps * eps * x0 - 2 * eps * eps * xT + 12 * (T * T * a0 - T * T * aT) * q + 2 * (T 
* eps * eps * eps + 3 * T * eps * p) * v0) / (T * T * T) / (math.pow(eps, 5) + 9 * eps * eps * eps * p + 36 * eps * q)

new formula:
-3/T^3*(12*p*T^2*a0-12*p*T^2*aT-6*q*T*v0*epsilon+12*q*T*v0-6*q*T^2*a0*epsilon+12*q*T^2*a0+6*q*T*vT*epsilon-12*q*T*vT+2*x0*epsilon^2-6*x0*epsilon+6*x0+2*v0*epsilon^2*T-6*v0*epsilon*T+6*v0*T+a0*epsilon^2*T^2-3*a0*epsilon*T^2+3*a0*T^2-2*xT*epsilon^2+6*xT*epsilon-6*xT)/epsilon/(36*p+9*q*epsilon^2-36*q*epsilon+36*q+epsilon^4-6*epsilon^3+15*epsilon^2-18*epsilon+9)
*/
      val epsilon=eps
	val jOpt: Double = -3.0 / math.pow(T, 3)*  
(12*p*T*T * 
a0-12*p*T*T*aT-6*q*T*v0*epsilon+12*q*T*v0-6*q*T*T*a0*epsilon+12*q*T*T*a0+6*q*T*vT*epsilon-12*q*T*vT+2*x0*epsilon*eps-6*x0*epsilon+6*x0+2*v0*epsilon*eps*T-6*v0*epsilon*T+ 
6*v0*T+a0*epsilon*eps*T*T-3*a0*epsilon*T*T+3*a0*T*T-2*xT*epsilon*eps+6*xT*epsilon-6*xT)/epsilon/(36*p+9*q*epsilon*eps-36*q*epsilon+36*q+math.pow(epsilon,4)-6*math.pow(epsilon,3)+15*epsilon*eps-18*epsilon+9)

    val j = if (jOpt > maxJ) maxJ else if (jOpt < -maxJ) -maxJ else jOpt
    val pos1 = stepCurve(pos0, j, t1)
    (pos1, j, lossFunction(pos1, posT, T, p, q))
  }

  // Plot motion between t=0 and t=T with given parameters.
  def plot(pos0: Pos, posT: Pos, T: Double, maxJ: Double, p: Double, q: Double, smallTimesteps: Int, largeTimesteps: Int, totalTime: Double, noise: Double = 0.0): Seq[(Double, Pos, Double, Double)] = {
    val deltaT = T / largeTimesteps.toDouble
    val largeStepData = (1 to (largeTimesteps * totalTime / T).toInt).map(i => i.toDouble * deltaT).scanLeft((0.0, pos0, 0.0, lossFunction(pos0, posT, T, p, q))) { case ((t1, pos1, j1, l1), tNext) =>
      val (posN, jNext, lossNext) = step(pos1, posT, T, deltaT, maxJ, p, q)
      val posNext = Pos(posN.x + noise * deltaT * deltaT / 2, posN.v + noise * deltaT, posN.a + noise)
      (tNext, posNext, jNext, lossNext)
    }
    largeStepData.zip(largeStepData.drop(1))
      .flatMap { case ((t1, pos1, j1, l1), (t2, pos2, j2, l2)) =>
        // Output `smallTimesteps` data points leading up to the given values (t2, pos2, j2, l2).
        (0 until smallTimesteps).map(i => t1 + i.toDouble * (t2 - t1) / smallTimesteps).map { t =>
          (t, stepCurve(pos1, j2, t), j2, l1)
        }
      }
  }

  val dataString = plot(
    pos0 = Pos(0, 0, 0),
    posT = Pos(1, 0, 0),
    T = 1,
    maxJ = 10,
    p = 0.01,
    q = 1,
    smallTimesteps = 20,
    largeTimesteps = 20,
    totalTime = 10,
    noise = 0,
  ).map { case (t, pos, j, l) => s"$t ${pos.x} ${pos.v} ${pos.a} $j ${l/100.0}" }
    .mkString("\n")

  val dataFile = "/tmp/data.txt"
  Files.write(Paths.get(dataFile), dataString.getBytes)
  val gnuplotFile = "/tmp/gnuplot.txt"
  val pngFile = "/tmp/gnuplot.png"
  Files.write(Paths.get(gnuplotFile), // set yrange[-0.01: 0.01]
    s"""set output '$pngFile'
       |set datafile separator ' '
       |set terminal png size 3072,1536
       |set style data lines
       |set title 'Motion with dynamic programming'
       |plot '$dataFile' using 1:2 title 'x position', '' using 1:3 title 'velocity', '' using 1:4 title 'acceleration', '' using 1:5 title 'jerk', '' using 1:6 title 'loss function'
       |""".stripMargin.getBytes)
  import scala.sys.process._
  s"gnuplot '$gnuplotFile'".!
  s"open '$pngFile' ".!
}
