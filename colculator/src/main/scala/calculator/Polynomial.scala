package calculator

import scala.math._

object Polynomial extends PolynomialInterface:
  def computeDelta(
                    a: Signal[Double],
                    b: Signal[Double],
                    c: Signal[Double]
                  ): Signal[Double] =
    Signal { pow(b(), 2)  - ( 4 * a() * c() ) }

  def computeSolutions(
                        a: Signal[Double],
                        b: Signal[Double],
                        c: Signal[Double],
                        delta: Signal[Double]
                      ): Signal[Set[Double]] =
    Signal {
      delta() match
        case d if d < 0 => Set()
        case _ =>
          Set(
            ( ( b() * -1 ) + sqrt(delta()) ) / ( a() * 2 ),
            ( ( b() * -1 ) - sqrt(delta()) ) / ( a() * 2 )
          )
    }
