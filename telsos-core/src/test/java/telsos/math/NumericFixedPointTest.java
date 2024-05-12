// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Objects;

import org.junit.jupiter.api.Test;

import telsos.math.typeclasses.Num;
import telsos.math.typeclasses.Ord;
import telsos.math.typeclasses.instances.BigDecimalInstances;

class NumericFixedPointTest {

  class BigDecimalFixedPoint implements NumericFixedPoint<BigDecimal> {

    final MathContext mc;

    BigDecimalFixedPoint(MathContext mc) {
      this.mc = Objects.requireNonNull(mc);
    }

    @Override
    public Num<BigDecimal> num() {
      return BigDecimalInstances.numWith(mc);
    }

    @Override
    public Ord<BigDecimal> ord() {
      return BigDecimalInstances.ORD;
    }
  }

  @Test
  void testEvalUnaryOperatorOfTT() {
    final var mc = new MathContext(100, RoundingMode.HALF_EVEN);
    final var fixedPoint = new BigDecimalFixedPoint(mc);
    final HeronSqrt<BigDecimal> sqrt = () -> fixedPoint;
    final var value = sqrt.eval(fixedPoint.num().fromInt(2));

    assertThat(value.toString()).startsWith("1.4142135623730950488016896");
  }

}
