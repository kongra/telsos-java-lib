// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.junit.jupiter.api.Test;

import telsos.typeclasses.Enum;
import telsos.typeclasses.Num;
import telsos.typeclasses.Ord;
import telsos.typeclasses.instances.BigDecimalInstances;

class NumericFixedPointTest {

  class BigDecimalFixedPoint implements NumericFixedPoint<BigDecimal> {

    final Num<BigDecimal> NUM;

    BigDecimalFixedPoint(MathContext mc) {
      NUM = BigDecimalInstances.numWith(mc);
    }

    @Override
    public Num<BigDecimal> num() {
      return NUM;
    }

    @Override
    public Ord<BigDecimal> ord() {
      return BigDecimalInstances.ORD;
    }

    @Override
    public Enum<BigDecimal> enm() {
      return BigDecimalInstances.ENUM;
    }
  }

  @Test
  void testEvalUnaryOperatorOfTT() {
    final var mc = new MathContext(100, RoundingMode.HALF_EVEN);
    final var fixedPoint = new BigDecimalFixedPoint(mc);
    final var enm = fixedPoint.enm();
    final HeronSqrt<BigDecimal> sqrt = () -> fixedPoint;
    final var value = sqrt.eval(enm.fromInt(2).orElseThrow());

    assertThat(value.toString()).startsWith("1.4142135623730950488016896");
  }

}
