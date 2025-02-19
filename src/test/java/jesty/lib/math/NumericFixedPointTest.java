package jesty.lib.math;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.junit.jupiter.api.Test;

import telsos.java.lib.math.HeronSqrt;
import telsos.java.lib.math.NumericFixedPoint;
import telsos.java.lib.typeclass.Enum;
import telsos.java.lib.typeclass.Num;
import telsos.java.lib.typeclass.Ord;
import telsos.java.lib.typeclass.instances.BigDecimalInstances;

class NumericFixedPointTest {

  static class BigDecimalFixedPoint implements NumericFixedPoint<BigDecimal> {

    final Num<BigDecimal> num;

    BigDecimalFixedPoint(MathContext mc) {
      num = BigDecimalInstances.numWith(mc);
    }

    @Override
    public Num<BigDecimal> num() {
      return num;
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
