// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class NewtonsSqrtTest {

  NewtonsMethod<BigDecimal> newtonsMethod;

  NewtonsSqrt<BigDecimal> sqrt;

  @BeforeEach
  void setUp() {
    final var mc = new MathContext(100, RoundingMode.HALF_EVEN);
    final var fixedPoint = new NumericFixedPointTest.BigDecimalFixedPoint(mc);
    newtonsMethod = () -> fixedPoint;
    sqrt          = () -> newtonsMethod;
  }

  @Test
  void test() {
    final var enm = newtonsMethod.fixedPoint().enm();
    final var value = sqrt.eval(enm.fromInt(2).orElseThrow());
    assertThat(value.toString()).startsWith("1.41421356237309504");
  }

}
