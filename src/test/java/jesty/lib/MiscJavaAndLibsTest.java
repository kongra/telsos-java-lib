package jesty.lib;

import static org.assertj.core.api.Assertions.assertThat;

import java.security.SecureRandom;

import org.junit.jupiter.api.Test;

class MiscJavaAndLibsTest {

  @Test
  void testNaN2IntConversion() {
    final var x = Math.sqrt(-5);
    assertThat(x).isNaN();

    final var n = (int) x;
    assertThat(n).isZero();
  }

  @Test
  void testRandomIntsGeneration() {
    final var random = new SecureRandom();
    final var bound = 100;
    final var n = random.nextInt(bound);
    assertThat(n).isBetween(0, bound - 1);
  }

}
