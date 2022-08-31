// © 2020 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class StopwatchTest {

  Stopwatch timer;

  @BeforeEach
  void setUp() {
    timer = Stopwatch.start();
  }

  @Test
  void testElapsedNanosecs() {
    final var t1 = timer.elapsedNanosecs();
    final var t2 = timer.elapsedNanosecs();

    assertThat(t1).isNotNegative();
    assertThat(t2)
        .isNotNegative()
        .isGreaterThan(t1);
  }

  @Test
  void testElapsedMsecs() {
    final var msecs = timer.elapsedMsecs();
    final var nanos = timer.elapsedNanosecs();

    assertThat(msecs).isNotNegative();
    assertThat(nanos).isNotNegative();
    assertThat((double) nanos).isGreaterThan(msecs);

    System.out.println("msecs = " + msecs);
    System.out.println("nanos = " + nanos);
    
    final var t1 = timer.elapsedMsecs();
    final var t2 = timer.elapsedMsecs();

    assertThat(t1).isNotNegative();
    assertThat(t2)
        .isNotNegative()
        .isGreaterThan(t1);
  }

  @Test
  void testElapstr() {
    final var s1 = timer.elapstr();
    final var s2 = timer.toString();

    assertThat(s1)
        .isNotNull()
        .isNotEmpty()
        .isNotBlank();

    assertThat(s2)
        .isNotNull()
        .isNotEmpty()
        .isNotBlank();

    assertThat(s1).isNotEqualTo(s2);

    System.out.println("s1 = " + s1);
    System.out.println("s2 = " + s2);
  }

}
