// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class KleeneTest {

  @Test
  void testFromBoolean() {
    assertThat(Kleene.fromBoolean(true)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.fromBoolean(Boolean.TRUE)).isEqualTo(Kleene.TRUE);

    assertThat(Kleene.fromBoolean(false)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.fromBoolean(Boolean.FALSE)).isEqualTo(Kleene.FALSE);

    assertThat(Kleene.fromBoolean(null)).isEqualTo(Kleene.UNKNOWN);
  }

  @Test
  void testParseString() {
    assertThat(Kleene.parseString("true")).contains(Kleene.TRUE);
    assertThat(Kleene.parseString("TRUE")).contains(Kleene.TRUE);

    assertThat(Kleene.parseString("false")).contains(Kleene.FALSE);
    assertThat(Kleene.parseString("FALSE")).contains(Kleene.FALSE);

    assertThat(Kleene.parseString("unknown")).contains(Kleene.UNKNOWN);
    assertThat(Kleene.parseString("UNKNOWN")).contains(Kleene.UNKNOWN);

    assertThat(Kleene.parseString("$^@$^%---+++")).isEmpty();
  }

  @Test
  void testNegate() {
    assertThat(Kleene.TRUE.negate()).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.FALSE.negate()).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.UNKNOWN.negate()).isEqualTo(Kleene.UNKNOWN);
  }

  @Test
  void testAnd() {
    assertThat(Kleene.TRUE.and(() -> Kleene.TRUE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.TRUE.and(() -> Kleene.FALSE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.TRUE.and(() -> Kleene.UNKNOWN)).isEqualTo(Kleene.UNKNOWN);

    assertThat(Kleene.FALSE.and(() -> Kleene.TRUE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.FALSE.and(() -> Kleene.FALSE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.FALSE.and(() -> Kleene.UNKNOWN)).isEqualTo(Kleene.FALSE);

    assertThat(Kleene.UNKNOWN.and(() -> Kleene.TRUE)).isEqualTo(Kleene.UNKNOWN);
    assertThat(Kleene.UNKNOWN.and(() -> Kleene.FALSE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.UNKNOWN.and(() -> Kleene.UNKNOWN))
        .isEqualTo(Kleene.UNKNOWN);
  }

  @Test
  void testOr() {
    assertThat(Kleene.TRUE.or(() -> Kleene.TRUE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.TRUE.or(() -> Kleene.FALSE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.TRUE.or(() -> Kleene.UNKNOWN)).isEqualTo(Kleene.TRUE);

    assertThat(Kleene.FALSE.or(() -> Kleene.TRUE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.FALSE.or(() -> Kleene.FALSE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.FALSE.or(() -> Kleene.UNKNOWN)).isEqualTo(Kleene.UNKNOWN);

    assertThat(Kleene.UNKNOWN.or(() -> Kleene.TRUE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.UNKNOWN.or(() -> Kleene.FALSE)).isEqualTo(Kleene.UNKNOWN);
    assertThat(Kleene.UNKNOWN.or(() -> Kleene.UNKNOWN))
        .isEqualTo(Kleene.UNKNOWN);
  }

  @Test
  void testXor() {
    assertThat(Kleene.TRUE.xor(() -> Kleene.TRUE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.TRUE.xor(() -> Kleene.FALSE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.TRUE.xor(() -> Kleene.UNKNOWN)).isEqualTo(Kleene.UNKNOWN);

    assertThat(Kleene.FALSE.xor(() -> Kleene.TRUE)).isEqualTo(Kleene.TRUE);
    assertThat(Kleene.FALSE.xor(() -> Kleene.FALSE)).isEqualTo(Kleene.FALSE);
    assertThat(Kleene.FALSE.xor(() -> Kleene.UNKNOWN))
        .isEqualTo(Kleene.UNKNOWN);

    assertThat(Kleene.UNKNOWN.xor(() -> Kleene.TRUE)).isEqualTo(Kleene.UNKNOWN);
    assertThat(Kleene.UNKNOWN.xor(() -> Kleene.FALSE))
        .isEqualTo(Kleene.UNKNOWN);
    assertThat(Kleene.UNKNOWN.xor(() -> Kleene.UNKNOWN))
        .isEqualTo(Kleene.UNKNOWN);
  }

}
