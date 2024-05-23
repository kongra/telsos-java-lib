// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.strings;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NonBlankTest {

  @Test
  void testOf() {
    assertThat(NonBlank.of(StrTest.NO_WHITESPACE).get().value())
        .isEqualTo(StrTest.NO_WHITESPACE);

    assertThat(NonBlank.of(StrTest.HAVING_WHITESPACE).get().value())
        .isEqualTo(StrTest.HAVING_WHITESPACE);

    assertThat(NonBlank.of(Str.strip(StrTest.HAVING_WHITESPACE)).get().value())
        .isEqualTo(StrTest.NO_WHITESPACE);
  }

  @Test
  void testOptOf() {
    assertThat(NonBlank.of(StrTest.NULL)).isEmpty();
    assertThat(NonBlank.of(StrTest.EMPTY)).isEmpty();
    assertThat(NonBlank.of(StrTest.BLANK)).isEmpty();

    assertThat(NonBlank.of(StrTest.NO_WHITESPACE))
        .isNotEmpty()
        .hasValueSatisfying(s -> StrTest.NO_WHITESPACE.equals(s.value()));

    assertThat(NonBlank.of(StrTest.HAVING_WHITESPACE))
        .isNotEmpty()
        .hasValueSatisfying(s -> StrTest.HAVING_WHITESPACE.equals(s.value()));
  }

  @Test
  void testEquality() {
    final var o1 = NonBlank.of(StrTest.NO_WHITESPACE);
    final var o11 = NonBlank.of(StrTest.NO_WHITESPACE);
    final var o2 = NonBlank.of(StrTest.HAVING_WHITESPACE);

    assertThat(o1)
        .isNotEqualTo(null)
        .isNotEqualTo(o2)
        .isEqualTo(o1)
        .isEqualTo(o11);
  }

  @Test
  void testHashCode() {
    final var h1 = NonBlank.of(StrTest.NO_WHITESPACE).hashCode();
    final var h11 = NonBlank.of(StrTest.NO_WHITESPACE).hashCode();
    final var h2 = NonBlank.of(StrTest.HAVING_WHITESPACE).hashCode();
    final var h22 = NonBlank.of(StrTest.HAVING_WHITESPACE).hashCode();

    assertThat(h1).isEqualTo(h11);
    assertThat(h2).isEqualTo(h22);
  }

}
