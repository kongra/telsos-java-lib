package telsos.string;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

import telsos.ChError;

@SuppressWarnings("static-method")
class NonBlankTest {

  @Test
  void testOf() {
    assertThatThrownBy(() -> {
      NonBlank.of(StrTest.NULL);
    }).isInstanceOf(ChError.class);

    assertThatThrownBy(() -> {
      NonBlank.of(StrTest.EMPTY);
    }).isInstanceOf(ChError.class);

    assertThatThrownBy(() -> {
      NonBlank.of(StrTest.BLANK);
    }).isInstanceOf(ChError.class);

    assertThat(NonBlank.of(StrTest.NO_WHITESPACE).value())
        .isEqualTo(StrTest.NO_WHITESPACE);

    assertThat(NonBlank.of(StrTest.HAVING_WHITESPACE).value())
        .isEqualTo(StrTest.HAVING_WHITESPACE);

    assertThat(NonBlank.of(Str.strip(StrTest.HAVING_WHITESPACE)).value())
        .isEqualTo(StrTest.NO_WHITESPACE);
  }

  @Test
  void testOptionallyOf() {
    assertThat(NonBlank.optionallyOf(StrTest.NULL)).isEmpty();
    assertThat(NonBlank.optionallyOf(StrTest.EMPTY)).isEmpty();
    assertThat(NonBlank.optionallyOf(StrTest.BLANK)).isEmpty();

    assertThat(NonBlank.optionallyOf(StrTest.NO_WHITESPACE))
        .isNotEmpty()
        .hasValueSatisfying(s -> StrTest.NO_WHITESPACE.equals(s.value()));

    assertThat(NonBlank.optionallyOf(StrTest.HAVING_WHITESPACE))
        .isNotEmpty()
        .hasValueSatisfying(s -> StrTest.HAVING_WHITESPACE.equals(s.value()));
  }

  @Test
  void testIsBlank() {
    assertThat(NonBlank.isBlank(StrTest.NULL)).isTrue();
    assertThat(NonBlank.isBlank(StrTest.EMPTY)).isTrue();
    assertThat(NonBlank.isBlank(StrTest.BLANK)).isTrue();

    assertThat(NonBlank.isBlank(StrTest.NO_WHITESPACE)).isFalse();
    assertThat(NonBlank.isBlank(StrTest.HAVING_WHITESPACE)).isFalse();
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
