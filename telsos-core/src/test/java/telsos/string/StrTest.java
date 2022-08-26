package telsos.string;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class StrTest {

  static final String NULL = null;

  static final String EMPTY = "";

  static final String BLANK = " \t\n\r";

  static final String NO_WHITESPACE = "abcd";

  static final String HAVING_WHITESPACE = BLANK + NO_WHITESPACE + BLANK;

  @Test
  void testStrip() {
    assertThat(NULL).isNull();
    assertThat(EMPTY).isEmpty();
    assertThat(BLANK).isBlank();

    assertThat(NO_WHITESPACE)
        .isNotNull()
        .isNotBlank()
        .isNotEmpty();

    assertThat(HAVING_WHITESPACE)
        .isNotNull()
        .isNotBlank()
        .isNotEmpty();

    assertThat(Str.strip(NULL))
        .isNull();

    assertThat(Str.strip(EMPTY))
        .isNotNull()
        .isEmpty();

    assertThat(Str.strip(BLANK))
        .isNotNull()
        .isEmpty();

    assertThat(Str.strip(NO_WHITESPACE))
        .isNotNull()
        .isNotBlank()
        .isNotEmpty()
        .isEqualTo(NO_WHITESPACE);

    assertThat(Str.strip(HAVING_WHITESPACE))
        .isNotNull()
        .isNotBlank()
        .isNotEmpty()
        .isEqualTo(NO_WHITESPACE)
        .isNotEqualTo(HAVING_WHITESPACE);
  }

}
