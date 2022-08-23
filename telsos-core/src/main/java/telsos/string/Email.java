// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.string;

import java.util.Optional;

import telsos.Ch;

public final class Email extends ValidatedString {

  public static Email of(String s) {
    return Str.validOf(s, Ch.email, Email::new);
  }

  public static Optional<Email> optionally(String s) {
    return Str.validOf(s, Ch.emailValidator::isValid, Email::new);
  }

  public static Email ofStripped(String s) {
    return Str.validOfStripped(s, Ch.email, Email::new);
  }

  private Email(String value) {
    super(value);
  }

}
