// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.string;

import java.util.Optional;
import java.util.function.Predicate;

import org.apache.commons.validator.routines.EmailValidator;

import telsos.Ch;
import telsos.ChError;

public final class Email extends ValidatedString {

  public static Email of(String s) {
    return Str.validOf(s, ch, Email::new);
  }

  public static Optional<Email> optionally(String s) {
    return Str.validOf(s, pred, Email::new);
  }

  public static Email ofStripped(String s) {
    return Str.validOfStripped(s, ch, Email::new);
  }

  public static String chEmail(String s) {
    if (pred.test(s))
      return s;
    throw new ChError("Invalid email " + s);
  }

  public static final EmailValidator validator = EmailValidator
      .getInstance();

  public static final Ch<String> ch = Email::chEmail;

  public static final Predicate<String> pred = validator::isValid;

  private Email(String value) {
    super(value);
  }

}
