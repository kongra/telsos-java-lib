// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.string;

import java.util.Optional;
import java.util.function.Predicate;

import org.apache.commons.validator.routines.EmailValidator;

import telsos.Ch;
import telsos.newtype.Newtype;

public final class Email extends Newtype<String> {

  public static Email of(String s) {
    return Ch.checked(s, ch, Email::new);
  }

  public static Optional<Email> optionallyOf(String s) {
    return Ch.checkedOptionally(s, pred, Email::new);
  }

  public static final EmailValidator validator = EmailValidator
      .getInstance();

  public static final Predicate<String> pred = NonBlank.pred
      .and(validator::isValid);

  public static final Ch<String> ch = Ch.of(pred);

  @Override
  public String toString() {
    return value();
  }

  private Email(String value) {
    super(value);
  }

}
