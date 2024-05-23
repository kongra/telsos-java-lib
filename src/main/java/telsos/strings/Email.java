// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.strings;

import java.util.Optional;

import org.apache.commons.validator.routines.EmailValidator;

import telsos.newtypes.Newtype;

public final class Email extends Newtype<String> {

  public static Optional<Email> of(String s) {
    return of(s, Email::isValidEmail, Email::new);
  }

  public static boolean isValidEmail(String s) {
    return EmailValidator.getInstance().isValid(s);
  }

  private Email(String value) {
    super(value);
  }

}
