package jesty.lib.string;

import java.util.Optional;

import org.apache.commons.validator.routines.EmailValidator;

import jesty.lib.newtype.Newtype;

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
