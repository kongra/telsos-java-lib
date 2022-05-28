package telsos.profile;

import java.time.LocalDate;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import io.vavr.control.Option;

@ApplicationScoped
public class ProfileTools {

  @Transactional
  public Option<Profile> findById(long id) {
    return Option.of(Profile.findById(id));
  }

  @Transactional
  public Option<Profile> findByEmail(String email) {
    var query = Profile.find("email = :email", Map.of("email", email));
    return Option.ofOptional(query.singleResultOptional());
  }

  @Transactional
  public Option<Profile> addProfile(String email) {
    if (findByEmail(email).isDefined())
      return Option.none();

    var profile = new Profile(email, LocalDate.now());
    profile.persist();
    return Option.of(profile);
  }

  // @SuppressWarnings("static-method")
  // @Transactional
  int foo(int n) {
    return n + n;
  }

  public int runFoo(int n) {
    return foo(n);
  }

}
