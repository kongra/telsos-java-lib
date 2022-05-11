package telsos.profile;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

@Entity
public class Profile extends PanacheEntity {

  @Email
  @NotNull
  @Column(nullable = false, unique = true)
  public String email;

  @NotNull
  public LocalDate registrationDate;

  public Profile() {}

  public Profile(@Email @NotNull String email,
      @NotNull LocalDate registrationDate) {
    this.email = email;
    this.registrationDate = registrationDate;
  }

  public long daysSinceRegistration() {
    return ChronoUnit.DAYS.between(registrationDate, LocalDateTime.now());
  }

}
