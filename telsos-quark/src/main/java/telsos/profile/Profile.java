package telsos.profile;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Transient;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

@Entity
public class Profile extends PanacheEntity {

  @NotNull
  public UUID uid;

  @Email
  @NotNull
  @Column(nullable = false, unique = true)
  public String email;

  @NotNull
  @Transient
  public LocalDate registrationDate;

  public Profile() {}

  public Profile(@Email @NotNull String email,
      @NotNull LocalDate registrationDate) {
    this.email = email;
    this.registrationDate = registrationDate;
    this.uid = UUID.randomUUID();
  }

  public long daysSinceRegistration() {
    return ChronoUnit.DAYS.between(registrationDate, LocalDateTime.now());
  }

  @Override
  public final int hashCode() {
    final var prime = 31;
    var result = 1;
    result = prime * result + ((uid == null) ? 0 : uid.hashCode());
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof Profile)) {
      return false;
    }
    Profile other = (Profile) obj;
    if (uid == null) {
      if (other.uid != null) {
        return false;
      }
    } else if (!uid.equals(other.uid)) {
      return false;
    }
    return true;
  }

}
