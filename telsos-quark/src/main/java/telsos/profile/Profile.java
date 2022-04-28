package telsos.profile;

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

}
