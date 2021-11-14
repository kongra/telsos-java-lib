package telsos.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;

@Entity
public class Person {

  @Id
  private long id;

  private String firstName;

  private String lastName;

  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

}
