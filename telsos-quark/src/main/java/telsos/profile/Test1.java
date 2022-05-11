package telsos.profile;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;

@Entity
@Table(name = "test1")
public class Test1 extends PanacheEntityBase {

  @Id
  public long id;

  @Column(name = "first_name")
  public String firstName;

  public Test1() {}

  public Test1(long id, String firstName) {
    this.id = id;
    this.firstName = firstName;
  }

}
