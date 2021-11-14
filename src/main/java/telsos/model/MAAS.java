package telsos.model;

import org.postgresql.xa.PGXADataSource;

import jakarta.ejb.EJB;
import jakarta.ejb.Singleton;

@Singleton
@EJB(name = "java:comp/env/jdbc/MAAS")
public class MAAS extends PGXADataSource {

  public MAAS() {
    super();

    setUrl("jdbc:postgresql://localhost/MAAS");
    setUser("jee");
    setPassword("jee");
  }

}
