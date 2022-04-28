package telsos.quark;

import java.util.Objects;

import javax.sql.DataSource;

import telsos.jdbc.Dialect;
import telsos.jdbc.Ds;

public final class PostgresDataSource implements Ds {

  public static PostgresDataSource of(DataSource dataSource) {
    return new PostgresDataSource(dataSource);
  }

  @Override
  public Dialect dialect() {
    return Dialect.POSTGRES;
  }

  @Override
  public DataSource dataSource() {
    return dataSource;
  }

  private final DataSource dataSource;

  private PostgresDataSource(DataSource dataSource) {
    Objects.requireNonNull(dataSource);
    this.dataSource = dataSource;
  }

}
