package telsos.db;

import static telsos.Delay.delay;

import javax.sql.DataSource;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import telsos.Delay;

public class Pools {

  public static DataSource pgMAAS() {
    return pgMAAS.get();
  }

  private static final Delay<DataSource> pgMAAS = delay(() -> {
    var config = new HikariConfig();
    config.setJdbcUrl("jdbc:postgresql://localhost/MAAS");
    config.setUsername("jee");
    config.setPassword("jee");
    config.addDataSourceProperty("cachePrepStmts", "true");
    config.addDataSourceProperty("prepStmtCacheSize", "250");
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
    return new HikariDataSource(config);
  });

  private Pools() {
  }
}
