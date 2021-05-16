package telsos.db;

import org.jooq.SQLDialect;

import com.zaxxer.hikari.HikariConfig;

public class MAAS extends HikariPool implements DBI {

  public static MAAS get() {
    return INSTANCE;
  }

  @Override
  public SQLDialect dialect() {
    return SQLDialect.POSTGRES;
  }

  @Override
  public HikariConfig hikariConfig() {
    var config = new HikariConfig();
    config.setJdbcUrl("jdbc:postgresql://localhost/MAAS");
    config.setUsername("jee");
    config.setPassword("jee");
    config.addDataSourceProperty("cachePrepStmts", "true");
    config.addDataSourceProperty("prepStmtCacheSize", "250");
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

    config.setMaximumPoolSize(100);
    config.setAutoCommit(true);
    config.setValidationTimeout(5000);

    return config;
  }

  private static final MAAS INSTANCE = new MAAS();

  private MAAS() {
  }
}
