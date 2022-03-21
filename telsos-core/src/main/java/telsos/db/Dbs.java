package telsos.db;

import javax.sql.DataSource;

import com.zaxxer.hikari.HikariConfig;

import telsos.db.tools.Db;
import telsos.db.tools.HikariPool;
import telsos.db.tools.SQLDialect;

public class Dbs {

  public static final Db maas = new Db() {

    private final HikariPool pool = new HikariPool() {
      @Override
      public HikariConfig hikariConfig() {
        var config = new HikariConfig();
        config.setJdbcUrl("jdbc:postgresql://localhost/MAAS");
        config.setUsername("jee1");
        config.setPassword("jee12345");
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", "250");
        config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

        config.setMaximumPoolSize(100);
        config.setAutoCommit(true);
        config.setValidationTimeout(5000);

        return config;
      }
    };

    @Override
    public SQLDialect dialect() {
      return SQLDialect.POSTGRES;
    }

    @Override
    public DataSource dataSource() {
      return pool.dataSource();
    }
  };

  private Dbs() {
  }
}
