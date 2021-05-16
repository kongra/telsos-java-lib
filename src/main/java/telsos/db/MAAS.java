package telsos.db;

import static telsos.Delay.delay;

import javax.sql.DataSource;

import org.jooq.SQLDialect;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import telsos.Delay;

public class MAAS implements DBI {

  public static MAAS get() {
    return INSTANCE;
  }

  @Override
  public SQLDialect dialect() {
    return SQLDialect.POSTGRES;
  }

  @Override
  public DataSource dataSource() {
    return ds.get();
  }

  private final Delay<DataSource> ds = delay(() -> {
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

    var hikari = new HikariDataSource(config);

    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      hikari.close();
    }));

    return hikari;
  });

  private static final MAAS INSTANCE = new MAAS();

  private MAAS() {
  }
}
