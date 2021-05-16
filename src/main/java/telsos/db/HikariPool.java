package telsos.db;

import static telsos.Delay.delay;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import telsos.Delay;

public abstract class HikariPool {

  public abstract HikariConfig hikariConfig();

  public HikariDataSource dataSource() {
    return dataSource.deref();
  }

  private final Delay<HikariDataSource> dataSource = delay(() -> {
    var hikari = new HikariDataSource(hikariConfig());
    Runtime.getRuntime().addShutdownHook(new Thread(hikari::close));
    return hikari;
  });

}
