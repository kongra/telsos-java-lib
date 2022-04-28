// © 2021 Konrad Grzanek <kongra@gmail.com>
package telsos.jdbc;

import static telsos.Ch.chIn;
import static telsos.Ch.chNat;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.EnumMap;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

import javax.sql.DataSource;

import telsos.TelsosException;
import telsos.Utils;

public final class Tx {

  @FunctionalInterface
  public interface Expr<T> {

    T eval(Connection conn);

  }

  @FunctionalInterface
  public interface Stmt {

    void exec(Connection conn);

  }

  @FunctionalInterface
  public interface TxExpr<T> {

    T eval(TxCtx ctx);

  }

  @FunctionalInterface
  public interface TxStmt {

    void exec(TxCtx ctx);

  }

  public static final class TxCtx {

    public final Connection conn;

    public final int isolationLevel;

    public final int allowedRestartsCount;

    public final Dialect dialect;

    private int restartsCount;

    private final Thread thread;

    private TxCtx(Connection conn, int isolationLevel, int allowedRestartsCount,
        Dialect dialect) {
      this.conn = Objects.requireNonNull(conn);
      this.isolationLevel = chIsolationLevel(isolationLevel);
      this.allowedRestartsCount = chNat(allowedRestartsCount);
      this.dialect = Objects.requireNonNull(dialect);
      thread = Thread.currentThread();
    }

    private synchronized void markRestart() {
      restartsCount++;
    }

    public synchronized int restartsCount() {
      return restartsCount;
    }

    public void assertSingleThreadUse() throws IllegalAccessError {
      if (thread != Thread.currentThread())
        throw new IllegalAccessError(
            "You can't call TxCtx from another thread!");
    }
  }

  public static <T> T withConn(DataSource ds, Expr<T> expr) {
    try (var conn = ds.getConnection()) {
      return expr.eval(conn);
    } catch (SQLException e) {
      throw new TelsosException(e);
    }
  }

  public static void withConn(DataSource ds, Stmt stmt) {
    withConn(ds, conn -> {
      stmt.exec(conn);
      return null;
    });
  }

  private static <T> T inTransaction(TxCtx ctx, TxExpr<T> expr) {
    final var conn = ctx.conn;
    final boolean autoCommit;
    try {
      autoCommit = conn.getAutoCommit();
    } catch (SQLException e) {
      throw Utils.sneakyThrow(e);
    }
    try {
      conn.setAutoCommit(false);
      conn.setTransactionIsolation(ctx.isolationLevel);
      final var result = expr.eval(ctx);
      conn.commit();
      return result;
    } catch (Exception e) {
      try {
        conn.rollback();
      } catch (SQLException e1) {
        // In the future: maybe log
      }
      throw Utils.sneakyThrow(e);
    } finally {
      try {
        conn.setAutoCommit(autoCommit);
      } catch (SQLException e) {
        // In the future: maybe log
      }
    }
  }

  public static Optional<SQLException> asSQLException(Throwable t) {
    while (true) {
      if (t == null)
        return Optional.empty();

      if (t instanceof SQLException sqlE)
        return Optional.of(sqlE);

      t = t.getCause();
    }
  }

  @FunctionalInterface
  private interface IsRestarting {

    boolean eval(Throwable e);

  }

  private static final EnumMap<Dialect, IsRestarting> RESTARTS_FINDERS;
  static {
    RESTARTS_FINDERS = new EnumMap<>(Dialect.class);
  }

  public static <T> T restartingTx(TxCtx ctx, Supplier<T> supplier) {
    for (var i = 0;; i++) {
      try {
        return supplier.get();
      } catch (Exception e) {
        if (i == ctx.allowedRestartsCount)
          throw e;

        var isRestarting = RESTARTS_FINDERS.get(ctx.dialect);
        if (null == isRestarting || !isRestarting.eval(e))
          throw e;

        // We continue the restarting iterations
        ctx.markRestart();
      }
    }
  }

  // FOR VARIOUS ISOLATION LEVELS
  public static int chIsolationLevel(int isolationLevel) {
    return chIn(isolationLevel, ISOLATION_LEVELS);
  }

  private static final int[] ISOLATION_LEVELS = { Connection.TRANSACTION_NONE,
      Connection.TRANSACTION_READ_UNCOMMITTED,
      Connection.TRANSACTION_READ_COMMITTED,
      Connection.TRANSACTION_REPEATABLE_READ,
      Connection.TRANSACTION_SERIALIZABLE };

  public static <T> T inSerializable(Dialect dialect, Connection conn,
      int allowedRestartsCount, TxExpr<T> expr) {
    var ctx = new TxCtx(conn, Connection.TRANSACTION_SERIALIZABLE,
        allowedRestartsCount, dialect);
    return restartingTx(ctx, () -> inTransaction(ctx, expr));
  }

  public static void inSerializable(Dialect dialect, Connection conn,
      int allowedRestartsCount, TxStmt stmt) {
    inSerializable(dialect, conn, allowedRestartsCount, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  public static <T> T inSerializable(Dialect dialect, DataSource ds,
      int allowedRestartsCount, TxExpr<T> expr) {
    Expr<T> expr1 = conn -> inSerializable(dialect, conn, allowedRestartsCount,
        expr);
    return withConn(ds, expr1);
  }

  public static void inSerializable(Dialect dialect, DataSource ds,
      int allowedRestartsCount, TxStmt stmt) {
    inSerializable(dialect, ds, allowedRestartsCount, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  public static <T> T inReadCommitted(Dialect dialect, Connection conn,
      TxExpr<T> expr) {
    var allowedRestartsCount = 0;
    var ctx = new TxCtx(conn, Connection.TRANSACTION_READ_COMMITTED,
        allowedRestartsCount, dialect);
    return restartingTx(ctx, () -> inTransaction(ctx, expr));
  }

  public static void inReadCommitted(Dialect dialect, Connection conn,
      TxStmt stmt) {
    inReadCommitted(dialect, conn, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  public static <T> T inReadCommitted(Dialect dialect, DataSource ds,
      TxExpr<T> expr) {
    Expr<T> expr1 = conn -> inReadCommitted(dialect, conn, expr);
    return withConn(ds, expr1);
  }

  public static void inReadCommitted(Dialect dialect, DataSource ds,
      TxStmt stmt) {
    inReadCommitted(dialect, ds, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  // POSTGRESQL
  static {
    RESTARTS_FINDERS.put(Dialect.POSTGRES, t -> asSQLException(t)
        .map(e -> "40001".equals(e.getSQLState())).orElse(false));
  }

  private Tx() {}
}
