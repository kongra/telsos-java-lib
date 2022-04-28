// © 2021 Konrad Grzanek <kongra@gmail.com>
package telsos.jdbc;

import static telsos.Ch.chIn;
import static telsos.Ch.chNat;

import java.lang.System.Logger;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.EnumMap;
import java.util.Objects;
import java.util.function.Predicate;

import javax.sql.DataSource;

import io.vavr.CheckedConsumer;
import io.vavr.CheckedFunction0;
import io.vavr.Function1;
import io.vavr.control.Option;
import telsos.TelsosException;
import telsos.Utils;

public final class Tx {

  @FunctionalInterface
  public interface Expr<T> extends Function1<Connection, T> {}

  @FunctionalInterface
  public interface Stmt extends CheckedConsumer<Connection> {
    default void acceptNothrow(Connection ctx) {
      try {
        accept(ctx);
      } catch (Throwable t) {
        throw new TelsosException(t);
      }
    }
  }

  @FunctionalInterface
  public interface TxExpr<T> extends Function1<TxCtx, T> {}

  @FunctionalInterface
  public interface TxStmt extends CheckedConsumer<TxCtx> {
    default void acceptNothrow(TxCtx ctx) {
      try {
        accept(ctx);
      } catch (Throwable t) {
        throw new TelsosException(t);
      }
    }
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
      return expr.apply(conn);
    } catch (SQLException e) {
      throw new TelsosException(e);
    }
  }

  public static void withConn(DataSource ds, Stmt stmt) {
    withConn(ds, conn -> {
      stmt.acceptNothrow(conn);
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
      final var result = expr.apply(ctx);
      conn.commit();
      return result;
    } catch (SQLException e) {
      try {
        conn.rollback();
      } catch (SQLException e1) {
        LOG.log(Logger.Level.ERROR, e1);
      }
      throw Utils.sneakyThrow(e);
    } finally {
      try {
        conn.setAutoCommit(autoCommit);
      } catch (SQLException e) {
        LOG.log(Logger.Level.ERROR, e);
      }
    }
  }

  public static Option<SQLException> asSQLException(Throwable t) {
    while (true) {
      if (t == null)
        return Option.none();

      if (t instanceof SQLException sqlE)
        return Option.of(sqlE);

      t = t.getCause();
    }
  }

  @FunctionalInterface
  private interface IsRestarting extends Predicate<Throwable> {}

  private static final EnumMap<Dialect, IsRestarting> RESTARTS_FINDERS;
  static {
    RESTARTS_FINDERS = new EnumMap<>(Dialect.class);
  }

  public static <T> T restartingTx(TxCtx ctx, CheckedFunction0<T> supplier) {
    for (var i = 0;; i++) {
      try {
        return supplier.apply();
      } catch (Throwable e) {
        if (i == ctx.allowedRestartsCount)
          throw new TelsosException(e);

        var isRestarting = RESTARTS_FINDERS.get(ctx.dialect);
        if (null == isRestarting || !isRestarting.test(e))
          throw new TelsosException(e);

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
      stmt.acceptNothrow(ctx);
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
      stmt.acceptNothrow(ctx);
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
      stmt.acceptNothrow(ctx);
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
      stmt.acceptNothrow(ctx);
      return null;
    });
  }

  private static boolean isPostgres40001(SQLException e) {
    return "40001".equals(e.getSQLState());
  }

  // POSTGRESQL
  static {
    RESTARTS_FINDERS.put(Dialect.POSTGRES,
        t -> asSQLException(t).map(Tx::isPostgres40001).getOrElse(false));
  }

  private static final Logger LOG = System.getLogger(Tx.class.getName());

  private Tx() {}
}
