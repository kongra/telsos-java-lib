package telsos.db;

import static telsos.Ch.chIn;
import static telsos.Ch.chNat;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.EnumMap;
import java.util.Objects;
import java.util.function.Supplier;

import javax.sql.DataSource;

import org.jooq.DSLContext;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;

public class DBI {

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

  public static class TxCtx {

    public final Connection conn;

    public final int isolationLevel;

    public final int allowedRestartsCount;

    public final SQLDialect dialect;

    private DSLContext dslContext;

    private int restartsCount;

    private TxCtx(Connection conn, int isolationLevel, int allowedRestartsCount,
        SQLDialect dialect) {
      this.conn = Objects.requireNonNull(conn);
      this.isolationLevel = chIsolationLevel(isolationLevel);
      this.allowedRestartsCount = chNat(allowedRestartsCount);
      this.dialect = Objects.requireNonNull(dialect);
    }

    public synchronized DSLContext create() {
      if (dslContext == null) {
        dslContext = DSL.using(conn, dialect);
      }
      return dslContext;
    }

    public synchronized void markRestart() {
      restartsCount++;
    }

    public synchronized int restartsCount() {
      return restartsCount;
    }
  }

  public static <T> T withConn(DataSource ds, Expr<T> expr) {
    try (var conn = ds.getConnection()) {
      return expr.eval(conn);
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }

  public static void withConn(DataSource ds, Stmt stmt) {
    withConn(ds, conn -> {
      stmt.exec(conn);
      return null;
    });
  }

  public static <T> T inTransaction(TxCtx ctx, TxExpr<T> expr) {
    try {
      final var conn = ctx.conn;
      final var autoCommit = conn.getAutoCommit();
      try {
        conn.setAutoCommit(false);
        conn.setTransactionIsolation(ctx.isolationLevel);
        final var result = expr.eval(ctx);
        conn.commit();
        return result;
      } catch (Throwable e) {
        conn.rollback();
        throw e;
      } finally {
        conn.setAutoCommit(autoCommit);
      }
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }

  public static void inTransaction(TxCtx ctx, TxStmt stmt) {
    inTransaction(ctx, ctx1 -> {
      stmt.exec(ctx1);
      return null;
    });
  }

  public static SQLException asSQLException(Throwable t) {
    while (true) {
      if (t == null)
        return null;
      if (t instanceof SQLException)
        return (SQLException) t;

      t = t.getCause();
    }
  }

  @FunctionalInterface
  private interface IsRestarting {

    boolean eval(Throwable e);

  }

  private static final EnumMap<SQLDialect, IsRestarting> RESTARTS_FINDERS;
  static {
    RESTARTS_FINDERS = new EnumMap<>(SQLDialect.class);
  }

  public static <T> T restartingTx(TxCtx ctx, Supplier<T> supplier) {
    for (var i = 0;; i++) {
      try {
        return supplier.get();
      } catch (Throwable t) {
        if (i == ctx.allowedRestartsCount)
          throw t;

        var isRestarting = RESTARTS_FINDERS.get(ctx.dialect);
        if (null == isRestarting)
          // No way to perform a check for this dialect
          throw t;

        if (!isRestarting.eval(t))
          throw t;

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

  public static <T> T inSerializable(Connection conn, SQLDialect dialect,
      int allowedRestartsCount, TxExpr<T> expr) {
    var ctx = new TxCtx(conn, Connection.TRANSACTION_SERIALIZABLE,
        allowedRestartsCount, dialect);
    return restartingTx(ctx, () -> inTransaction(ctx, expr));
  }

  public static void inSerializable(Connection conn, SQLDialect dialect,
      int allowedRestartsCount, TxStmt stmt) {
    inSerializable(conn, dialect, allowedRestartsCount, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  public static <T> T inSerializable(DataSource ds, SQLDialect dialect,
      int allowedRestartsCount, TxExpr<T> expr) {
    Expr<T> expr1 = conn -> inSerializable(conn, dialect, allowedRestartsCount,
        expr);
    return withConn(ds, expr1);
  }

  public static void inSerializable(DataSource ds, SQLDialect dialect,
      int allowedRestartsCount, TxStmt stmt) {
    inSerializable(ds, dialect, allowedRestartsCount, ctx -> {
      stmt.exec(ctx);
      return null;
    });
  }

  // POSTGRESQL
  static {
    RESTARTS_FINDERS.put(SQLDialect.POSTGRES, t -> {
      var e = asSQLException(t);
      return null != e && "40001".equals(e.getSQLState());
    });
  }

  // public static <T> T inPGserializable(Connection conn,
  // int allowedRestartsCount, TxExpr<T> expr) {
  // var ctx = new TxCtx(conn, Connection.TRANSACTION_SERIALIZABLE,
  // allowedRestartsCount, SQLDialect.POSTGRES);
  // return restartingTx(ctx, () -> inTransaction(ctx, expr));
  // }
  //
  // public static void inPGserializable(Connection conn, int
  // allowedRestartsCount,
  // TxStmt stmt) {
  // inPGserializable(conn, allowedRestartsCount, conn1 -> {
  // stmt.exec(conn1);
  // return null;
  // });
  // }
  //
  // public static <T> T inPGserializable(DataSource ds, int
  // allowedRestartsCount,
  // TxExpr<T> expr) {
  // Expr<T> expr1 = conn -> inPGserializable(conn, allowedRestartsCount, expr);
  // return withConn(ds, expr1);
  // }
  //
  // public static void inPGserializable(DataSource ds, int
  // allowedRestartsCount,
  // TxStmt stmt) {
  // inPGserializable(ds, allowedRestartsCount, conn -> {
  // stmt.exec(conn);
  // return null;
  // });
  // }

}
