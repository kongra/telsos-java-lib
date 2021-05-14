package telsos.db;

import javax.sql.DataSource;

import org.jooq.SQLDialect;

import telsos.db.Tools.TxExpr;
import telsos.db.Tools.TxStmt;

public abstract class DBI {

  public abstract SQLDialect dialect();

  public abstract DataSource dataSource();

  public final <T> T inSerializable(int allowedRestartsCount, TxExpr<T> expr) {
    return Tools.inSerializable(dialect(), dataSource(), allowedRestartsCount,
        expr);
  }

  public final void inSerializable(int allowedRestartsCount, TxStmt stmt) {
    Tools.inSerializable(dialect(), dataSource(), allowedRestartsCount, stmt);
  }

  public final <T> T inSerializable1(TxExpr<T> expr) {
    return inSerializable(1, expr);
  }

  public final void inSerializable1(TxStmt stmt) {
    inSerializable(1, stmt);
  }

  public final <T> T inSerializable2(TxExpr<T> expr) {
    return inSerializable(2, expr);
  }

  public final void inSerializable2(TxStmt stmt) {
    inSerializable(2, stmt);
  }

  public final <T> T inSerializable5(TxExpr<T> expr) {
    return inSerializable(5, expr);
  }

  public final void inSerializable5(TxStmt stmt) {
    inSerializable(5, stmt);
  }

  public final <T> T inSerializable8(TxExpr<T> expr) {
    return inSerializable(8, expr);
  }

  public final void inSerializable8(TxStmt stmt) {
    inSerializable(8, stmt);
  }

  public final <T> T inSerializable(TxExpr<T> expr) {
    return inSerializable(Integer.MAX_VALUE, expr);
  }

  public final void inSerializable(TxStmt stmt) {
    inSerializable(Integer.MAX_VALUE, stmt);
  }

  public final <T> T inReadCommitted(TxExpr<T> expr) {
    return Tools.inReadCommitted(dialect(), dataSource(), expr);
  }

  public final void inReadCommitted(TxStmt stmt) {
    Tools.inReadCommitted(dialect(), dataSource(), stmt);
  }

}
