// © 2021 Konrad Grzanek <kongra@gmail.com>
package telsos.jdbc;

import javax.sql.DataSource;

import telsos.jdbc.Tx.TxExpr;
import telsos.jdbc.Tx.TxStmt;

public interface Ds {

  Dialect dialect();

  DataSource dataSource();

  default <T> T inSerializable(int allowedRestartsCount, TxExpr<T> expr) {
    return Tx.inSerializable(dialect(), dataSource(), allowedRestartsCount,
        expr);
  }

  default void inSerializable(int allowedRestartsCount, TxStmt stmt) {
    Tx.inSerializable(dialect(), dataSource(), allowedRestartsCount, stmt);
  }

  default <T> T inSerializable1(TxExpr<T> expr) {
    final var restartsCount = 2;
    return inSerializable(restartsCount, expr);
  }

  default void inSerializable1(TxStmt stmt) {
    final var restartsCount = 1;
    inSerializable(restartsCount, stmt);
  }

  default <T> T inSerializable2(TxExpr<T> expr) {
    final var restartsCount = 2;
    return inSerializable(restartsCount, expr);
  }

  default void inSerializable2(TxStmt stmt) {
    final var restartsCount = 2;
    inSerializable(restartsCount, stmt);
  }

  default <T> T inSerializable5(TxExpr<T> expr) {
    final var restartsCount = 5;
    return inSerializable(restartsCount, expr);
  }

  default void inSerializable5(TxStmt stmt) {
    final var restartsCount = 5;
    inSerializable(restartsCount, stmt);
  }

  default <T> T inSerializable8(TxExpr<T> expr) {
    final var restartsCount = 8;
    return inSerializable(restartsCount, expr);
  }

  default void inSerializable8(TxStmt stmt) {
    final var restartsCount = 8;
    inSerializable(restartsCount, stmt);
  }

  default <T> T inSerializable(TxExpr<T> expr) {
    return inSerializable(Integer.MAX_VALUE, expr);
  }

  default void inSerializable(TxStmt stmt) {
    inSerializable(Integer.MAX_VALUE, stmt);
  }

  default <T> T inReadCommitted(TxExpr<T> expr) {
    return Tx.inReadCommitted(dialect(), dataSource(), expr);
  }

  default void inReadCommitted(TxStmt stmt) {
    Tx.inReadCommitted(dialect(), dataSource(), stmt);
  }

}
