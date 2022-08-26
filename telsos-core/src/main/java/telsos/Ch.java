// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.IntUnaryOperator;
import java.util.function.LongUnaryOperator;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import telsos.newtype.Newtype;

/**
 * type Ch T = T -> T
 *
 * @author kongra
 *
 * @param <T>
 */
@FunctionalInterface
public interface Ch<T> extends UnaryOperator<T> {

  static <T> Ch<T> of(Predicate<T> pred) {
    Objects.requireNonNull(pred);
    return t -> pred.test(t) ? t : fail(t);
  }

  static <T> Ch<T> of(Predicate<T> pred, Function<T, String> message) {
    Objects.requireNonNull(pred);
    Objects.requireNonNull(message);
    return t -> pred.test(t) ? t : fail(t, message);
  }

  static <T, S extends Newtype<T>> S checked(T t,
      Ch<T> ch,
      Function<T, S> constr) {
    return constr.apply(ch.apply(t));
  }

  static <T, S extends Newtype<T>> Optional<S> checkedOptionally(T t,
      Predicate<T> pred,
      Function<T, S> constr) {
    return pred.test(t)
        ? Optional.of(constr.apply(t))
        : Optional.empty();
  }

  static <T> T fail(T t) {
    return Utils.fail(new ChError(t));
  }

  static <T> T fail(T t, Function<T, String> message) {
    Utils.sneakyThrow(new ChError(t, message.apply(t)));
    return null;
  }

  static byte chPos(byte b) {
    if (b > 0)
      return b;
    throw new ChError(b);
  }

  static short chPos(short s) {
    if (s > 0)
      return s;
    throw new ChError(s);
  }

  static int chPos(int i) {
    if (i > 0)
      return i;
    throw new ChError(i);
  }

  static long chPos(long l) {
    if (l > 0)
      return l;
    throw new ChError(l);
  }

  static byte chNat(byte b) {
    if (b >= 0)
      return b;
    throw new ChError(b);
  }

  static short chNat(short s) {
    if (s >= 0)
      return s;
    throw new ChError(s);
  }

  static int chNat(int i) {
    if (i >= 0)
      return i;
    throw new ChError(i);
  }

  static long chNat(long l) {
    if (l >= 0)
      return l;
    throw new ChError(l);
  }

  static void validateRange(long start, long end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  static void validateRange(int start, int end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  private static long chRange0(long start, long end, long l) {
    if (start <= l && l <= end)
      return l;
    throw new ChError(l);
  }

  private static int chRange0(int start, int end, int i) {
    if (start <= i && i <= end)
      return i;
    throw new ChError(i);
  }

  static long chRange(long start, long end, long l) {
    validateRange(start, end);
    return chRange0(start, end, l);
  }

  static LongUnaryOperator chRange(long start, long end) {
    validateRange(start, end);
    return l -> chRange0(start, end, l);
  }

  static int chRange(int start, int end, int i) {
    validateRange(start, end);
    return chRange0(start, end, i);
  }

  static int chIn(int i, int... values) {
    for (final var v : values) {
      if (i == v)
        return i;
    }
    throw new ChError();
  }

  static IntUnaryOperator chRange(int start, int end) {
    validateRange(start, end);
    return i -> chRange0(start, end, i);
  }

  static double chNonNeg(double d) {
    if (d >= 0)
      return d;
    throw new ChError(d);
  }

  static float chNonNeg(float f) {
    if (f >= 0)
      return f;
    throw new ChError(f);
  }

  static double chPos(double d) {
    if (d > 0)
      return d;
    throw new ChError(d);
  }

  static float chPos(float f) {
    if (f > 0)
      return f;
    throw new ChError(f);
  }

}
