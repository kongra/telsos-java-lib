// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import java.util.function.DoublePredicate;
import java.util.function.IntPredicate;
import java.util.function.LongPredicate;

public final class Ranges {

  private static final String ERROR_MESSAGE_PREFIX = "start > end: ";

  public static IntPredicate rangePred(int start, int end) {
    if (start > end)
      throw new IllegalArgumentException(
          ERROR_MESSAGE_PREFIX + start + "," + end);
    return n -> start <= n && n <= end;
  }

  public static LongPredicate rangePred(long start, long end) {
    if (start > end)
      throw new IllegalArgumentException(
          ERROR_MESSAGE_PREFIX + start + "," + end);
    return n -> start <= n && n <= end;
  }

  public static DoublePredicate rangePred(double start, double end) {
    if (start > end)
      throw new IllegalArgumentException(
          ERROR_MESSAGE_PREFIX + start + "," + end);
    return n -> start <= n && n <= end;
  }
}
