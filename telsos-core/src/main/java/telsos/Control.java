// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.function.LongConsumer;

import telsos.math.newtype.NatLong;
import telsos.math.newtype.PosLong;
import telsos.util.AscRange;
import telsos.util.DescRange;

public final class Control {

  public static long times(NatLong n, LongConsumer body) {
    return rangeAsc(0, n.value(), 1, body);
  }

  public static long doRange(AscRange ascRange, LongConsumer body) {
    return doRange(ascRange, STEP_1, body);
  }

  public static long doRange(
      AscRange ascRange,
      PosLong step,
      LongConsumer body) {

    final var startInclusive = ascRange.value().first();
    final var endExclusive = ascRange.value().second();
    return rangeAsc(startInclusive, endExclusive, step.value(), body);
  }

  public static long doRange(DescRange descRange, LongConsumer body) {
    return doRange(descRange, STEP_1, body);
  }

  public static long doRange(
      DescRange descRange,
      PosLong step,
      LongConsumer body) {

    final var startInclusive = descRange.value().first();
    final var endExclusive = descRange.value().second();
    return rangeDesc(startInclusive, endExclusive, step.value(), body);
  }

  public static final PosLong STEP_1 = PosLong.of(1)
      .orElseThrow(ImpossibleException::new);

  private static long rangeAsc(
      long startInclusive,
      long endExclusive,
      long step,
      LongConsumer body) {

    var completedStepsCount = 0L;
    for (var i = startInclusive; i < endExclusive; i += step) {
      body.accept(i);
      completedStepsCount++;
    }
    return completedStepsCount;
  }

  private static long rangeDesc(
      long startInclusive,
      long endExclusive,
      long step,
      LongConsumer body) {

    var completedStepsCount = 0L;
    for (var i = startInclusive; i > endExclusive; i -= step) {
      body.accept(i);
      completedStepsCount++;
    }
    return completedStepsCount;
  }

  private Control() {}

}
