// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.utils;

import java.util.Optional;

import telsos.newtypes.Newtype;

public final class DescRange extends Newtype<LongPair> {

  public static Optional<DescRange> of(LongPair longPair) {
    return of(longPair, DescRange::isValid, DescRange::new);
  }

  public static Optional<DescRange> of(long startInclusive,
      long endExclusive) {
    return of(new LongPair(startInclusive, endExclusive), DescRange::isValid,
        DescRange::new);
  }

  public static boolean isValid(LongPair longPair) {
    return longPair.first() >= longPair.second();
  }

  private DescRange(LongPair value) {
    super(value);
  }

}