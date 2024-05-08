// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses.instances;

import java.util.Objects;

import telsos.math.typeclasses.Bounded;

public record BoundsRecord<T>(T minBound, T maxBound)
    implements Bounded.Bounds<T> {

  public BoundsRecord {
    Objects.requireNonNull(minBound);
    Objects.requireNonNull(maxBound);
  }

}
