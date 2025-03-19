package telsos.java.lib.typeclass.instances;

import java.util.Objects;

import telsos.java.lib.typeclass.Bounded;

public record BoundsRecord<T>(T minBound, T maxBound)
    implements Bounded.Bounds<T> {

  public BoundsRecord {
    Objects.requireNonNull(minBound);
    Objects.requireNonNull(maxBound);
  }

}
