package telsos.java.lib.typeclass.instances;

import telsos.java.lib.O;
import telsos.java.lib.typeclass.Bounded;

public record BoundsRecord<T>(T minBound, T maxBound)
    implements Bounded.Bounds<T> {

  public BoundsRecord {
    O.nn(minBound);
    O.nn(maxBound);
  }

}
