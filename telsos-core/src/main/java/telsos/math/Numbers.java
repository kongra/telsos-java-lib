package telsos.math;

public final class Numbers {

  public static boolean areEqual(double x, double y, double epsilon) {
    return Math.abs(x - y) < epsilon;
  }

  public static boolean isEffectivelyInt(double d) {
    return areEqual(d, Math.rint(d), 0.0000000000001D);
  }

  private Numbers() {}

}
