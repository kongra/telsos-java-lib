package telsos.java.lib.math;

public final class Numbers {

  public static boolean areEqual(double x, double y, double epsilon) {
    return Math.abs(x - y) < epsilon;
  }

  public static boolean isEffectivelyInt(double d) {
    final var epsilon = 0.0000000000001D;
    return areEqual(d, Math.rint(d), epsilon);
  }

  private Numbers() {}

}