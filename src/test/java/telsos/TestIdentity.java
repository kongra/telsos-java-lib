package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class TestIdentity {

  static class Point2D {
    final double x;
    final double y;

    Point2D(double x, double y) {
      this.x = x;
      this.y = y;
    }

    @Override
    public final int hashCode() {
      final int prime = 31;
      int result = 1;
      long temp;
      temp = Double.doubleToLongBits(x);
      result = prime * result + (int) (temp ^ (temp >>> 32));
      temp = Double.doubleToLongBits(y);
      result = prime * result + (int) (temp ^ (temp >>> 32));
      return result;
    }

    @Override
    public final boolean equals(Object obj) {
      if (this == obj)
        return true;
      if (!(obj instanceof Point2D))
        return false;
      Point2D other = (Point2D) obj;
      if (Double.doubleToLongBits(x) != Double.doubleToLongBits(other.x))
        return false;
      if (Double.doubleToLongBits(y) != Double.doubleToLongBits(other.y))
        return false;
      return true;
    }
  }

  static class ColoredPoint2D extends Point2D {

    final String color;

    ColoredPoint2D(double x, double y, String color) {
      super(x, y);
      this.color = color;
    }
  }

  @Test
  void test() {
    var p1 = new Point2D(1, 1);
    var p2 = new Point2D(-3, 4);
    var p3 = new Point2D(1, 1);
    var p4 = new ColoredPoint2D(1, 1, "A kind of blue");

    assertThat(p1).isNotSameAs(p2).isEqualTo(p3).isEqualTo(p4);
  }

}
