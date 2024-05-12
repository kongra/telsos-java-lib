// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses.instances;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Objects;

import telsos.math.typeclasses.Bounded;
import telsos.math.typeclasses.Eq;
import telsos.math.typeclasses.Monoid;
import telsos.math.typeclasses.Num;
import telsos.math.typeclasses.Ord;
import telsos.math.typeclasses.Semigroup;

public final class BigDecimalInstances {

  private static final BoundsRecord<BigDecimal> BIG_DECIMAL_BOUNDS = new BoundsRecord<>(
      new BigDecimal("-0").setScale(0),
      new BigDecimal("0").setScale(0));

  public static final Bounded<BigDecimal> BOUNDED = () -> BIG_DECIMAL_BOUNDS;

  public static final Eq<BigDecimal> EQ = (x, y) -> x.compareTo(y) == 0;

  public static final Ord<BigDecimal> ORD = Ord.of(BigDecimal::compareTo);

  public static Semigroup<BigDecimal> semigroupWith(MathContext mc) {
    return monoidWith(mc);
  }

  public static final Semigroup<BigDecimal> SEMIGROUP;

  public static Monoid<BigDecimal> monoidWith(MathContext mc) {
    Objects.requireNonNull(mc);
    return new Monoid<>() {
      @Override
      public BigDecimal sconcat(BigDecimal x, BigDecimal y) {
        return x.add(y, mc);
      }

      @Override
      public BigDecimal mempty() {
        return BigDecimal.ZERO;
      }
    };
  }

  public static final Monoid<BigDecimal> MONOID = new Monoid<>() {
    @Override
    public BigDecimal sconcat(BigDecimal x, BigDecimal y) {
      return x.add(y);
    }

    @Override
    public BigDecimal mempty() {
      return BigDecimal.ZERO;
    }
  };

  static {
    SEMIGROUP = MONOID;
  }

  public static Num<BigDecimal> numWith(MathContext mc) {
    Objects.requireNonNull(mc);
    return new Num<>() {

      @Override
      public BigDecimal add(BigDecimal x, BigDecimal y) {
        return x.add(y, mc);
      }

      @Override
      public BigDecimal subtract(BigDecimal x, BigDecimal y) {
        return x.subtract(y, mc);
      }

      @Override
      public BigDecimal multiply(BigDecimal x, BigDecimal y) {
        return x.multiply(y, mc);
      }

      @Override
      public BigDecimal divide(BigDecimal x, BigDecimal y) {
        return x.divide(y, mc);
      }

      @Override
      public BigDecimal negate(BigDecimal x) {
        return x.negate();
      }

      @Override
      public int signum(BigDecimal x) {
        return x.signum();
      }

      @Override
      public BigDecimal fromInt(int i) {
        return new BigDecimal(i);
      }

    };
  }

  private BigDecimalInstances() {}

}
