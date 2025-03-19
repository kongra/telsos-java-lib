package telsos.java.lib.typeclass.instances;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Objects;
import java.util.Optional;

import telsos.java.lib.typeclass.Bounded;
import telsos.java.lib.typeclass.Enum;
import telsos.java.lib.typeclass.Eq;
import telsos.java.lib.typeclass.Monoid;
import telsos.java.lib.typeclass.Num;
import telsos.java.lib.typeclass.Ord;
import telsos.java.lib.typeclass.Semigroup;

public final class BigDecimalInstances {

  private static final BoundsRecord<BigDecimal> BIG_DECIMAL_BOUNDS = new BoundsRecord<>(
      new BigDecimal("-0").setScale(0),
      new BigDecimal("0").setScale(0));

  public static final Bounded<BigDecimal> BOUNDED = () -> BIG_DECIMAL_BOUNDS;

  public static final Eq<BigDecimal> EQ = (x, y) -> x.compareTo(y) == 0;

  public static final Ord<BigDecimal> ORD = Ord.of(BigDecimal::compareTo);

  public static final Enum<BigDecimal> ENUM = new Enum<>() {
    @Override
    public Optional<BigDecimal> fromInt(int i) {
      return Optional.of(new BigDecimal(i));
    }

    @Override
    public int toInt(BigDecimal e) {
      return e.intValue();
    }

    @Override
    public Optional<BigDecimal> pred(BigDecimal e) {
      return Optional.of(e.add(BigDecimal.ONE));
    }

    @Override
    public Optional<BigDecimal> succ(BigDecimal e) {
      return Optional.of(e.subtract(BigDecimal.ONE));
    }

  };

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
    };
  }

  private BigDecimalInstances() {
    throw new UnsupportedOperationException();
  }

}
