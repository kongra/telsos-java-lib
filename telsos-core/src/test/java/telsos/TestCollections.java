// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TestCollections {

  @Test
  void test() {
    List<String> l1 = new ArrayList<>();
    assertThat(l1).isEmpty();
    assertThatExceptionOfType(IndexOutOfBoundsException.class)
        .isThrownBy(() -> {
          l1.get(0);
        });

    l1.add("xyz");
    assertThat(l1).isNotEmpty();
    assertThat(l1.get(0)).isEqualTo("xyz");

    var l2 = List.of(123, 145, -3);
    assertThat(l2).hasSize(3);

    assertThatExceptionOfType(UnsupportedOperationException.class)
        .isThrownBy(() -> {
          l2.add(45);
        });
  }

  enum Status {
    OK(0) {
      @Override
      public int toIntCode() {
        return Integer.MAX_VALUE - code;
      }
    },
    ERROR(-1) {
      @Override
      public int toIntCode() {
        return Integer.MIN_VALUE - code;
      }
    },
    UNKNOWN(1) {
      @Override
      public int toIntCode() {
        return code;
      }
    };

    public final byte code;

    Status(int code) {
      Ch.chRange(Byte.MIN_VALUE, Byte.MAX_VALUE, code);
      this.code = (byte) code;
    }

    public abstract int toIntCode();
  }

  @Test
  void test1() {
    var s1 = Status.OK;

    assertThat(s1).isSameAs(Status.OK).isNotSameAs(Status.ERROR)
        .isEqualTo(Status.OK);
    assertThat(s1.toIntCode()).isEqualTo(Integer.MAX_VALUE);
  }

  @Test
  void test2() {
    Set<Status> statuses1 = new HashSet<>();
    statuses1.add(Status.OK);
    assertThat(statuses1).contains(Status.OK).doesNotContain(Status.ERROR);

    EnumSet<Status> statuses2 = EnumSet.of(Status.OK);
    assertThat(statuses2).contains(Status.OK).doesNotContain(Status.ERROR);
  }

  @Test
  void test3() {
    Map<Status, String> m1 = new EnumMap<>(Status.class);
    assertThat(m1.put(Status.OK, "It's OK.")).isNull();
    assertThat(m1.put(Status.ERROR, "It's NOT OK.")).isNull();
  }

}
