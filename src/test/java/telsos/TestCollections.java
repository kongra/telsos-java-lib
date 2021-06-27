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

class TestCollections {

  @Test
  void test() {
    List<String> l1 = new ArrayList<>();
    assertThat(l1.isEmpty()).isTrue();

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
    .isThrownBy(() -> {
      l1.get(0);
    });

    l1.add("xyz");
    assertThat(l1.isEmpty()).isFalse();
    assertThat(l1.get(0)).isEqualTo("xyz");

    // var iter1 = l1.iterator();
    // while (iter1.hasNext()) {
    // System.out.println(iter1.next());
    // }
    //
    // for (String s1 : l1) {
    // System.out.println(s1);
    // }

    var l2 = List.of(123, 145, -3);
    assertThat(l2.size()).isEqualTo(3);

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
    Status s1 = Status.OK;

    assertThat(s1 == Status.OK).isTrue();
    assertThat(s1 == Status.ERROR).isFalse();

    assertThat(s1.equals(Status.OK)).isTrue();
    assertThat(s1.toIntCode()).isEqualTo(Integer.MAX_VALUE);
  }

  @Test
  void test2() {
    Set<Status> statuses1 = new HashSet<>();
    statuses1.add(Status.OK);

    assertThat(statuses1.contains(Status.OK)).isTrue();
    assertThat(statuses1.contains(Status.ERROR)).isFalse();

    EnumSet<Status> statuses2 = EnumSet.of(Status.OK);
    assertThat(statuses2.contains(Status.OK)).isTrue();
    assertThat(statuses2.contains(Status.ERROR)).isFalse();

  }

  @Test
  void test3() {
    Map<Status, String> m1 = new EnumMap<>(Status.class);
    m1.put(Status.OK, "It's OK.");
    m1.put(Status.ERROR, "It's NOT OK.");

  }

}
