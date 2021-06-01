package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.ArrayList;
import java.util.List;

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

    //    var iter1 = l1.iterator();
    //    while (iter1.hasNext()) {
    //      System.out.println(iter1.next());
    //    }
    //
    //    for (String s1 : l1) {
    //      System.out.println(s1);
    //    }

    var l2 = List.of(123, 145, -3);
    assertThat(l2.size()).isEqualTo(3);

    assertThatExceptionOfType(UnsupportedOperationException.class)
    .isThrownBy(() -> {
      l2.add(45);
    });
  }

}
