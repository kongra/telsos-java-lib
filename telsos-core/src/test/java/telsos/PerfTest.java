package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.ArrayList;

import org.eclipse.collections.impl.list.mutable.primitive.DoubleArrayList;
import org.junit.jupiter.api.Test;

class PerfTest {
  
  static int test1(int n) {
    var queue = new ArrayList<Double>(n);
    for (int i = 0; i < n; i++) {
      queue.add((2.0f / 3.0f) * (double) i);
    }
    System.out.println("Queue len is " + queue.size());
    return queue.size();
  }
  
  static int test2(int n) {
    var queue = new DoubleArrayList(n);
    for (int i = 0; i < n; i++) {
      queue.add((2.0f / 3.0f) * (double) i);
    }
    System.out.println("Queue len is " + queue.size());
    return queue.size();
  }
  
  @SuppressWarnings("static-method")
  @Test
  void test() {
    for (int i = 0; i < 100; i++) {
      assertThat(test2(10_000_000)).isPositive();
    }
  }

}
