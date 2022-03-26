package telsos;

import java.util.List;

import org.junit.jupiter.api.Test;

import telsos.pprint.TreePrinter;

class TestTreePrinter {

  @Test
  void test() {
    TreePrinter<Integer> printer = TreePrinter.of(i -> List.of(i + 1, i + 2),
        String::valueOf);

    var depth = 2;
    printer.pprint(0, depth, s -> System.out.print(s));
  }
}
