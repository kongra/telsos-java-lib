// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import static java.util.Map.entry;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.jupiter.api.Test;

import io.vavr.control.Option;

public class TestBreadthFirstSearch {

  public static Entry<String, Iterable<String>> en(String key,
      String... values) {
    return entry(key, List.of(values));
  }

  public static final Map<String, Iterable<String>> tree1 = Map.ofEntries(
      en("a", "b", "c"), en("b", "d", "e"), en("c", "f", "g"),
      en("d", "h", "i"), en("e", "j", "k"), en("f", "l", "m"),
      en("g", "n", "o"), en("h", "p", "q"), en("i", "r", "s"),
      en("o", "t", "u"), en("u", "v", "w"));

  public static Iterable<String> children(String s) {
    return tree1.getOrDefault(s, List.of());
  }

  public static Option<String> search(String start, String goal) {
    return BreadthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start);
  }

  public static Option<String> searchWithArray(String start, String goal) {
    return BreadthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start, ArrayDeque::new);
  }

  public static Iterable<String> children1(String s) {
    return tree1.getOrDefault(s, null);
  }

  public static Option<String> search1(String start, String goal) {
    return BreadthFirstSearch
        .of(TestBreadthFirstSearch::children1, goal::equals).search(start);
  }

  public static Option<String> searchWithArray1(String start, String goal) {
    return BreadthFirstSearch
        .of(TestBreadthFirstSearch::children1, goal::equals)
        .search(start, ArrayDeque::new);
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearch() {
    assertThat(search("a", "a")).isEqualTo(Option.of("a"));
    assertThat(search("a", "k")).isEqualTo(Option.of("k"));
    assertThat(search("k", "k")).isEqualTo(Option.of("k"));

    assertThat(search("d", "a")).isEqualTo(Option.none());
    assertThat(search("www", "a")).isEqualTo(Option.none());
    assertThat(search("a", "www")).isEqualTo(Option.none());
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearch1() {
    assertThat(search1("a", "a")).isEqualTo(Option.of("a"));
    assertThat(search1("a", "k")).isEqualTo(Option.of("k"));
    assertThat(search1("k", "k")).isEqualTo(Option.of("k"));

    assertThat(search1("d", "a")).isEqualTo(Option.none());
    assertThat(search1("www", "a")).isEqualTo(Option.none());
    assertThat(search1("a", "www")).isEqualTo(Option.none());
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearchWithArray() {
    assertThat(searchWithArray("a", "a")).isEqualTo(Option.of("a"));
    assertThat(searchWithArray("a", "k")).isEqualTo(Option.of("k"));
    assertThat(searchWithArray("k", "k")).isEqualTo(Option.of("k"));

    assertThat(searchWithArray("d", "a")).isEqualTo(Option.none());
    assertThat(searchWithArray("www", "a")).isEqualTo(Option.none());
    assertThat(searchWithArray("a", "www")).isEqualTo(Option.none());
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearchWithArray1() {
    assertThat(searchWithArray1("a", "a")).isEqualTo(Option.of("a"));
    assertThat(searchWithArray1("a", "k")).isEqualTo(Option.of("k"));
    assertThat(searchWithArray1("k", "k")).isEqualTo(Option.of("k"));

    assertThat(searchWithArray1("d", "a")).isEqualTo(Option.none());
    assertThat(searchWithArray1("www", "a")).isEqualTo(Option.none());
    assertThat(searchWithArray1("a", "www")).isEqualTo(Option.none());
  }

}
