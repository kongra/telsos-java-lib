// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayDeque;

import org.junit.jupiter.api.Test;

import io.vavr.control.Option;

public class TestDepthFirstSearch {

  public static Option<String> search(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start);
  }

  public static Option<String> searchWithArray(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start, ArrayDeque::new);
  }

  public static Option<String> search1(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children1, goal::equals)
        .search(start);
  }

  public static Option<String> searchWithArray1(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children1, goal::equals)
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
