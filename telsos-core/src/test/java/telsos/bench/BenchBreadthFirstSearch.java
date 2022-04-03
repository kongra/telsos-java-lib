// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.bench;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.infra.Blackhole;

import telsos.paip.TestBreadthFirstSearch;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class BenchBreadthFirstSearch {

  @Benchmark
  public static void benchaa(Blackhole blackhole) {
    var v = TestBreadthFirstSearch.search1("a", "a");
    blackhole.consume(v);
  }

  @Benchmark
  public static void benchak(Blackhole blackhole) {
    var v = TestBreadthFirstSearch.search1("a", "k");
    blackhole.consume(v);
  }

  @Benchmark
  public static void benchawww(Blackhole blackhole) {
    var v = TestBreadthFirstSearch.search1("a", "www");
    blackhole.consume(v);
  }

}
