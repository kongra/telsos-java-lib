package telsos.java.lib.bench;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.infra.Blackhole;

import telsos.java.lib.paip.TestDepthFirstSearch;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class BenchDepthFirstSearch {

  @Benchmark
  public static void benchaa(Blackhole blackhole) {
    final var v = TestDepthFirstSearch.search1("a", "a");
    blackhole.consume(v);
  }

  @Benchmark
  public static void benchap(Blackhole blackhole) {
    final var v = TestDepthFirstSearch.search1("a", "p");
    blackhole.consume(v);
  }

  @Benchmark
  public static void benchau(Blackhole blackhole) {
    final var v = TestDepthFirstSearch.search1("a", "u");
    blackhole.consume(v);
  }

  @Benchmark
  public static void benchax(Blackhole blackhole) {
    final var v = TestDepthFirstSearch.search1("a", "x");
    blackhole.consume(v);
  }

}
