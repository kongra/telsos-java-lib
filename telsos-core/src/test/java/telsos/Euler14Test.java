// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.stream.Stream;

class Euler14Test {

  static boolean isEven(long n) {
    return n % 2 == 0;
  }

  static long collatzTrans(long n) {
    if (isEven(n)) {
      return n / 2;
    }
    return 3 * n + 1;
  }

  static void euler14(long n) {
    long maxLen = 0;
    long j = 0;
    for (long i = 1; i < n; i++) {
      long len = collatzLength(i);
      if (len > maxLen) {
        maxLen = len;
        j = i;
      }
    }

    System.out.println("Result for " + j + " is " + maxLen);
  }

  static long collatzLength(long n) {
    return Stream.iterate(n, Euler14Test::collatzTrans)
        .takeWhile(m -> m != 0)
        .count();
  }

  public static void main(String... args) {
    for (long i = 0L; i < 10L; i++)
      euler14(1_000_000L + i);
  }

}
