package telsos.java.lib;

public final class HumanReadable {

  private HumanReadable() {}

  private static final String[] BYTES_MULTIPLIES_POSTFIXES = { "bytes", "KB",
      "MB", "GB", "TB", "PB", "EB", "ZB", "YB" };

  public static String bytes(double bytes, int digits) {
    var index = 0;
    final var KB = 1024;
    for (index = 0; index < BYTES_MULTIPLIES_POSTFIXES.length; index++) {
      if (bytes < KB) {
        break;
      }

      bytes = bytes / KB;
    }

    return "%%.%df %%s".formatted(digits).formatted(bytes,
        BYTES_MULTIPLIES_POSTFIXES[index]);
  }

  private static final String[] NANOSECS_MULTIPLIES_POSTFIXES = { "ns", "µs",
      "ms", "s" };

  public static String nanosecs(double nanosecs, int digits) {
    var index = 0;
    for (index = 0; index < NANOSECS_MULTIPLIES_POSTFIXES.length; index++) {
      if (nanosecs < 1_000) {
        break;
      }

      nanosecs = nanosecs / 1_000;
    }

    return "%%.%df %%s".formatted(digits).formatted(nanosecs,
        NANOSECS_MULTIPLIES_POSTFIXES[index]);
  }
}
