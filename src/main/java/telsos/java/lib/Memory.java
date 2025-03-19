package telsos.java.lib;

public final class Memory {

  public static String room() {
    final var rt = Runtime.getRuntime();
    final var free = rt.freeMemory();
    final var total = rt.totalMemory();
    final var mx = rt.maxMemory();
    final var used = total - free;
    final var digits = 2;

    return "Used: %s | Free: %s | Total :%s | Max: %s".formatted(
        HumanReadable.bytes(used, digits),
        HumanReadable.bytes(free, digits),
        HumanReadable.bytes(total, digits),
        HumanReadable.bytes(mx, digits));
  }

  private Memory() {}

}
