package telsos.strings;

public final class Str {

  public static String strip(String s) {
    return s == null ? null : s.strip();
  }

  public static String wrapInQuotes(String s) {
    return "\"" + s + "\"";
  }

  private Str() {}

}
