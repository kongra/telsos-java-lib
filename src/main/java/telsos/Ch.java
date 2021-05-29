// Copyright (c) Konrad Grzanek
// Created 2019-07-22
package telsos;

import java.util.function.IntUnaryOperator;
import java.util.function.LongUnaryOperator;

public final class Ch {

  private Ch() {
    throw new AssertionError();
  }

  public static byte chPos(byte b) {
    if (b > 0)
      return b;
    throw new AssertionError();
  }

  public static short chPos(short s) {
    if (s > 0)
      return s;
    throw new AssertionError();
  }

  public static int chPos(int i) {
    if (i > 0)
      return i;
    throw new AssertionError();
  }

  public static long chPos(long l) {
    if (l > 0)
      return l;
    throw new AssertionError();
  }

  public static byte chNat(byte b) {
    if (b >= 0)
      return b;
    throw new AssertionError();
  }

  public static short chNat(short s) {
    if (s >= 0)
      return s;
    throw new AssertionError();
  }

  public static int chNat(int i) {
    if (i >= 0)
      return i;
    throw new AssertionError();
  }

  public static long chNat(long l) {
    if (l >= 0)
      return l;
    throw new AssertionError();
  }

  public static void validateRange(long start, long end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  public static void validateRange(int start, int end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  private static long chRange0(long start, long end, long l) {
    if (start <= l && l <= end)
      return l;
    throw new AssertionError();
  }

  private static int chRange0(int start, int end, int i) {
    if (start <= i && i <= end)
      return i;
    throw new AssertionError();
  }

  public static long chRange(long start, long end, long l) {
    validateRange(start, end);
    return chRange0(start, end, l);
  }

  public static LongUnaryOperator chRange(long start, long end) {
    validateRange(start, end);
    return l -> chRange0(start, end, l);
  }

  public static int chRange(int start, int end, int i) {
    validateRange(start, end);
    return chRange0(start, end, i);
  }

  public static int chIn(int i, int... values) {
    for (var v : values) {
      if (i == v)
        return i;
    }
    throw new AssertionError();
  }

  public static IntUnaryOperator chRange(int start, int end) {
    validateRange(start, end);
    return i -> chRange0(start, end, i);
  }

  public static double chNonNeg(double d) {
    if (d >= 0)
      return d;
    throw new AssertionError();
  }

  public static float chNonNeg(float f) {
    if (f >= 0)
      return f;
    throw new AssertionError();
  }

  public static double chPos(double d) {
    if (d > 0)
      return d;
    throw new AssertionError();
  }

  public static float chPos(float f) {
    if (f > 0)
      return f;
    throw new AssertionError();
  }

  public static String chNonBlank(String s) {
    if (s == null || s.isBlank())
      throw new AssertionError();
    return s;
  }

}
