// Copyright (c) Konrad Grzanek
// Created 2019-07-22
package telsos;

import java.util.function.IntUnaryOperator;
import java.util.function.LongUnaryOperator;

import org.apache.commons.validator.routines.EmailValidator;

@FunctionalInterface
public interface Ch<T> {

  T ch(T obj);

  static byte chPos(byte b) {
    if (b > 0)
      return b;
    throw new ChError();
  }

  static short chPos(short s) {
    if (s > 0)
      return s;
    throw new ChError();
  }

  static int chPos(int i) {
    if (i > 0)
      return i;
    throw new ChError();
  }

  static long chPos(long l) {
    if (l > 0)
      return l;
    throw new ChError();
  }

  static byte chNat(byte b) {
    if (b >= 0)
      return b;
    throw new ChError();
  }

  static short chNat(short s) {
    if (s >= 0)
      return s;
    throw new ChError();
  }

  static int chNat(int i) {
    if (i >= 0)
      return i;
    throw new ChError();
  }

  static long chNat(long l) {
    if (l >= 0)
      return l;
    throw new ChError();
  }

  static void validateRange(long start, long end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  static void validateRange(int start, int end) {
    if (start > end)
      throw new IllegalArgumentException(
          "start > end with start=" + start + ", end=" + end);
  }

  private static long chRange0(long start, long end, long l) {
    if (start <= l && l <= end)
      return l;
    throw new ChError();
  }

  private static int chRange0(int start, int end, int i) {
    if (start <= i && i <= end)
      return i;
    throw new ChError();
  }

  static long chRange(long start, long end, long l) {
    validateRange(start, end);
    return chRange0(start, end, l);
  }

  static LongUnaryOperator chRange(long start, long end) {
    validateRange(start, end);
    return l -> chRange0(start, end, l);
  }

  static int chRange(int start, int end, int i) {
    validateRange(start, end);
    return chRange0(start, end, i);
  }

  static int chIn(int i, int... values) {
    for (var v : values) {
      if (i == v)
        return i;
    }
    throw new ChError();
  }

  static IntUnaryOperator chRange(int start, int end) {
    validateRange(start, end);
    return i -> chRange0(start, end, i);
  }

  static double chNonNeg(double d) {
    if (d >= 0)
      return d;
    throw new ChError();
  }

  static float chNonNeg(float f) {
    if (f >= 0)
      return f;
    throw new ChError();
  }

  static double chPos(double d) {
    if (d > 0)
      return d;
    throw new ChError();
  }

  static float chPos(float f) {
    if (f > 0)
      return f;
    throw new ChError();
  }

  static String chNonBlank(String s) {
    if (s == null || s.isBlank())
      throw new ChError();
    return s;
  }

  Ch<String> nonBlank = Ch::chNonBlank;

  EmailValidator emailValidator = EmailValidator.getInstance();

  static String chEmail(String s) {
    if (emailValidator.isValid(s))
      return s;
    throw new ChError("Invalid email " + s);
  }

  Ch<String> email = Ch::chEmail;
}
