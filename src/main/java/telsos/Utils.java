// Copyright (c) kongra
// Created 18.07.19
package telsos;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonProcessingException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import telsos.function.ThrowingSupplier;

public final class Utils {

  /**
   * Throw even checked exceptions without being required to declare them or
   * catch them. Suggested idiom:
   * <p>
   * <code>throw sneakyThrow( some exception );</code>
   */
  public static TelsosException sneakyThrow(Throwable t) {
    // http://www.mail-archive.com/javaposse@googlegroups.com/msg05984.html
    if (t == null)
      throw new NullPointerException();
    Utils.sneakyThrow0(t);
    return new TelsosException();
  }

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void sneakyThrow0(Throwable t) throws T {
    throw (T) t;
  }

  public static Response forward(String path, HttpServletRequest request,
      HttpServletResponse response) {
    try {
      request.getRequestDispatcher(path).forward(request, response);
      return null;
    } catch (ServletException | IOException e) {
      return Response.status(Status.INTERNAL_SERVER_ERROR).build();
    }
  }

  public static Response endPoint(
      ThrowingSupplier<Response, JsonProcessingException> supplier) {
    try {
      return supplier.get();
    } catch (JsonProcessingException e) {
      return Response.status(Status.BAD_REQUEST).build();
    } catch (TelsosException | ChError e) {
      // TODO: add logging
      return Response.status(Status.INTERNAL_SERVER_ERROR).build();
    }
  }

  private Utils() {
  }
}
