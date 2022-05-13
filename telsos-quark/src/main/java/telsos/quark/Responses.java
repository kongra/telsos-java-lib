package telsos.quark;

import java.util.function.Supplier;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

public final class Responses {

  public static Response ok(Object entity) {
    return Response.ok(entity).build();
  }

  public static Response badRequest() {
    return Response.status(Status.BAD_REQUEST).build();
  }

  public static Supplier<Response> badRequestSupplier() {
    return () -> Response.status(Status.BAD_REQUEST).build();
  }

  public static Response internalServerError() {
    return Response.status(Status.INTERNAL_SERVER_ERROR).build();
  }

  private Responses() {}

}
