package telsos.rest;

import jakarta.validation.Valid;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Response;

@SuppressWarnings("static-method")
@Path("/notif")
public class NotifResource {

  @GET
  @Path("/ping")
  public Response ping() {
    System.out.println("It's alive");
    return Response.ok().entity("Service online").build();
  }

  @POST
  @Path("/pong")
  @Consumes("application/json")
  @Produces("application/json")
  public User pong(@Valid User user) {
    return user;
  }

}
