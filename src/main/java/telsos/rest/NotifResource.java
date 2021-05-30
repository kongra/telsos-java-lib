package telsos.rest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import telsos.Utils;

@Path("/notif")
public class NotifResource {

  @GET
  @Path("/ping")
  @Produces(MediaType.TEXT_HTML)
  public Response ping(@Context HttpServletRequest request,
      @Context HttpServletResponse response) {
    return Utils.forward("/index.jsp", request, response);
  }

  @POST
  @Path("/pong")
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  public Response pong(String user) {
    return Response.ok().entity("{}").build();
  }

}
