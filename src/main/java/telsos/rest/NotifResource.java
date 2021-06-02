package telsos.rest;

import java.io.IOException;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import telsos.JSON;
import telsos.Utils;
import telsos.profile.Profile;

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
  public Response pong(String profileJSON) {
    try {
      return pongImpl(profileJSON);
    } catch (IOException e) {
      return Response.status(Status.INTERNAL_SERVER_ERROR).build();
    }
  }

  private static Response pongImpl(String profileJSON) throws IOException {
    var profile1 = JSON.readValue(Profile.class, profileJSON);
    return Response.ok().entity(JSON.writeValueAsString(profile1)).build();
  }

  @POST
  @Path("/pong1")
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  public @Valid User pong(@Valid User user) {
    return user;
  }

}
