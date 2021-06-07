package telsos.rest;

import com.fasterxml.jackson.core.JsonProcessingException;

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
    return Utils.endPoint(() -> pongImpl(profileJSON));
  }

  private static Response pongImpl(String profileJSON)
      throws JsonProcessingException {
    var profile = JSON.readValue(Profile.class, profileJSON);
    return Response.ok().entity(JSON.writeValueAsString(profile)).build();
  }

  @POST
  @Path("/pong1")
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  public @Valid User pong(@Valid User user) {
    return user;
  }

}
