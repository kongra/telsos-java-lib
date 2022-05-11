package telsos.profile;

import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.validation.Validator;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import telsos.Ch;

@ApplicationScoped
@Path("/profiles")
public class ProfileResource {

  @Inject
  ProfileTools profileTools;

  @Inject
  Validator validator;

  @GET
  @Path("/{id}")
  public Response getProfile(@PathParam("id") long id) {
    var profile = profileTools.findById(id).getOrNull();
    if (profile == null)
      return Response.status(Status.BAD_REQUEST).build();

    var violations = validator.validate(profile);
    if (!violations.isEmpty())
      return Response.status(Status.INTERNAL_SERVER_ERROR).build();

    return Response.ok(Map.of("profile", profile)).build();
  }

  @POST
  @Path("/{email}")
  public Response addProfile(@PathParam("email") String email) {
    if (!Ch.emailValidator.isValid(email))
      return Response
          .status(Status.BAD_REQUEST.getStatusCode(), "Malformed email")
          .build();

    var profile = profileTools.addProfile(email);
    if (profile.isDefined())
      return Response.ok(profile.get()).status(Status.CREATED).build();

    return Response.status(Status.BAD_REQUEST.getStatusCode(), "Email in use")
        .build();
  }

}
