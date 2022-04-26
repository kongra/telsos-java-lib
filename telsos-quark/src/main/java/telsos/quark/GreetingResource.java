// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.quark;

import static telsos.Ch.chRange;

import java.util.Map;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("/greeting")
public class GreetingResource {

  @Inject
  GreetingTools greetingTools;

  @Inject
  GreetingCounter greetingCounter;

  @GET
  @Path("/hello/{id}")
  @Produces(MediaType.TEXT_PLAIN)
  public String hello(@PathParam("id") long id) {
    greetingCounter.inc();
    chRange(1, 100, id);
    LOG.debug("Is's ok to be nice");

    return greetingTools.sayHello("Test" + id);
  }

  @GET
  @Path("/hello1/{id}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response hello1(@PathParam("id") String id) {
    greetingCounter.inc();
    try {
      var id1 = Long.parseLong(id);
      var s = greetingTools.sayHello("Hi-" + id1);
      return Response.ok(Map.of("result", s)).build();
    } catch (NumberFormatException e) {
      return Response.status(Response.Status.BAD_REQUEST).build();
    }
  }

  @GET
  @Path("/count")
  @Produces(MediaType.TEXT_PLAIN)
  public Response count() {
    return Response.ok(greetingCounter.value()).build();
  }

  public GreetingResource() {
    LOG.info("GreetingResource::constructor");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingResource.class);

}