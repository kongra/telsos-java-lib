// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.quark;

import java.sql.SQLException;
import java.util.Map;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.sql.DataSource;
import javax.transaction.Transactional;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import telsos.profile.Test1;

@ApplicationScoped
@Path("/greeting")
public class GreetingResource {

  @Inject
  GreetingTools greetingTools;

  @Inject
  GreetingCounter greetingCounter;

  @Inject
  DataSource dataSource;

  @GET
  @Path("/hello/{id}")
  @Produces(MediaType.TEXT_PLAIN)
  public String hello(@PathParam("id") long id) {
    greetingCounter.inc();
    LOG.debug("Is's ok to be nice");

    return greetingTools.sayHello("Test" + id);
  }

  @GET
  @Path("/hello1/{id}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response hello1(@PathParam("id") String id) {
    greetingCounter.inc();
    try {
      final var id1 = Long.parseLong(id);
      final var s = greetingTools.sayHello("Hi-" + id1);
      return Response.ok(Map.of("result", s)).build();
    } catch (final NumberFormatException e) {
      return Response.status(Response.Status.BAD_REQUEST).build();
    }
  }

  @GET
  @Path("/count")
  @Produces(MediaType.TEXT_PLAIN)
  public Response count() {
    return Response.ok(greetingCounter.value()).build();
  }

  @GET
  @Path("/hello-jdbc/{id}")
  @Produces(MediaType.APPLICATION_JSON)
  public Response helloJdbc(@PathParam("id") long id) {
    return PostgresDataSource.of(dataSource).inSerializable(tx -> {
      try (var stmt = tx.conn
          .prepareStatement("select first_name from test1 where id=?")) {
        final var id_column = 1;
        stmt.setLong(id_column, id);

        try (var rs = stmt.executeQuery()) {
          if (rs.next()) {
            final var firstName = rs.getString("first_name");
            final var json = Map.of(id, firstName);
            return Response.ok(json).build();
          }
          return Response.ok(Map.of()).build();
        }
      } catch (final SQLException e) {
        LOG.error("Problem when executing", e);
        return Response.status(Status.INTERNAL_SERVER_ERROR).build();
      }
    });
  }

  @GET
  @Path("/hello-jpa/{id}")
  @Produces(MediaType.APPLICATION_JSON)
  @Transactional
  public Response helloJPA(@PathParam("id") long id) {
    final var result = (Test1) Test1.findById(id);
    if (result == null)
      return Response.status(Status.BAD_REQUEST).build();

    final var firstName = result.firstName;
    final var json = Map.of(id, firstName);
    return Response.ok(json).build();
  }

  public GreetingResource() {
    LOG.info("GreetingResource::constructor");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingResource.class);

}