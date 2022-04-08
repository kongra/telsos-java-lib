package telsos.quark;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.is;

import org.junit.jupiter.api.Test;

import io.quarkus.test.junit.QuarkusTest;

@QuarkusTest
class GreetingResourceTest {

  @Test
  void testHelloEndpoint() {
    given().when().get("/greeting/hello/12").then().statusCode(200)
        .body(is("Saying HELLO to Test12"));
  }

}