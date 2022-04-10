// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.quark;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.is;

// @QuarkusTest
class GreetingResourceTest {

  // @Test
  void testHelloEndpoint() {
    given().when().get("/greeting/hello/12").then().statusCode(200)
        .body(is("Saying HELLO to Test12"));
  }

}