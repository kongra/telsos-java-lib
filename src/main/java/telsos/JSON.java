package telsos;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;

public class JSON {

  public static synchronized ObjectReader readerFor(Class<?> c) {
    var reader = readers.get(c);
    if (null == reader) {
      reader = mapper.readerFor(c);
      readers.put(c, reader);
    }
    return reader;
  }

  public static synchronized ObjectWriter writerFor(Class<?> c) {
    var writer = writers.get(c);
    if (null == writer) {
      writer = mapper.writerFor(c);
      writers.put(c, writer);
    }
    return writer;
  }

  public static <T> T readValue(Class<?> c, String json)
      throws JsonMappingException, JsonProcessingException {
    return readerFor(c).readValue(json);
  }

  public static String writeValue(Class<?> c, Object obj)
      throws JsonProcessingException {
    return writerFor(c).writeValueAsString(obj);
  }

  public static String writeValue(Object obj) throws JsonProcessingException {
    return writeValue(obj.getClass(), obj);
  }

  private static final ObjectMapper mapper = new ObjectMapper();
  private static final Map<Class<?>, ObjectReader> readers = new HashMap<>();
  private static final Map<Class<?>, ObjectWriter> writers = new HashMap<>();

  private JSON() {
  }

}
