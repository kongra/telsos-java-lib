package telsos;

public class TestMyData {

  public static void main(String... args) {
    // MyData md1 = new MyData("kongra@gmail.com");
    // MyData md2 = new MyData("kongra@gmail.com");
    //
    // md1.setName("Abcd");
    // md2.setName("Xyz");
    //
    // System.out.println(md1);
    // System.out.println(md1.equals(md2));

    MyData md3 = new MyData.MyDataBuilder().email("kongra@gmail.com").age(18)
        .build();
    System.out.println(md3);

    Delay<Integer> d1 = Delay.delay(() -> {
      System.out.println("A long way in front of me ...");
      return 5;
    });

    System.out.println(d1.deref());
  }

}
