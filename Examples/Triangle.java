public class Triangle {

  public static void main(String[] args) {
    drawTriangle(100, 0, 0, 0);
  }

  public static void drawTriangle(int length, int angularShift, int x, int y) {
    Turtle t = new Turtle(); t.hide(); t.speed(0);
    t.up(); t.setPosition(x - length / 2, y); t.down(); 

    for (int side = 0; side < 3; side++) {
      t.forward(length);
      t.right(240);
    } 
    t.save("triangle.jpg");
  }

}