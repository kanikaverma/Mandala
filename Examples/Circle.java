public class Circle {

  public static void main(String[] args) {
    drawCircle(100, 0, 0);
  }

  public static void drawCircle(int radius, int x, int y) {
    Turtle t = new Turtle(); t.hide(); t.speed(0); 
    t.up(); t.setPosition(x, y + radius); t.down();
    for (int i = 0; i < 360; i++) {
      t.forward(radius * 2 * Math.PI / 360);
      t.right(1);
    }
    t.save("circle.jpg");
  }

}