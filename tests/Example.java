public class Example {

  final static int DEGREES = 360; 

  public static void main(String[] args) {
    drawDot(0, 0);
    drawCircle(50, 0, 0);
    drawCircle(100, 0, 0);
    drawCircle(200, 0, 0);
  }

  public static void drawCircle(int radius, int x, int y) {
    Turtle t = new Turtle();
    t.speed(5); // change speed to 0 to turn off animation 
    t.up();
    t.setPosition(x, y + radius);
    t.down();
    for (int i = 0; i < DEGREES; i++) {
      t.forward(radius * 2 * Math.PI / DEGREES);
      t.right(1);
    }
  }

  public static void drawDot(int x, int y) {
    Turtle t = new Turtle();
    t.setPosition(x, y);
    t.dot();
  }

}