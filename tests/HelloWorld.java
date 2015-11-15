public class HelloWorld {

  public static void main(String[] args) {
    drawDot(0, 0);
  }

  public static void drawDot(int x, int y) {
    Turtle t = new Turtle();
    t.hide();
    t.setPosition(x, y);
    t.dot();
  }

}