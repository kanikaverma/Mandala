public class Square {

  public static void main(String[] args) {
    drawSquare(100, 0, 0, 0);
  }

  public static void drawSquare(int length, int angularShift, int x, int y) {
    Turtle t = new Turtle(); t.hide(); t.speed(0);
    t.up(); t.setPosition(x - length / 2, y + length / 2); t.down(); 
    
    t.right(angularShift);

    for (int side = 0; side < 4; side++) {
      t.forward(length);
      t.right(90);
    }
  }

}