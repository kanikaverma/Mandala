public class Expected {

  public static void main(String[] args) {

    Turtle t = new Turtle(); 
    t.hide(); 
    t.speed(0);

    drawCircle(t, 30, 100, 100);
    drawSquare(t, 60, -80, -70, 45);
    drawTriangle(t, 75, 150, -100, 45);

  }

  public static void drawCircle(Turtle t, double radius, double x, double y) {
    t.up(); t.setPosition(x, y+radius); t.down(); 
    for (int i = 0; i < 360; i++) {
      t.forward(radius*2*Math.PI / 360);
      t.right(1);
    }
  }

  public static void drawSquare(Turtle t, double size, double x, double y, double rotation) {
    t.up(); t.setPosition(x - size/2, y + size/2); 
    double radius = Math.sqrt(2) * size / 2; 
    if (rotation > 0) t.left(45);
    for (int i = 0; i < rotation; i++) {
      t.forward(radius*2*Math.PI / 360);
      t.right(1);
    }
    t.down();
    t.right(rotation);
    int turn = 90; 
    t.forward(size);
    t.right(turn);
    t.forward(size);
    t.right(turn);
    t.forward(size);
    t.right(turn);
    t.forward(size);
    t.right(turn);
    t.left(rotation);
    if (rotation > 0) t.left(rotation - 45);
  }

  public static void drawTriangle(Turtle t, double size, double x, double y, double rotation) {
    t.up(); t.setPosition(x - size/2, y - size/2);
    double radius = Math.sqrt(2) * size / 2; 
    if (rotation > 0) t.left(45);
    for (int i = 0; i < rotation; i++) {
      t.forward(radius*2*Math.PI / 360);
      t.right(1);
    }
    t.down(); 
    t.right(rotation);
    int turn = 240; 
    t.forward(size);
    t.right(turn);
    t.forward(size);
    t.right(turn);
    t.forward(size);
    t.right(turn);
    t.left(rotation);
    if (rotation > 0) t.left(rotation - 45);
  }

}