public class Program{

public static void drawCircle(Turtle t, double radius, double x, double y, String color) {
   t.penColor(color);
   t.up(); t.setPosition(x , y + radius); t.down();
		for (int i = 0; i < 360; i++) {
			t.forward(radius * 2 * Math.PI / 360);
			t.right(1);
       }
}
public static void drawSquare(Turtle t, double size, double x, double y, double rotation, String color) {
    t.penColor(color);
	  t.up();
    t.setPosition(x - size/2, y + size/2);
    rotation = rotation % 90;
    double radius = Math.sqrt(2) * size / 2;
    if (rotation > 0 ) t.left(45);
	  for (int i = 0; i < rotation; i++) {
			t.forward(radius * 2 * Math.PI / 360);
			t.right(1);
       }
	  t.down();
    if (rotation > 0) t.right(45);
    int turn = 90;
    t.forward( size ); t.right( turn );
    t.forward( size ); t.right( turn );
    t.forward( size ); t.right( turn );
    t.forward( size ); t.right( turn );
    t.left( rotation );
}
public static void drawTriangle(Turtle t, double size, double x, double y, double rotation, String color) {
    t.penColor(color);
    t.up(); t.setPosition(x - size/2, y + Math.sqrt(3)*size/6);
    rotation = rotation % 120;
    double radius = size / Math.sqrt(3);
    if (rotation > 0) t.left(60);
    for (int i = 0; i < rotation; i++) {
      t.forward(radius*2*Math.PI / 360); t.right(1);
    }
    t.down(); if (rotation > 0) t.right(60); int turn = 120;
    t.forward(size); t.right(turn);
    t.forward(size); t.right(turn);
    t.forward(size); t.right(turn);
    t.left( rotation );
}
  public static void main(String[] args) {

    Turtle t = new Turtle();
    t.hide();
    t.speed(0);
    drawSquare(t,50.,-70.7109126647,70.7104435718,45.,"green");
    drawSquare(t,50.,-99.9999999996,-0.00026535897938,45.,"green");
    drawSquare(t,50.,-70.7105373907,-70.7108188464,45.,"green");
    drawSquare(t,50.,0.000132679489668,-99.9999999999,45.,"green");
    drawSquare(t,50.,70.7107250279,-70.7106312094,45.,"green");
    drawSquare(t,50.,100.,0.,45.,"green");
    drawSquare(t,50.,70.7107250279,70.7106312094,45.,"green");
    drawSquare(t,50.,0.000132679489668,99.9999999999,45.,"green");
try {
  Thread.sleep(3000);
} catch(InterruptedException ex) {
 Thread.currentThread().interrupt();
}
  t.save("Program.jpg");
  }

}
