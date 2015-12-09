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
    drawCircle(t,25.,0.,0.,"yellow");
    drawSquare(t,50.,-70.7109126647,70.7104435718,315.,"red");
    drawSquare(t,50.,-99.9999999996,-0.00026535897938,270.,"red");
    drawSquare(t,50.,-70.7105373907,-70.7108188464,225.,"red");
    drawSquare(t,50.,0.000132679489668,-99.9999999999,180.,"red");
    drawSquare(t,50.,70.7107250279,-70.7106312094,135.,"red");
    drawSquare(t,50.,100.,0.,90.,"red");
    drawSquare(t,50.,70.7107250279,70.7106312094,45.,"red");
    drawSquare(t,50.,0.000132679489668,99.9999999999,0.,"red");
    drawSquare(t,50.,-99.9999999996,-0.00026535897938,45.,"blue");
    drawSquare(t,50.,0.000132679489668,-99.9999999999,45.,"blue");
    drawSquare(t,50.,100.,0.,45.,"blue");
    drawSquare(t,50.,0.000132679489668,99.9999999999,45.,"blue");
    drawTriangle(t,35.,-123.744097163,123.743276251,315.,"green");
    drawTriangle(t,35.,-174.999999999,-0.000464378213914,270.,"green");
    drawTriangle(t,35.,-123.743440434,-123.743932981,225.,"green");
    drawTriangle(t,35.,0.000232189106919,-175.,180.,"green");
    drawTriangle(t,35.,123.743768799,-123.743604616,135.,"green");
    drawTriangle(t,35.,175.,0.,90.,"green");
    drawTriangle(t,35.,123.743768799,123.743604616,45.,"green");
    drawTriangle(t,35.,0.000232189106919,175.,0.,"green");
  t.save("Program.jpg");
  }

}
