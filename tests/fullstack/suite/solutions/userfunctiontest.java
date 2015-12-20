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
    drawTriangle(t,-150.,58.779018992,-80.9013406956,504.,"red");
    drawTriangle(t,-150.,95.105823829,-30.9011694571,468.,"red");
    drawTriangle(t,-150.,95.1054958275,30.9021789427,432.,"red");
    drawTriangle(t,-150.,58.7781602728,80.9019645926,396.,"red");
    drawTriangle(t,-150.,-0.000398038469047,99.9999999992,360.,"red");
    drawTriangle(t,-150.,-58.7788043128,80.9014966707,324.,"red");
    drawTriangle(t,-150.,-95.1057418296,30.9014218288,288.,"red");
    drawTriangle(t,-150.,-95.1055778289,-30.9019265717,252.,"red");
    drawTriangle(t,-150.,-58.7783749532,-80.9018086192,216.,"red");
    drawTriangle(t,-150.,0.000132679489668,-99.9999999999,180.,"red");
    drawTriangle(t,-150.,-5.87788043128,8.09014966707,0.,"red");
    drawTriangle(t,-150.,-9.51057418296,3.09014218288,0.,"red");
    drawTriangle(t,-150.,-9.51055778289,-3.09019265717,0.,"red");
    drawTriangle(t,-150.,-5.87783749532,-8.09018086192,0.,"red");
    drawTriangle(t,-150.,1.32679489668e-05,-9.99999999999,0.,"red");
    drawTriangle(t,-150.,5.87785896332,-8.09016526452,0.,"red");
    drawTriangle(t,-150.,9.51056598296,-3.09016742004,0.,"red");
    drawTriangle(t,-150.,9.51056598296,3.09016742004,0.,"red");
    drawTriangle(t,-150.,5.87785896332,8.09016526452,0.,"red");
    drawTriangle(t,-150.,1.32679489668e-05,9.99999999999,0.,"red");
    drawTriangle(t,-150.,-5.87788043128,8.09014966707,0.,"red");
    drawTriangle(t,-150.,-9.51057418296,3.09014218288,0.,"red");
    drawTriangle(t,-150.,-9.51055778289,-3.09019265717,0.,"red");
    drawTriangle(t,-150.,-5.87783749532,-8.09018086192,0.,"red");
    drawTriangle(t,-150.,1.32679489668e-05,-9.99999999999,0.,"red");
    drawTriangle(t,-150.,5.87785896332,-8.09016526452,0.,"red");
    drawTriangle(t,-150.,9.51056598296,-3.09016742004,0.,"red");
    drawTriangle(t,-150.,9.51056598296,3.09016742004,0.,"red");
    drawTriangle(t,-150.,5.87785896332,8.09016526452,0.,"red");
    drawTriangle(t,-150.,1.32679489668e-05,9.99999999999,0.,"red");
try {
  Thread.sleep(3000);
} catch(InterruptedException ex) {
 Thread.currentThread().interrupt();
}
  t.save("Program.jpg");
  }

}
