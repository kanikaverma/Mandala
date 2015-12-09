public class Program{

public static void drawCircle(Turtle t, double radius, double x, double y) {
   t.up(); t.setPosition(x , y + radius); t.down();
		for (int i = 0; i < 360; i++) {
			t.forward(radius * 2 * Math.PI / 360);
			t.right(1);
       }
}
public static void drawSquare(Turtle t, double size, double x, double y, double rotation) {
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
public static void drawTriangle(Turtle t, double size, double x, double y, double rotation) {
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
    drawCircle(t,25.,-0.000398038469047,-99.9999999992);
    drawCircle(t,25.,-70.7109126647,-70.7104435718);
    drawCircle(t,25.,-99.9999999996,0.00026535897938);
    drawCircle(t,25.,-70.7105373907,70.7108188464);
    drawCircle(t,25.,0.000132679489668,99.9999999999);
    drawCircle(t,25.,70.7107250279,70.7106312094);
    drawCircle(t,25.,100.,-0.);
    drawCircle(t,25.,70.7107250279,-70.7106312094);
    drawCircle(t,25.,0.000132679489668,-99.9999999999);
    drawSquare(t,50.,-0.000398038469047,-99.9999999992,90.);
    drawSquare(t,50.,-99.9999999996,0.00026535897938,90.);
    drawSquare(t,50.,0.000132679489668,99.9999999999,90.);
    drawSquare(t,50.,100.,-0.,90.);
    drawSquare(t,50.,0.000132679489668,-99.9999999999,90.);
    drawSquare(t,50.,-0.000398038469047,-99.9999999992,45.);
    drawSquare(t,50.,-99.9999999996,0.00026535897938,45.);
    drawSquare(t,50.,0.000132679489668,99.9999999999,45.);
    drawSquare(t,50.,100.,-0.,45.);
    drawSquare(t,50.,0.000132679489668,-99.9999999999,45.);
    drawCircle(t,25.,-66.970190579,-161.678673837);
    drawCircle(t,25.,-161.679118112,-66.9691180054);
    drawCircle(t,25.,-161.678762693,66.9699760645);
    drawCircle(t,25.,-66.9693325203,161.679029258);
    drawCircle(t,25.,66.9697615499,161.678851548);
    drawCircle(t,25.,161.678940403,66.9695470352);
    drawCircle(t,25.,161.678940403,-66.9695470352);
    drawCircle(t,25.,66.9697615499,-161.678851548);
    drawCircle(t,25.,-66.9693325203,-161.679029258);
    drawSquare(t,50.,-0.000398038469047,-99.9999999992,30.);
    drawSquare(t,50.,-99.9999999996,0.00026535897938,30.);
    drawSquare(t,50.,0.000132679489668,99.9999999999,30.);
    drawSquare(t,50.,100.,-0.,30.);
    drawSquare(t,50.,0.000132679489668,-99.9999999999,30.);
    drawSquare(t,50.,-0.000398038469047,-99.9999999992,60.);
    drawSquare(t,50.,-99.9999999996,0.00026535897938,60.);
    drawSquare(t,50.,0.000132679489668,99.9999999999,60.);
    drawSquare(t,50.,100.,-0.,60.);
    drawSquare(t,50.,0.000132679489668,-99.9999999999,60.);
    drawTriangle(t,35.,-0.000696567320832,-174.999999999,0.);
    drawTriangle(t,35.,-123.744097163,-123.743276251,0.);
    drawTriangle(t,35.,-174.999999999,0.000464378213914,0.);
    drawTriangle(t,35.,-123.743440434,123.743932981,0.);
    drawTriangle(t,35.,0.000232189106919,175.,0.);
    drawTriangle(t,35.,123.743768799,123.743604616,0.);
    drawTriangle(t,35.,175.,-0.,0.);
    drawTriangle(t,35.,123.743768799,-123.743604616,0.);
    drawTriangle(t,35.,0.000232189106919,-175.,0.);
    drawTriangle(t,35.,-0.000696567320832,-174.999999999,180.);
    drawTriangle(t,35.,-123.744097163,-123.743276251,180.);
    drawTriangle(t,35.,-174.999999999,0.000464378213914,180.);
    drawTriangle(t,35.,-123.743440434,123.743932981,180.);
    drawTriangle(t,35.,0.000232189106919,175.,180.);
    drawTriangle(t,35.,123.743768799,123.743604616,180.);
    drawTriangle(t,35.,175.,-0.,180.);
    drawTriangle(t,35.,123.743768799,-123.743604616,180.);
    drawTriangle(t,35.,0.000232189106919,-175.,180.);
  t.save("Program.jpg");
  }

}
