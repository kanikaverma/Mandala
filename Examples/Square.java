public class Square {

  public static void main(String[] args) {
    Turtle t = new Turtle();
    t.hide();
    t.speed(0);

  int size = 100; //logical units
  int turn = 90; //in degree

  t.right(45);
  //draw a square
  t.forward( size );
  t.right( turn );

  t.forward( size );
  t.right( turn );

  t.forward( size );
  t.right( turn );

  t.forward( size );
  t.right( turn );

  }

}