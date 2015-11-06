open Graphics;;

open graph " 640x480";;

draw_circle 250 250 20;
draw_circle 350 250 20;
draw_circle 300 200 120;
draw_arc 300 200 60 60 180 360;

read_line ();;

(*COMPILE WITH:
ocamlc graphics.cma graphicsTest.ml -o graphicsTest
*)
