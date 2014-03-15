/*
 * Harbour Project source code:
 *    demonstration/test code for instance variables casting and allocating
 *    in multiinherited classes
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#xtranslate QQOut( [<x,...>] ) => [OutStd( <x> )]
#xtranslate QOut( [<x,...>] ) => OutStd( hb_eol() )[; OutStd( <x> )]

#include "hbclass.ch"

procedure main()

   local o := myclass4():new(), i, cbErr

   ? Date(), Time(), Version(), OS()
   ?

   /* direct assignment, possible because the variables have differ names */
   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [12]:"
   ? " (x1) (y1) (z1) (x2) (y2) (z2) (x3) (y3) (z3) (x4) (y4) (z4)"
   ?
   ? "initialization..."
   o:x1 := " X1 "; o:y1 := " Y1 "; o:z1 := " Z1 "
   o:x2 := " X2 "; o:y2 := " Y2 "; o:z2 := " Z2 "
   o:x3 := " X3 "; o:y3 := " Y3 "; o:z3 := " Z3 "
   o:x4 := " X4 "; o:y4 := " Y4 "; o:z4 := " Z4 "

   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be:  X1   Y1   Z1"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?
   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [12]:"
   ? "  X1   Y1   Z1   X2   Y2   Z2   X3   Y3   Z3   X4   Y4   Z4"
   ?

   ? "Setting MYCLASS1 instance variables..."
   o:myclass1:x1 := "[X1]"
   o:myclass1:y1 := "[Y1]"
   o:myclass1:z1 := "[Z1]"
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   ? "Setting MYCLASS2 instance variables..."
   o:myclass2:x2 := "[X2]"
   o:myclass2:y2 := "[Y2]"
   o:myclass2:z2 := "[Z2]"
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: [X2] [Y2] [Z2]"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   ? "Setting MYCLASS3 instance variables..."
   o:myclass3:x3 := "[X3]"
   o:myclass3:y3 := "[Y3]"
   o:myclass3:z3 := "[Z3]"
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: [X2] [Y2] [Z2]"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: [X3] [Y3] [Z3]"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "Setting MYCLASS4 instance variables..."
      o:myclass4:x4 := "[X4]"
      o:myclass4:y4 := "[Y4]"
      o:myclass4:z4 := "[Z4]"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: [X2] [Y2] [Z2]"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: [X3] [Y3] [Z3]"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: [X4] [Y4] [Z4]"
   ?

   ? "Setting MYCLASS3:MYCLASS1 instance variables..."
   o:myclass3:myclass1:x1 := "<X1>"
   o:myclass3:myclass1:y1 := "<Y1>"
   o:myclass3:myclass1:z1 := "<Z1>"
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: [X2] [Y2] [Z2]"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: [X3] [Y3] [Z3]"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: [X4] [Y4] [Z4]"
   ?

   ? "Setting MYCLASS3:MYCLASS2 instance variables..."
   o:myclass3:myclass2:x2 := "<X2>"
   o:myclass3:myclass2:y2 := "<Y2>"
   o:myclass3:myclass2:z2 := "<Z2>"
   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: <X2> <Y2> <Z2>"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: [X3] [Y3] [Z3]"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: [X4] [Y4] [Z4]"
   ?

   ? "Setting SUPER instance variables..."
   o:super:x1 := "{X1}"
   o:super:y1 := "{Y1}"
   o:super:z1 := "{Z1}"
   o:super:x2 := "{X2}"
   o:super:y2 := "{Y2}"
   o:super:z2 := "{Z2}"
   o:super:x3 := "{X3}"
   o:super:y3 := "{Y3}"
   o:super:z3 := "{Z3}"

   ? "MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: {X1} {Y1} {Z1}"
   ? "MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: {X2} {Y2} {Z2}"
   ? "MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: {X3} {Y3} {Z3}"
   ? "MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: [X4] [Y4] [Z4]"
   ?
   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [12]:"
   ? " {X1} {Y1} {Z1} {X2} {Y2} {Z2} {X3} {Y3} {Z3} [X4] [Y4] [Z4]"
   ?

   return


create class myclass1
export:
    var x1 init "(x1)"
    var y1 init "(y1)"
    var z1 init "(z1)"
endclass

create class myclass2 from myclass1
export:
    var x2 init "(x2)"
    var y2 init "(y2)"
    var z2 init "(z2)"
endclass

create class myclass3 from myclass1, myclass2
export:
    var x3 init "(x3)"
    var y3 init "(y3)"
    var z3 init "(z3)"
endclass

create class myclass4 from myclass3, myclass2
export:
    var x4 init "(x4)"
    var y4 init "(y4)"
    var z4 init "(z4)"
endclass
