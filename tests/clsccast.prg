/*
 * Harbour Project source code:
 *    demonstration/test code for class variables casting and allocating
 *    in multiinherited classes
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#xtranslate QQOut( [<x,...>] ) => [OutStd( <x> )]
#xtranslate QOut( [<x,...>] ) => OutStd( hb_eol() )[; OutStd( <x> )]

#include "hbclass.ch"

proc main()
   local o := myclass4():new(), i, cbErr

   ? Date(), Time(), Version(), OS()
   ?

   ? "myclass1 class vars:", Str( __cls_CntClsData( o:myclass1:classH ), 3 ), "  => should be:   3"
   ? "myclass2 class vars:", Str( __cls_CntClsData( o:myclass2:classH ), 3 ), "  => should be:   6"
   ? "myclass3 class vars:", Str( __cls_CntClsData( o:myclass3:classH ), 3 ), "  => should be:   9"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "myclass4 class vars:", Str( __cls_CntClsData( o:myclass4:classH ), 3 ), "  => should be:  12"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "myclass4 class vars:", Str( __cls_CntClsData( o:classH ), 3 ), "  => should be:  12"
   ?

   /* direct assignment, possible because the variables have differ names */
   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [0]:"
   ?
   ? "initialization..."
   o:x1 := " X1 "; o:y1 := " Y1 "; o:z1 := " Z1 "
   o:x2 := " X2 "; o:y2 := " Y2 "; o:z2 := " Z2 "
   o:x3 := " X3 "; o:y3 := " Y3 "; o:z3 := " Z3 "
   o:x4 := " X4 "; o:y4 := " Y4 "; o:z4 := " Z4 "

   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: (x1) (y1) (z1)"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: (x1) (y1) (z1)"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: (x2) (y2) (z2)"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: (x1) (y1) (z1)"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: (x2) (y2) (z2)"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: (x3) (y3) (z3)"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be:  X1   Y1   Z1"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be:  X2   Y2   Z2"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be:  X3   Y3   Z3"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be:  X4   Y4   Z4"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be:  X1   Y1   Z1"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [0]:"
   ?

   ? "Setting MYCLASS1 class variables..."
   o:myclass1:x1 := "[X1]"
   o:myclass1:y1 := "[Y1]"
   o:myclass1:z1 := "[Z1]"
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: (x1) (y1) (z1)"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: (x2) (y2) (z2)"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: (x1) (y1) (z1)"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: (x2) (y2) (z2)"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: (x3) (y3) (z3)"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be:  X1   Y1   Z1"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be:  X2   Y2   Z2"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be:  X3   Y3   Z3"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be:  X4   Y4   Z4"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be:  X1   Y1   Z1"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   ? "Setting MYCLASS2 class variables..."
   o:myclass2:x1 := "{X1}"
   o:myclass2:y1 := "{Y1}"
   o:myclass2:z1 := "{Z1}"
   o:myclass2:x2 := "{X2}"
   o:myclass2:y2 := "{Y2}"
   o:myclass2:z2 := "{Z2}"
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: {X1} {Y1} {Z1}"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: {X2} {Y2} {Z2}"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: (x1) (y1) (z1)"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: (x2) (y2) (z2)"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: (x3) (y3) (z3)"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be:  X1   Y1   Z1"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be:  X2   Y2   Z2"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be:  X3   Y3   Z3"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be:  X4   Y4   Z4"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be:  X1   Y1   Z1"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?

   ? "Setting MYCLASS3 class variables..."
   o:myclass3:x1 := "<X1>"
   o:myclass3:y1 := "<Y1>"
   o:myclass3:z1 := "<Z1>"
   o:myclass3:x2 := "<X2>"
   o:myclass3:y2 := "<Y2>"
   o:myclass3:z2 := "<Z2>"
   o:myclass3:x3 := "<X3>"
   o:myclass3:y3 := "<Y3>"
   o:myclass3:z3 := "<Z3>"
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: {X1} {Y1} {Z1}"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: {X2} {Y2} {Z2}"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: <X2> <Y2> <Z2>"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: <X3> <Y3> <Z3>"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be:  X1   Y1   Z1"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be:  X2   Y2   Z2"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be:  X3   Y3   Z3"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be:  X4   Y4   Z4"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be:  X1   Y1   Z1"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be:  X2   Y2   Z2"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be:  X3   Y3   Z3"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be:  X4   Y4   Z4"
   ?




   ? "Setting MYCLASS4 class variables..."
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      o:myclass4:x1 := "|X1|"
      o:myclass4:y1 := "|Y1|"
      o:myclass4:z1 := "|Z1|"
      o:myclass4:x2 := "|X2|"
      o:myclass4:y2 := "|Y2|"
      o:myclass4:z2 := "|Z2|"
      o:myclass4:x3 := "|X3|"
      o:myclass4:y3 := "|Y3|"
      o:myclass4:z3 := "|Z3|"
      o:myclass4:x4 := "|X4|"
      o:myclass4:y4 := "|Y4|"
      o:myclass4:z4 := "|Z4|"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: [X1] [Y1] [Z1]"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: {X1} {Y1} {Z1}"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: {X2} {Y2} {Z2}"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: <X2> <Y2> <Z2>"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: <X3> <Y3> <Z3>"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be: |X1| |Y1| |Z1|"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be: |X2| |Y2| |Z2|"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be: |X3| |Y3| |Z3|"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be: |X4| |Y4| |Z4|"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: |X1| |Y1| |Z1|"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: |X2| |Y2| |Z2|"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: |X3| |Y3| |Z3|"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: |X4| |Y4| |Z4|"
   ?


   ? "Setting MYCLASS3:MYCLASS1 class variables..."
   o:myclass3:myclass1:x1 := "^X1^"
   o:myclass3:myclass1:y1 := "^Y1^"
   o:myclass3:myclass1:z1 := "^Z1^"
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: ^X1^ ^Y1^ ^Z1^"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: {X1} {Y1} {Z1}"
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: {X2} {Y2} {Z2}"
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: <X2> <Y2> <Z2>"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: <X3> <Y3> <Z3>"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be: |X1| |Y1| |Z1|"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be: |X2| |Y2| |Z2|"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be: |X3| |Y3| |Z3|"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be: |X4| |Y4| |Z4|"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: |X1| |Y1| |Z1|"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: |X2| |Y2| |Z2|"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: |X3| |Y3| |Z3|"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: |X4| |Y4| |Z4|"
   ?

   ? "Setting MYCLASS3:MYCLASS2 class variables..."
   o:myclass3:myclass2:x1 := "=X1="
   o:myclass3:myclass2:y1 := "=Y1="
   o:myclass3:myclass2:z1 := "=Z1="
   o:myclass3:myclass2:x2 := "=X2="
   o:myclass3:myclass2:y2 := "=Y2="
   o:myclass3:myclass2:z2 := "=Z2="
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: ^X1^ ^Y1^ ^Z1^"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: =X1= =Y1= =Z1="
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: =X2= =Y2= =Z2="
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: <X1> <Y1> <Z1>"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: <X2> <Y2> <Z2>"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: <X3> <Y3> <Z3>"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be: |X1| |Y1| |Z1|"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be: |X2| |Y2| |Z2|"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be: |X3| |Y3| |Z3|"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be: |X4| |Y4| |Z4|"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: |X1| |Y1| |Z1|"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: |X2| |Y2| |Z2|"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: |X3| |Y3| |Z3|"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: |X4| |Y4| |Z4|"
   ?

   ? "Setting SUPER class variables..."
   o:super:x1 := "*X1*"
   o:super:y1 := "*Y1*"
   o:super:z1 := "*Z1*"
   o:super:x2 := "*X2*"
   o:super:y2 := "*Y2*"
   o:super:z2 := "*Z2*"
   o:super:x3 := "*X3*"
   o:super:y3 := "*Y3*"
   o:super:z3 := "*Z3*"
   ? "[1] MYCLASS1 VARS:", o:myclass1:x1, o:myclass1:y1, o:myclass1:z1, "  => should be: ^X1^ ^Y1^ ^Z1^"
   ? "[2] MYCLASS1 VARS:", o:myclass2:x1, o:myclass2:y1, o:myclass2:z1, "  => should be: =X1= =Y1= =Z1="
   ? "[2] MYCLASS2 VARS:", o:myclass2:x2, o:myclass2:y2, o:myclass2:z2, "  => should be: =X2= =Y2= =Z2="
   ? "[3] MYCLASS1 VARS:", o:myclass3:x1, o:myclass3:y1, o:myclass3:z1, "  => should be: *X1* *Y1* *Z1*"
   ? "[3] MYCLASS2 VARS:", o:myclass3:x2, o:myclass3:y2, o:myclass3:z2, "  => should be: *X2* *Y2* *Z2*"
   ? "[3] MYCLASS3 VARS:", o:myclass3:x3, o:myclass3:y3, o:myclass3:z3, "  => should be: *X3* *Y3* *Z3*"
   cbErr := ErrorBlock( {| oErr | Break( oErr ) } )
   begin sequence
      ? "[4] MYCLASS1 VARS:", o:myclass4:x1, o:myclass4:y1, o:myclass4:z1, "  => should be: |X1| |Y1| |Z1|"
      ? "[4] MYCLASS2 VARS:", o:myclass4:x2, o:myclass4:y2, o:myclass4:z2, "  => should be: |X2| |Y2| |Z2|"
      ? "[4] MYCLASS3 VARS:", o:myclass4:x3, o:myclass4:y3, o:myclass4:z3, "  => should be: |X3| |Y3| |Z3|"
      ? "[4] MYCLASS4 VARS:", o:myclass4:x4, o:myclass4:y4, o:myclass4:z4, "  => should be: |X4| |Y4| |Z4|"
   recover
      ? "ERROR: no selfclass casting"
   end sequence
   ErrorBlock( cbErr )
   ? "    MYCLASS1 VARS:", o:x1, o:y1, o:z1, "  => should be: |X1| |Y1| |Z1|"
   ? "    MYCLASS2 VARS:", o:x2, o:y2, o:z2, "  => should be: |X2| |Y2| |Z2|"
   ? "    MYCLASS3 VARS:", o:x3, o:y3, o:z3, "  => should be: |X3| |Y3| |Z3|"
   ? "    MYCLASS4 VARS:", o:x4, o:y4, o:z4, "  => should be: |X4| |Y4| |Z4|"
   ?


   ? "instance variables [" + hb_ntos( Len( o ) ) + "]:"; ?
   for i := 1 to Len( o ); ?? "", o[ i ]; next
   ? " => shoule be [0]:"
   ?

   return


create class myclass1
export:
   class var x1 init "(x1)"
   class var y1 init "(y1)"
   class var z1 init "(z1)"
endclass

create class myclass2 from myclass1
export:
   class var x2 init "(x2)"
   class var y2 init "(y2)"
   class var z2 init "(z2)"
endclass

create class myclass3 from myclass1, myclass2
export:
   class var x3 init "(x3)"
   class var y3 init "(y3)"
   class var z3 init "(z3)"
endclass

create class myclass4 from myclass3, myclass2
export:
   class var x4 init "(x4)"
   class var y4 init "(y4)"
   class var z4 init "(z4)"
endclass
