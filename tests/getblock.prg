/*
 * test code for GET SetGet block for aliased macro variables
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 */

PROCEDURE Main()

   LOCAL GetList := {}, aStr
   MEMVAR idx, fld, als
   PRIVATE idx, fld, als

   aStr := { { "F1", "C", 10, 0 }, ;
             { "F2", "C", 10, 0 } }

   dbCreate( "_tst1", aStr )
   USE _tst1 NEW
   dbAppend()
   field->F1 := "FIRST"
   field->F2 := "SECOND"

   dbCreate( "_tst2", aStr )
   USE _tst2 NEW
   dbAppend()
   field->F1 := "first"
   field->F2 := "second"
   Select( 1 )

   CLS

   idx := "1"
   fld := "F" + idx
   als := "_tst" + idx

   @  1, 0 GET f&idx
   @  2, 0 GET field->f&idx
   @  3, 0 GET _tst1->f&idx
   @  4, 0 GET &als->f&idx
   @  5, 0 GET _tst&idx->f&idx

   @  7, 0 GET &fld
   @  8, 0 GET field->&fld
   @  9, 0 GET _tst1->&fld
   @ 10, 0 GET &als->&fld
   @ 11, 0 GET _tst&idx->&fld

   @ 13, 0 GET &als->F1
   @ 14, 0 GET _tst&idx->F1

   InKey( 0 )

   idx := "2"
   fld := "F" + idx
   als := "_tst" + idx

   READ

RETURN
