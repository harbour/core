/*
 * Example/test code for object destructors
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#include "hbclass.ch"

MEMVAR P

PROCEDURE Main()

   LOCAL bError := ErrorBlock( {| oErr | myErrorHandler( oErr ) } )

   PUBLIC P := NIL

   ? "First simple tests when object is not destroyed by GC", "---"
   ?
   SIMPLETEST( 0 )
   SIMPLETEST( 1 )
   SIMPLETEST( 2 )
   SIMPLETEST( 3 )

   ?
   ? "Now object will be destroyed by GC", "---"
   ?
   GCFREETEST( 0 )
   GCFREETEST( 1 )
   GCFREETEST( 2 )
   GCFREETEST( 3 )

   ErrorBlock( bError )

   ?
   ? "*** END OF TEST ***"

   RETURN

STATIC PROCEDURE SIMPLETEST( type )

   LOCAL o

   ?
   ? "=> o := myClass():new(", hb_ntos( type ), ")"
   o := myClass():new( type )
   ? "=> o:className() ->", o:className()
   ? "=> o := NIL"
   BEGIN SEQUENCE
      o := NIL
   END SEQUENCE

   RETURN

STATIC PROCEDURE GCFREETEST( type )

   LOCAL o, a

   ?
   ? "=> o := myClass():new(", hb_ntos( type ), ")"
   o := myClass():new( type )
   ? "=> o:className() ->", o:className()
   ? "=> create corss reference: a := { o, NIL }; a[ 2 ] := a; a := NIL"
   a := { o, NIL }; a[ 2 ] := a; a := NIL
   ? "=> o := NIL"
   BEGIN SEQUENCE
      o := NIL
   END SEQUENCE
   ? "=> hb_gcAll()"
   BEGIN SEQUENCE
      hb_gcAll()
   END SEQUENCE

   RETURN

STATIC PROCEDURE myErrorHandler( oErr )

   ? "Error ->", hb_ntos( oErr:gencode ), oErr:description + ":", oErr:operation
   BREAK oErr

   RETURN

CREATE CLASS myClass

   VAR         type
   VAR         var1

   CLASS VAR   var2

   METHOD      init()
   DESTRUCTOR  dtor()

ENDCLASS

METHOD init( type ) CLASS myClass

   ? "Hi, I'm INIT method of class:", ::classname()
   ::type := type

   RETURN self

PROCEDURE dtor() CLASS myClass

   ? "   Hi, I'm desturctor of class:", ::classname()

   SWITCH ::type
   CASE 1
      ? "   I'm storing reference to self in instance variable."
      ? "   Bad practice but safe in Harbour because it will be destroyed."
      ::var1 := self
      EXIT
   CASE 2
      ? "   I'm storing reference to self in class variable."
      ? "   It's programmer bug which should cause RT error."
      ::var2 := self
      EXIT
   CASE 3
      ? "   I'm storing reference to self in public variable."
      ? "   It's programmer bug which should cause RT error."
      P := self
      EXIT
   OTHERWISE
      ? "   I do not store any references to self."
      ? "   It's a safe destructor."
   ENDSWITCH

   RETURN
