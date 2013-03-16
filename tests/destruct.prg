/*
 * Harbour Project source code:
 *    example/test code for object destructors
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "hbclass.ch"

MEMVAR P

PROCEDURE Main()

   LOCAL bError

   PUBLIC P := NIL

   bError := ErrorBlock( {| oErr | myErrorHandler( oErr ) } )

   ? "First simple tests when object is not destroyed by GC"
   ? "====================================================="
   SIMPLETEST( 0 )
   SIMPLETEST( 1 )
   SIMPLETEST( 2 )
   SIMPLETEST( 3 )

   ?
   ? "Now object will be destroyed by GC"
   ? "=================================="
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
   ? "=> o := myClass():new( " + hb_ntos( type ) + " )"
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
   ? "=> o := myClass():new( " + hb_ntos( type ) + " )"
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

STATIC FUNCTION myErrorHandler( oErr )

   ? "Error ->", hb_ntos( oErr:gencode ), ;
      oErr:description + ":", oErr:operation
   BREAK oErr

   RETURN NIL

CREATE CLASS myClass

   VAR         TYPE
   VAR         var1

   CLASS VAR   var2

   METHOD      INIT
   DESTRUCTOR  dtor

END CLASS

METHOD INIT( type ) CLASS myClass

   ? "Hi, I'm INIT method of class:", self:classname()
   ::type := type

   RETURN self

PROCEDURE DTOR CLASS myClass

   ? "   Hi, I'm desturctor of class: ", self:classname()

   IF ::type == 1
      ? "   I'm storing reference to self in instance variable."
      ? "   Bad practice but safe in Harbour because it will be destroyed."
      ::var1 := self
   ELSEIF ::Type == 2
      ? "   I'm storing reference to self in class variable."
      ? "   It's programmer bug which should cause RT error."
      ::var2 := self
   ELSEIF ::Type == 3
      ? "   I'm storing reference to self in public variable."
      ? "   It's programmer bug which should cause RT error."
      P := self
   ELSE
      ? "   I do not store any references to self."
      ? "   It's a safe destructor."
   ENDIF

   RETURN
