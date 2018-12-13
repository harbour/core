#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := Three():New()

   o:CheckIt()

   RETURN

CREATE CLASS One

   METHOD New() INLINE Self
   METHOD Test() INLINE QOut( "One" )
   METHOD CheckIt() INLINE ::Test()

ENDCLASS

CREATE CLASS Two FROM One

   METHOD Test() INLINE ::super:Test()
   METHOD CheckIt() INLINE ::super:CheckIt()

ENDCLASS

CREATE CLASS Three FROM Two

   METHOD Test() INLINE QOut( "Three" )

ENDCLASS
