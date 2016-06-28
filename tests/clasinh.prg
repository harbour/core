#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oObject := TAnyClass():New()
   LOCAL oBase := TClassBase():New()

   HB_SYMBOL_UNUSED( oObject )
   HB_SYMBOL_UNUSED( oBase )

   RETURN

CREATE CLASS TClassBase

   METHOD New()
   METHOD Test() INLINE Alert( "Test" )

ENDCLASS

METHOD New() CLASS TClassBase

   RETURN Self

CREATE CLASS TAnyClass INHERIT TClassBase

   METHOD New()

ENDCLASS

METHOD New() CLASS TAnyClass

   ::super:New()
   ::super:Test()

   RETURN Self
