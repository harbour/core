// Testing Harbour hbclass.ch commands

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := TTest():New( "one", "two" )

   ? o:ClassName()
   ? o:One
   ? o:Two

   o:Test()

   RETURN

//

CREATE CLASS TTest INHERIT TParent

   VAR One, Two, Three

   METHOD New( One, Two )

   METHOD Test() INLINE QOut( "Hello" )

ENDCLASS

METHOD New( One, Two ) CLASS TTest

   ::super:New()

   ::One := One
   ::Two := Two

   RETURN Self

//

CREATE CLASS TParent

   VAR One

   METHOD New()

ENDCLASS

METHOD New() CLASS TParent

   ? "TParent:New()"

   RETURN Self
