//
// DynObj
//
// Implementation of dynamic objects in Harbour
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oForm := TForm():New()

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   /* Let's add an inline at run-time. Should already be possible */

   ? "Let's add inline 'CalcArea' at run-time to an already instanced class"

   __objAddInline( oForm, "CalcArea", ;
      {| self | ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) } )

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   ? "What is the Form area ?"
   ? oForm:CalcArea()

   ? "Let's add method 'Smile' at run-time to an already instanced class"

   __objAddMethod( oForm, "Smile", @Smile() )

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   ? "Smile please "
   oForm:Smile()

   WAIT

   ? "Data items before"
   ? hb_ValToExp( oForm )

   ? "Let's add an additional data item"

   __objAddData( oForm, "cHelp" )

   oForm:cHelp := "This is a real tricky test"

   ? "Data items after"
   ? hb_ValToExp( oForm )

   WAIT

   ? "Let's attach a bigger smile"

   __objModMethod( oForm, "Smile", @BigSmile() )

   ? "Let's smile"
   oForm:Smile()

   ? "And CalcArea() will now give a result in square inches"

   __objModInline( oForm, "CalcArea", ;
      {| self | ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) / ( 2.54 * 2.54 ) } )

   ? "What is the Form area ?"
   ? oForm:CalcArea()

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   ? "Delete CalcArea"
   __objDelInline( oForm, "CalcArea" )

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   ? "Delete Smile"
   __objDelMethod( oForm, "Smile" )

   ? "What methods are in the class:"
   ? hb_ValToExp( __objGetMethodList( oForm ) )

   WAIT

   ? "Data items before"
   ? hb_ValToExp( oForm )

   ? "Let's delete cHelp"

   __objDelData( oForm, "cHelp" )

   ? "Data items after"
   ? hb_ValToExp( oForm )

#if 0
   oForm:cHelp := "Please crash"
#endif

   RETURN

CREATE CLASS TForm STATIC

   VAR cText

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight

   METHOD New()
   METHOD Show() INLINE ::cText

END CLASS

METHOD New() CLASS TForm

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self

STATIC FUNCTION Smile()

   LOCAL Self := QSelf()

   IF ::CalcArea() == 300
      ? ":-)"
   ELSE
      ? ":-("
   ENDIF

   RETURN self

STATIC FUNCTION BigSmile()

   LOCAL Self := QSelf()

   ? ":-)))"

   RETURN self
