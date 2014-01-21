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

   Pause()

   ? "Data items before"
   ? hb_ValToExp( oForm )

   ? "Let's add an additional data item"

   __objAddData( oForm, "cHelp" )

   oForm:cHelp := "This is a real tricky test"

   ? "Data items after"
   ? hb_ValToExp( oForm )

   Pause()

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

   Pause()

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

STATIC FUNCTION TForm()

   STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := HBClass():New( "TForm" )    // starts a new class definition

      s_oClass:AddData( "cText" )           // define this class objects datas
      s_oClass:AddData( "nTop" )
      s_oClass:AddData( "nLeft" )
      s_oClass:AddData( "nBottom" )
      s_oClass:AddData( "nRight" )

      s_oClass:AddMethod( "New",  @New() )  // define this class objects methods
      s_oClass:AddInline( "Show", {| self | ::cText } )

      s_oClass:Create()                     // builds this class
   ENDIF

   RETURN s_oClass:Instance()                  // builds an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self

STATIC FUNCTION Smile()

   LOCAL self := QSelf()

   IF ::CalcArea() == 300
      ? ":-)"
   ELSE
      ? ":-("
   ENDIF

   RETURN self

STATIC FUNCTION BigSmile()

   LOCAL self := QSelf()

   ? ":-)))"

   RETURN self

STATIC PROCEDURE Pause()

   WAIT

   RETURN
