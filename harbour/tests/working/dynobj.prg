//
// DynObj
//
// Implementation of dynamic objects in Harbour
//
// Date : 1999/05/12
//
#define DATA_SYMBOL 1
#define DATA_VAL    2

function Main()

   local oForm := TForm():New()
   local nSeq

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

/* Let's add an inline at run-time. Should already be possible */

   QOut( "Let's add inline 'CalcArea' at run-time to an already instanced class" )

   oAddInline( oForm, "CalcArea", ;
      {|self| ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) } )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "What is the Form area ?" )
   QOut( oForm:CalcArea() )

   QOut( "Let's add method 'Smile' at run-time to an already instanced class" )

   oAddMethod( oForm, "Smile", @Smile() )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "Smile please " )
   oForm:Smile()

   Pause()

   QOut( "Data items before" )
   HBDebug( oForm )

   QOut( "Let's add an additional data item" )

   oAddData( oForm, "cHelp" )
   
   oForm:cHelp := "This is a real tricky test"

   QOut( "Data items after" )
   HBDebug( oForm )

   Pause()

   QOut( "Let's attach a bigger smile" )

   oModMethod( oForm, "Smile", @BigSmile() )

   QOut( "Let's smile" )
   oForm:Smile()

   QOut( "And CalcArea() will now give a result in square inches" )

   oModInline( oForm, "CalcArea", ;
      {|self| ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) / (2.54*2.54) } )

   QOut( "What is the Form area ?" )
   QOut( oForm:CalcArea() )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "Delete CalcArea" )
   oDelInline( oForm, "CalcArea" )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "Delete Smile" )
   oDelMethod( oForm, "Smile" )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   Pause()

   QOut( "Data items before" )
   HBDebug( oForm )

   QOut( "Let's delete cHelp" )

   oDelData( oForm, "cHelp" )
   
   QOut( "Data items after" )
   HBDebug( oForm )

/*   oForm:cHelp := "Please crash" */

return nil


function TForm()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cText" )           // define this class objects datas
      oClass:AddData( "nTop" )
      oClass:AddData( "nLeft" )
      oClass:AddData( "nBottom" )
      oClass:AddData( "nRight" )

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddInline( "Show", {|self| ::cText } )

      oClass:Create()                     // builds this class
   endif

return oClass:Instance()                  // builds an object of this class


static function New()

   local Self := QSelf()

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

return Self


static function Smile()

   local self := QSelf()

   if ::CalcArea() == 300
      QOut( ":-)" )
   else
      QOut( ":-(" )
   endif
return self


static function BigSmile()

   local self := QSelf()

   QOut( ":-)))" )
return self


function Pause()

   __Accept( "Pause :" )
return nil



