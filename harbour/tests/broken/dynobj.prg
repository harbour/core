#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

//
// DynObj
//
// Implementation of dynamic objects in Harbour
//
// Warning : Test version only. Definitively not ready for use.
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

   ClassAdd( oForm:ClassH, "CalcArea", ;
      {|self| ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) }, MET_INLINE )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "What is the Form area ?" )
   QOut( oForm:CalcArea() )

   QOut( "Let's add method 'Smile' at run-time to an already instanced class" )

   ClassAdd( oForm:ClassH, "Smile", @Smile(), MET_METHOD )

   QOut( "What methods are in the class :" )
   HBDebug( aoMethod( oForm ) )

   QOut( "Smile please " )
   oForm:Smile()

   QOut( "The next code can _not_ be used in the offical classes.c" )
   Pause()

   QOut( "Data items before" )
   HBDebug( oForm )

   QOut( "Let's add an additional data item" )

   nSeq := __wDataInc( oForm:ClassH )           // Allocate new Seq#
   QOut( "New Seq# ", nSeq )
   ClassAdd( oForm:ClassH, "cHelp" , nSeq, MET_DATA )
   ClassAdd( oForm:ClassH, "_cHelp", nSeq, MET_DATA )
   
   oForm:cHelp := "This is a real tricky test"

   QOut( "Data items after" )
   HBDebug( oForm )

   QOut( "Let's attach a bigger smile" )

   ClassMod( oForm:ClassH, "Smile", @BigSmile() )

   QOut( "Let's smile" )
   oForm:Smile()

/*   QOut( "Let's crash" )
   ClassMod( oForm:ClassH, "cText", 24 )
   ClassMod( oForm:ClassH, "_cText", 24 ) */

   QOut( "And CalcArea() will now give a result in square inches" )

   ClassAdd( oForm:ClassH, "CalcArea", ;
      {|self| ( ::nRight  - ::nLeft ) * ( ::nBottom - ::nTop ) / (2.54*2.54) },;
      MET_INLINE )

   QOut( "What is the Form area ?" )
   QOut( oForm:CalcArea() )

   QOut( "Just one left : Delete" )
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
