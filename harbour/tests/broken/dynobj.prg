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

   QOut( "The next code can _not_ be used in the offical classes.c" )
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


//
// <oObj> := oAddMethod( <oObj>, <cSymbol>, <nFuncPtr> )
//
// Add a method to an already existing class
//
function oAddMethod( oObj, cSymbol, nFuncPtr )

   if IsMessage( oObj, cSymbol )
      QOut( "OADDMETHOD: ", cSymbol, " already exists in class." )
   elseif ValType( nFuncPtr ) != "N"
      QOut( "OADDMETHOD: Argument type error <nFuncPtr>" )
   elseif ValType( oObj ) != "O"
      QOut( "OADDMETHOD: Argument type error <oObj>" )
   else
      ClassAdd( oObj:ClassH, cSymbol, nFuncPtr, MET_METHOD )
   endif
return oObj


//
// <oObj> := oAddInline( <oObj>, <cSymbol>, <bInline> )
//
// Add an INLINE to an already existing class
//
function oAddInline( oObj, cSymbol, bInline )

   if IsMessage( oObj, cSymbol )
      QOut( "OADDINLINE: ", cSymbol, " already exists in class." )
   elseif ValType( bInline ) != "B"
      QOut( "OADDINLINE: Argument type error <bInline>" )
   elseif ValType( oObj ) != "O"
      QOut( "OADDINLINE: Argument type error <oObj>" )
   else
      ClassAdd( oObj:ClassH, cSymbol, bInline, MET_INLINE )
   endif
return oObj

//
// <oObj> := oAddData( <oObj>, <cSymbol> )
//
// Add a DATA to an already existing class
//
function oAddData( oObj, cSymbol )

   local nSeq

   if IsMessage( oObj, cSymbol ) .or. IsMessage( oObj, "_" + cSymbol )
      QOut( "OADDDATA: ", cSymbol, " already exists in class." )
   elseif ValType( oObj ) != "O"
      QOut( "OADDDATA: Argument type error <oObj>" )
   else
      nSeq := __wDataInc( oObj:ClassH )         // Allocate new Seq#
      ClassAdd( oObj:ClassH, cSymbol,       nSeq, MET_DATA )
      ClassAdd( oObj:ClassH, "_" + cSymbol, nSeq, MET_DATA )
   endif
return oObj

//
// <oObj> := oModMethod( <oObj>, <cSymbol>, <nFuncPtr> )
//
// Modify a method to an already existing class
//
function oModMethod( oObj, cSymbol, nFuncPtr )

   if !IsMethod( oObj, cSymbol )
      QOut( "OMODMETHOD: ", cSymbol, " doesnot exists in class." )
   elseif ValType( nFuncPtr ) != "N"
      QOut( "OMODMETHOD: Argument type error <nFuncPtr>" )
   elseif ValType( oObj ) != "O"
      QOut( "OMODMETHOD: Argument type error <oObj>" )
   else
      ClassMod( oObj:ClassH, cSymbol, nFuncPtr )
   endif
return oObj


//
// <oObj> := oModInline( <oObj>, <cSymbol>, <bInline> )
//
// Modify an INLINE to an already existing class
//
function oModInline( oObj, cSymbol, bInline )

   if !IsMethod( oObj, cSymbol )
      QOut( "OMODINLINE: ", cSymbol, " doesnot exists in class." )
   elseif ValType( bInline ) != "B"
      QOut( "OMODINLINE: Argument type error <bInline>" )
   elseif ValType( oObj ) != "O"
      QOut( "OMODINLINE: Argument type error <oObj>" )
   else
      ClassMod( oObj:ClassH, cSymbol, bInline )
   endif
return oObj


//
// <oObj> := oDelMethod( <oObj>, <cSymbol> )
//
// Delete a method from an already existing class
//
function oDelMethod( oObj, cSymbol )

   if !IsMethod( oObj, cSymbol )
      QOut( "ODELMETHOD: ", cSymbol, " doesnot exists in class." )
   elseif ValType( oObj ) != "O"
      QOut( "ODELMETHOD: Argument type error <oObj>" )
   else
      ClassDel( oObj:ClassH, cSymbol )
   endif
return oObj

function oDelInline( oObj, cSymbol )
return oDelMethod( oObj, cSymbol )              // Same story


//
// <oObj> := oDelData( <oObj>, <cSymbol> )
//
// Delete a DATA from an already existing class
//
function oDelData( oObj, cSymbol )

   local nSeq

   if !IsData( oObj, cSymbol )
      QOut( "ODELDATA: ", cSymbol, " doesnot exists in class." )
   elseif ValType( oObj ) != "O"
      QOut( "ODELDATA: Argument type error <oObj>" )
   else
      ClassDel( oObj:ClassH, cSymbol,      )
      ClassDel( oObj:ClassH, "_" + cSymbol )
      nSeq := __wDataDec( oObj:ClassH )         // Decrease wData
   endif
return oObj


