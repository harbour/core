/*
 * $Id$
 */

/*
 * $Doc$
 * $Description$  Debug function tests.
 *                Based on classes.prg
 * $Requirement$  source\tools\stringp.prg
 *                source\rtl\objfunc.prg
 *                source\rtl\asort.prg
 * $End$
 *
 * Written by Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

PROCEDURE Main()

   LOCAL oForm   := TForm():New()
   LOCAL nNumber := 15

   QOut( oForm:ClassName() )
   oForm:Show()
   QOut()

   QOut( "-OBJECT additions-" )
   QOut( "What is in oForm ? " )
   Debug( oForm:Transfer() )

   QOut( "Does transfer exists ? ", __objHasMsg   ( oForm, "Transfer" ) )
   QOut( "Is   transfer DATA   ? ", __objHasData  ( oForm, "Transfer" ) )
   QOut( "Is   transfer METHOD ? ", __objHasMethod( oForm, "Transfer" ) )
   QOut( "Does nLeft    exists ? ", __objHasMsg   ( oForm, "nLeft"    ) )
   QOut( "Is   nLeft    DATA   ? ", __objHasData  ( oForm, "nLeft"    ) )
   QOut( "Is   nLeft    METHOD ? ", __objHasMethod( oForm, "nLeft"    ) )
   QOut( "Does unknown  exists ? ", __objHasMsg   ( oForm, "Unknown"  ) )
   QOut( "Is   unknown  DATA   ? ", __objHasData  ( oForm, "Unknown"  ) )
   QOut( "Is   unknown  METHOD ? ", __objHasMethod( oForm, "Unknown"  ) )

   QOut( "Set nLeft to 50 and nRight to 100" )
   oForm:Transfer( { "nLeft", 50 }, { "nRight", 100 } )
   Debug( oForm:Transfer() )

   Pause()


   QOut( "-DEBUG Functions-" )
   QOut( "-Statics-" )
   Debug( __dbgVMVarSList() )

   QOut( "-Global Stack-" )
   Debug ( __dbgVMStkGList() )

   QOut( "-Local Stack-" )
   Debug ( __dbgVMStkLList() )

   QOut( "-Parameters-" )
   Debug ( __dbgVMParLList() )

   Pause()

   FuncSecond( 241, "Hello" )

   RETURN

FUNCTION Pause()

   RETURN __Accept( "" )

FUNCTION FuncSecond( nParam, cParam, uParam )

   LOCAL cWhat   := "Something"
   LOCAL nNumber := 2
   LOCAL xParam
   LOCAL xStack

   QOut()
   QOut( "-Second procedure-" )
   QOut()

   QOut( "-Statics-" )
   Debug ( __dbgVMVarSList() )
   QOut()

   QOut( "-Global Stack- Len=", __dbgVMStkGCount() )
   Debug ( __dbgVMStkGList() )
   QOut()

   QOut( "-Local Stack- Len=", __dbgVMStkLCount() )
   xStack := Debug ( __dbgVMStkLList() )
   QOut()

   QOut( "-Parameters-" )
   xParam := Debug( __dbgVMParLList() )
   IF xParam[ xStack[ 7 ] ] == "Hello"
      QOut( ":-)" )
   ENDIF

   Pause()

   RETURN nil


/* $Doc$
 * $FuncName$     <oForm> TForm()
 * $Description$  Returns TForm object
 * $End$ */

FUNCTION TForm()

   STATIC oClass

   IF oClass == nil
      oClass := HBClass():New( "TFORM" )    // starts a new class definition

      oClass:AddData( "cName" )           // define this class objects datas
      oClass:AddData( "nTop" )
      oClass:AddData( "nLeft" )
      oClass:AddData( "nBottom" )
      oClass:AddData( "nRight" )

      oClass:AddVirtual( "aExcept" )      // Export exceptions

      oClass:AddMethod( "New",  @New() )  // define this class objects methods
      oClass:AddMethod( "Show", @Show() )
      oClass:AddMethod( "Transfer", @Transfer() )

      oClass:Create()                     // builds this class
   ENDIF

   RETURN oClass:Instance()                  // builds an object of this class


/* $Doc$
 * $FuncName$     <oForm> TForm:New()
 * $Description$  Constructor
 * $End$ */

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self


/* $Doc$
 * $FuncName$     TForm:Show()
 * $Description$  Show a form
 * $End$ */

STATIC FUNCTION Show()

   LOCAL Self := QSelf()

   QOut( "lets show a form from here :-)" )

   RETURN nil

   //
   // <xRet> TForm:Transfer( [<xArg,..>] )
   //
   // Generic object import and export function
   //
   // <xArg> is present.
   //
   // Maximum number of arguments passed is limited to 10 !
   //
   // An argument can be one of the following :
   //
   // { <cSymbol>, <xValue> }              Set DATA <cSymbol> to <xValue>
   // { { <cSym1>, <xVal1> }, { <cSym2>, <xVal2> }, ... }
   //                                      Set a whole list symbols to value
   //                                      Normal way of set objects from external
   //                                      sources, like memo files.
   // <oObject>                            Set self according to the DATA
   //                                      contained in <oObject>
   //                                      Can be used to transfer info from
   //                                      one class to another
   //
   // If <xArg> is not present, the current object will be returned as an array
   // for description see __objSetValueList / __objGetValueList.
   //
   // The method aExcept() is called to determine the DATA which should not
   // be returned. Eg. hWnd ( do not copy this DATA from external source )
   //
   // Say we want to copy oSource into oTarget we say :
   //
   // oTarget:Transfer( oSource )
   //
   // If we do not want 'cName' duplicated we have to use __objGetValueList :
   //
   // aNewExcept := aClone( oSource:aExcept() )
   // aAdd( aNewExcept, "cName" )  /* Add cName to exception list               */
   // oTarget:Transfer( __objGetValueList( oSource, aNewExcept ) )
   //                              /* Get DATA from oSource with new exceptions */
   //                              /* Transfer DATA to oTarget                  */
   //
   // To set two DATA of oTarget :
   //
   // oTarget:Transfer( { "nLeft", 10 }, { "nRight", 5 } )
   //
   // or :
   //
   // aCollect := {}
   // aAdd( aCollect, { "nLeft" , 10 } )
   // aAdd( aCollect, { "nRight", 5  } )
   // oTarget:Transfer( aCollect )
   //
   // Copy oSource to a memo field :
   //
   // DbObject->Memo := oSource:Transfer()
   //
   // (Re)create oTarget from the memo field :
   //
   // oTarget := TTarget():New()
   // oTarget:Transfer( DbObject->Memo )
   //

STATIC FUNCTION Transfer( x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ) /* etc */

   LOCAL self   := QSelf()
   LOCAL aParam := __dbgVMParLList()
   LOCAL nLen   := PCount()
   LOCAL xRet
   LOCAL xData
   LOCAL n

   IF nLen == 0
      xRet := __objGetValueLIST( self, ::aExcept() )
   ELSE
      for n := 1 TO nLen

         xData := aParam[ n ]
         IF ValType( xData ) == "A"

            IF ValType( xData[ 1 ] ) == "A"     // 2D array passed
               xRet := __objSetValueList( self, xData )
            ELSE                                // 1D array passed
               xRet := __objSetValueList( self, { xData } )
            ENDIF

         ELSEIF ValType( xData ) == "O"         // Object passed
            xRet := ::Transfer( xData:Transfer() )
         ELSEIF !( ValType( xData ) == "U" )
            QOut( "TRANSFER: Incorrect argument(", n, ") ", xData )
         ENDIF

      next n
   ENDIF

   RETURN xRet
