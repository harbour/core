/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */

/* Debug function tests */

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oForm   := TForm():New()
   LOCAL nNumber := 15

   HB_SYMBOL_UNUSED( nNumber )

   ? oForm:ClassName()
   oForm:Show()
   ?

   ? "-OBJECT additions-"
   ? "What is in oForm ?"
   ? hb_ValToExp( oForm:Transfer() )

   ? "Does transfer exists ?", __objHasMsg(    oForm, "Transfer" )
   ? "Is   transfer DATA   ?", __objHasData(   oForm, "Transfer" )
   ? "Is   transfer METHOD ?", __objHasMethod( oForm, "Transfer" )
   ? "Does nLeft    exists ?", __objHasMsg(    oForm, "nLeft"    )
   ? "Is   nLeft    DATA   ?", __objHasData(   oForm, "nLeft"    )
   ? "Is   nLeft    METHOD ?", __objHasMethod( oForm, "nLeft"    )
   ? "Does unknown  exists ?", __objHasMsg(    oForm, "Unknown"  )
   ? "Is   unknown  DATA   ?", __objHasData(   oForm, "Unknown"  )
   ? "Is   unknown  METHOD ?", __objHasMethod( oForm, "Unknown"  )

   ? "Set nLeft to 50 and nRight to 100"
   oForm:Transfer( { "nLeft", 50 }, { "nRight", 100 } )
   ? hb_ValToExp( oForm:Transfer() )

   Inkey( 0 )

   ? "-DEBUG Functions-"
   ? "-Statics-"
   ? hb_ValToExp( __dbgVMVarSList() )

   ? "-Global Stack-"
   ? hb_ValToExp( __dbgVMStkGList() )

   ? "-Local Stack-"
   ? hb_ValToExp( __dbgVMStkLList() )

   ? "-Parameters-"
   ? hb_ValToExp( __dbgVMParLList() )

   Inkey( 0 )

   FuncSecond( 241, "Hello" )

   RETURN

STATIC PROCEDURE FuncSecond( nParam, cParam, uParam )

   LOCAL cWhat   := "Something"
   LOCAL nNumber := 2
   LOCAL xParam
   LOCAL xStack

   HB_SYMBOL_UNUSED( cWhat )
   HB_SYMBOL_UNUSED( nNumber )

   HB_SYMBOL_UNUSED( nParam )
   HB_SYMBOL_UNUSED( cParam )
   HB_SYMBOL_UNUSED( uParam )

   ?
   ? "-Second procedure-"
   ?

   ? "-Statics-"
   ? hb_ValToExp( __dbgVMVarSList() )
   ?

   ? "-Global Stack- Len=", __dbgVMStkGCount()
   ? hb_ValToExp( __dbgVMStkGList() )
   ?

   ? "-Local Stack- Len=", __dbgVMStkLCount()
   ? hb_ValToExp( xStack := __dbgVMStkLList() )
   ?

   ? "-Parameters-"
   ? hb_ValToExp( xParam := __dbgVMParLList() )
   IF ! Empty( xStack ) .AND. xParam[ xStack[ 7 ] ] == "Hello"
      ? ":-)"
   ENDIF

   Inkey( 0 )

   RETURN

CREATE CLASS TForm STATIC

   VAR cName
   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight

   METHOD aExcept() VIRTUAL

   METHOD New()
   METHOD Show()
   METHOD Transfer( ... )

ENDCLASS

METHOD New() CLASS TForm

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self

PROCEDURE Show() CLASS TForm

   ? "lets show a form from here :-)"

   RETURN

// <xRet> TForm:Transfer( [<xArg,..>] )
//
// Generic object import and export function
//
// <xArg> is present.
//
// Maximum number of arguments passed is limited to 10 !
//
// An argument can be one of the following:
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
// for description see __objSetValueList() / __objGetValueList().
//
// The method aExcept() is called to determine the DATA which should not
// be returned. Eg. hWnd (do not copy this DATA from external source)
//
// Say we want to copy oSource into oTarget we say:
//
// oTarget:Transfer( oSource )
//
// If we do not want 'cName' duplicated we have to use __objGetValueList():
//
// aNewExcept := AClone( oSource:aExcept() )
// AAdd( aNewExcept, "cName" )  /* Add cName to exception list               */
// oTarget:Transfer( __objGetValueList( oSource, aNewExcept ) )
//                              /* Get DATA from oSource with new exceptions */
//                              /* Transfer DATA to oTarget                  */
//
// To set two DATA of oTarget:
//
// oTarget:Transfer( { "nLeft", 10 }, { "nRight", 5 } )
//
// or:
//
// aCollect := {}
// AAdd( aCollect, { "nLeft" , 10 } )
// AAdd( aCollect, { "nRight", 5  } )
// oTarget:Transfer( aCollect )
//
// Copy oSource to a memo field:
//
// DbObject->Memo := oSource:Transfer()
//
// (Re)create oTarget from the memo field:
//
// oTarget := TTarget():New()
// oTarget:Transfer( DbObject->Memo )

METHOD Transfer( ... ) CLASS TForm

   LOCAL xRet
   LOCAL xData

   IF PCount() == 0
      xRet := __objGetValueList( self, ::aExcept() )
   ELSE
      FOR EACH xData IN __dbgVMParLList()

         DO CASE
         CASE HB_ISARRAY( xData )

            IF HB_ISARRAY( xData[ 1 ] )         // 2D array passed
               xRet := __objSetValueList( self, xData )
            ELSE                                // 1D array passed
               xRet := __objSetValueList( self, { xData } )
            ENDIF

         CASE HB_ISOBJECT( xData )              // Object passed
            xRet := ::Transfer( xData:Transfer() )
         CASE !( ValType( xData ) == "U" )
            ? "TRANSFER: Incorrect argument(", xData:__enumIndex(), ")", xData
         ENDCASE
      NEXT
   ENDIF

   RETURN xRet
