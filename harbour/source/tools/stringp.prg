/* $Doc$
 * $FuncName$     <xRet> Default( <xArg>, <xDefault> )
 * $Description$  If argument is not set, return default
 * $End$ */
function Default( xArg, xDef )
return if( ValType(xArg) != ValType(xDef), xDef, xArg )


/* $Doc$
 * $FuncName$     <cOut> ToChar( <xTxt>, [cSeparator], [lDebug] )
 * $Description$  Convert to character
 * $Arguments$    <xTxt>       : Item to write
 *                [cSeparator] : Separator for arrays
 *                [lDebug]     : .T. -> Write debug output
 *
 * In DEBUG mode :
 *
 * It will show the xItem according to the following format :
 *
 * <num>                        Numerical
 * dd/mm/yyyy                   Date
 * "<chr>"                      Character
 * {<el1>, <el2>, ...}          Array
 * NIL                          NIL
 * .T. / .F.                    Boolean
 * <ClassName>(<ClassH>):{<DataSymbol1>:<val1>, ... }
 *                              Object
 *
 *
 * $End$ */
function ToChar( xTxt, cSeparator, lDebug )

   local cValTxt
   local cOut
   local n
   local nLen
   local aData

   cSeparator := Default( cSeparator, " " )
   lDebug     := Default( lDebug,     .F. )
   cValTxt    := ValType( xTxt )

   do case
      case cValTxt=="C" .or. cValTxt=="M"       // Character
         cOut := if( lDebug, '"'+xTxt+'"', xTxt )

      case cValTxt=="N"                         // Numeric
         cOut := Alltrim(Str(xTxt))

      case cValTxt=="U"                         // Nothing to write
         cOut := if( lDebug, "NIL", "" )

      case cValTxt=="D"                         // Date
         cOut := TransForm(xTxt, "")

      case cValTxt=="L"                         // Logical
         if lDebug
            cOut := if( xTxt, ".T.", ".F." )
         else
            cOut := if( xTxt, "True", "False" )
         endif

      case cValTxt=="A"                         // Array
         if lDebug
            cOut += "{"
         else
            cOut := ""
         endif
         nLen := Len( xTxt )
         for n := 1 to nLen                     // For each item : Recurse !
            cOut += ToChar( xTxt[n], cSeparator, lDebug )
            if n != nLen
               cOut += cSeparator
            endif
         next n
         if lDebug
            cOut += "}"
         endif

      case cValTxt=="B"                         // Codeblock
         if lDebug
            cOut := "Block"
         else
            cOut := Eval( xTxt )
         endif

      case cValTxt=="O"                         // Object
         if lDebug
            cOut  := xTxt:ClassName() + "(#" + ToChar( xTxt:ClassH() ) + "):{"
            aData := aoGet( xTxt )
            nLen  := Len( aData )
            for n := 1 to nLen                     // For each item : Recurse !
               cOut += aData[n][DATA_SYMBOL] + ":" + ;
                       ToChar( aData[n][DATA_VAL], cSeparator, lDebug )
               if n != nLen
                  cOut += cSeparator
               endif
            next n
            cOut += "}"
         else
            cOut := ToChar( xTxt:Run(), cSeparator, lDebug )
         endif

   endcase

return cOut

//
// <xItem> Debug ( <xItem> )
//
// Non-volatile debugging function showing contents of xItem and returing
// passed argument.
//
function Debug( xItem )

   QOut( ToChar( xItem, ", ", .T. ) )

return xItem


/* $Doc$
 * $FuncName$     <oForm> TForm()
 * $Description$  Returns TForm object
 * $End$ */
function TForm()

   static oClass

   if oClass == nil
      oClass = TClass():New( "TFORM" )    // starts a new class definition

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
   endif

return oClass:Instance()                  // builds an object of this class


/* $Doc$
 * $FuncName$     <oForm> TForm:New()
 * $Description$  Constructor
 * $End$ */
static function New()

   local Self := QSelf()

   ::nTop    = 10
   ::nLeft   = 10
   ::nBottom = 20
   ::nRight  = 40

return Self


/* $Doc$
 * $FuncName$     TForm:Show()
 * $Description$  Show a form
 * $End$ */
static function Show()

   local Self := QSelf()

   QOut( "lets show a form from here :-)" )

return nil


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
// for description see aoSet / aoGet.
//
// The method aExcept() is called to determine the DATA which should not
// be returned. Eg. hWnd ( do not copy this DATA from external source )
//
// Say we want to copy oSource into oTarget we say :
//
// oTarget:Transfer( oSource )
//
// If we do not want 'cName' duplicated we have to use aoGet :
//
// aNewExcept := aClone( oSource:aExcept() )
// aAdd( aNewExcept, "cName" )  /* Add cName to exception list               */
// oTarget:Transfer( aoGet( oSource, aNewExcept ) )
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
static function Transfer( x1,x2,x3,x4,x5,x6,x7,x8,x9,x10 /* etc */ )

   local self   := QSelf()
   local aParam := __aParam()
   local nLen   := PCount()
   local xRet
   local xData
   local n

   if nLen == 0
      xRet := aOGet( self, ::aExcept() )
   else
      for n := 1 to nLen

         xData := aParam[ n ]
         if ValType( xData ) == "A"

            if ValType( xData[1] ) == "A"       // 2D array passed
               xRet := aOSet( self, xData )
            else                                // 1D array passed
               xRet := aOSet( self, {xData} )
            endif

         elseif ValType( xData ) == "O"         // Object passed
            xRet := ::Transfer( xData:Transfer() )
         elseif ValType( xData ) != "U"
            QOut( "TRANSFER: Incorrect argument(", n, ") ", xData )
         endif

      next n
   endif

return xRet



