/* $Doc$
 * $Description$  Debug function tests.
 *                Based on classes.prg
 * $Requirement$  source\tools\debug.c
 *                source\rtl\classes.c (1999/05/97)
 *                source\rtl\itemapi.c (1999/05/04)
 * $Date$
 * $End$ */

//
// Warning : This program contains abstract high level Harbour Power !!!!
//

#define DATA_SYMBOL 1
#define DATA_VAL    2

function Main()

   local oForm   := TForm():New()
   local nNumber := 15

   QOut( oForm:ClassName() )
   oForm:Show()
   QOut()

   QOut( "-DEBUG Functions-")
   Debug( oForm:Transfer() )

   oForm:Transfer( {"nLeft", 50}, {"nRight", 100} )

   QOut( "-Statics-" )
   Debug( __aStatic() )

   QOut( "-Global Stack-" )
   Debug ( __aGlobalStack() )

   QOut( "-Local Stack-" )
   Debug ( __aStack() )

   QOut( "-Parameters-" )
   Debug ( __aParam() )

   Pause()

   FuncSecond( 241, "Hello" )

return nil


function Pause()
return __Accept("")


function FuncSecond( nParam, cParam, uParam )

   local cWhat   := "Something"
   local nNumber := 2
   local xParam
   local xStack

   QOut()
   QOut( "-Second procedure-")
   QOut()

   QOut( "-Statics-" )
   Debug ( __aStatic() )
   QOut()

   QOut( "-Global Stack- Len=", __GlobalStackLen() )
   Debug ( __aGlobalStack() )
   QOut()

   QOut( "-Local Stack- Len=", __StackLen() )
   xStack := Debug ( __aStack() )
   QOut()

   QOut( "-Parameters-" )
   xParam := Debug( __aParam() )
   if xParam[ xStack[ 7 ] ] == "Hello"
      QOut( ":-)" )
   endif

   Pause()

return nil


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

      oClass:AddMethod( "aExcept", @Virtual() )
                                          // Export exceptions

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
static function Transfer( x1,x2,x3,x4,x5,x6,x7,x8,x9,x10 /* etc */ )

   local self   := QSelf()
   local aParam := __aParam()
   local nLen   := Len( aParam )                // PCount() not implemented
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

static function Virtual()               /* Not implemented ?? */
return nil


//
// aData aOData( oObject )
//
// Return an array containing the names of all the data items of oObject.
//
function aOData( oObject )

   local aInfo  := aSort( oObject:ClassSel() )
   local aData  := {}
   local n      := 1
   local nLen   := Len( aInfo )

   do while n <= nLen .and. Substr( aInfo[ n ], 1, 1 ) != "_"
      if !Empty( aScan( aInfo, "_" + aInfo[ n ], n + 1 ) )
         aAdd( aData, aInfo[ n ] )
      endif
      n++
   enddo

return aData

//
// <aData> aOGet( <oObject>, [<aExcept>] )
//
// Basically the same as aOData except that it returns a 2D array
// containing :
//
// [x][1]  Symbol name
// [x][2]  Value of DATA
//
// aExcept is an optional list of DATA you do not want to collect
//
function aOGet( oObject, aExcept )

   local aDataSymbol := aoData( oObject )
   local nLen        := Len( aDataSymbol )
   local aData       := {}
   local cSymbol
   local n

   aExcept := Default( aExcept, {} )
   for n := 1 to nLen
      cSymbol := aDataSymbol[ n ]
      if Empty( aScan( aExcept, cSymbol ) )
         aAdd( aData, { cSymbol, oSend( oObject, cSymbol ) } )
      endif
   next n
return aData


//
// aOSet( <oObject>, <aData> )
//
// The reverse of aOGet. It puts an 2D array of DATA into an object.
//
function aOSet( oObject, aData )

   local n
   local nLen := Len( aData )
//  aEval( aData, ;                             // aEval looses memory blocks
//       {|aItem| oSend( oObject, "_"+aItem[DATA_SYMBOL], aItem[DATA_VAL] ) } )

   for n:= 1 to nLen
      oSend( oObject, "_"+aData[n][DATA_SYMBOL], aData[n][DATA_VAL] )
   next n

return oObject


//
// <aSorted> aSort( <aUnsorted>, [nStart], [nCount], [bBlock] )
//
// Sort an array
//
function aSort( aIn, nStart, nCount, bBlock )

   nStart := Default( nStart, 1 )
   QuickSort( aIn,                                      ;
              nStart,                                   ;
              Default( nCount, Len(aIn) - nStart + 1 ), ;
              Default( bBlock, {| x, y | x < y } ) )
return aIn


//
// QuickSort( <aSort>, <nLeft>, <nRight>, <bOrder> )
//
// Perform a QuickSort of <aSort>.
//
// For instructions :
// http://monty.cnri.reston.va.us/grail/demo/quicksort/quicksort.htm
//
function QuickSort( aSort, nLeft, nRight, bOrder )

   local nUp     := nLeft
   local nDown   := nRight
   local xMiddle := aSort[ ( nLeft + nRight ) / 2 ]
   local xTemp
   local lOk     := .T.

   do while lOk
      do while Eval( bOrder, aSort[ nUp ], xMiddle   )
         nUp++
      enddo

      do while Eval( bOrder, xMiddle, aSort[ nDown ] )
         nDown--
      enddo

      if nUp <= nDown
         if nUp != nDown
            xTemp          := aSort[ nUp ]
            aSort[ nUp   ] := aSort[ nDown ]
            aSort[ nDown ] := xTemp
         endif
         nUp++
         nDown--
      endif

      lOk := nUp <= nDown
   enddo

   if nLeft < nDown
      QuickSort( aSort, nLeft, nDown , bOrder )
   endif

   if nUp < nRight
      QuickSort( aSort, nUp  , nRight, bOrder )
   endif

return nil


