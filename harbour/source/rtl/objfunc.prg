#define DATA_SYMBOL 1
#define DATA_VAL    2

//
// <lRet> := IsData( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as DATA ?
//
function IsData( oObject, cSymbol )

return IsMessage( oObject, cSymbol ) .and. IsMessage( oObject, "_" + cSymbol )


//
// <lRet> := IsMethod( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as METHOD ?
//
function IsMethod( oObject, cSymbol )

return IsMessage( oObject, cSymbol ) .and. !IsMessage( oObject, "_" + cSymbol )

//
// <aData> aOData( <oObject>, [lDataMethod] )
//
// Return an array containing the names of all the data items of oObject.
//
// lDataMethod = .T. (default) Return all DATAs
//               .F.           Return all METHODs
//
function aOData( oObject, lDataMethod )

   local aInfo  := aSort( oObject:ClassSel() )
   local aData  := {}
   local n      := 1
   local nLen   := Len( aInfo )
   local lFoundDM                               // Found DATA ?

   lDataMethod  := Default( lDataMethod, .T. )
   do while n <= nLen .and. Substr( aInfo[ n ], 1, 1 ) != "_"

/* If in range and no set function found yet ( set functions begin with a   */
/* leading underscore ).                                                    */

      lFoundDM := !Empty( aScan( aInfo, "_" + aInfo[ n ], n + 1 ) )

/* Find position of matching set function in array with all symbols         */

      if lFoundDM == lDataMethod                // If found -> DATA
                                                //     else    METHOD
         aAdd( aData, aInfo[ n ] )
      endif
      n++
   enddo

return aData


//
// aData aOMethod( oObject )
//
// Return an array containing the names of all the method of oObject.
//
function aOMethod( oObject )

return aOData( oObject, .F. )


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

//   aEval( aData, ;                            // Still losing 2 block
//        {|aItem| oSend( oObject, "_"+aItem[DATA_SYMBOL], aItem[DATA_VAL] ) } )

   for n := 1 to nLen
      oSend( oObject, "_" + aData[n][DATA_SYMBOL], aData[n][DATA_VAL] )
                                                // Send the message
   next n
return oObject

