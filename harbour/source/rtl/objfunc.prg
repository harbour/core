#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

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

   aEval( aData, ;
        {|aItem| oSend( oObject, "_"+aItem[DATA_SYMBOL], aItem[DATA_VAL] ) } )

return oObject


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


