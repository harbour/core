/*
 * ReadHRB
 *
 * This program will read the .HRB file and shows its contents
 *
 * ReadHRB <program file>  {No .HRB extension please}
 *
 * Written by Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
 *
 * Placed in the public domain
 */

#include "set.ch"

function Main( cFrom )

   local hFile
   local cBlock := " "
   local n, m
   local nVal
   local nSymbols
   local nFuncs
   local cMode := "SYMBOL"
   local cScope
   local nLenCount
   local nIdx
   local aTypes := { "NOLINK", "FUNC", "EXTERN" }

   set( _SET_EXACT, .T. )
   set( _SET_ALTERNATE, "readhrb.out" )
   set( _SET_ALTERNATE, .T. )

   if cFrom == NIL
      cFrom := "hello.hrb"
   else
      cFrom := cFrom + ".hrb"
   endif

   hFile := fOpen( cFrom )

   cBlock := fReadStr( hFile, 4 )
   nSymbols := asc(substr(cBlock,1,1))           +;
               asc(substr(cBlock,2,1)) *256      +;
               asc(substr(cBlock,3,1)) *65536    +;
               asc(substr(cBlock,4,1)) *16777216
   for n := 1 to nSymbols
      cBlock := fReadStr( hFile, 1 )
      do while asc( cBlock ) != 0
         QQOut( cBlock )
         cBlock := fReadStr( hFile, 1 )
      enddo
      cScope := fReadStr( hFile, 1 )
      QQOut(" Scope ", Hex2Val(asc(cScope)))
      cScope := fReadStr( hFile, 1 )
      nIdx   := asc( cScope ) + 1
      QQOut(" Type ", aTypes[ nIdx ] )
      QOut()
   next n

   cBlock := fReadStr( hFile, 4 )
   nFuncs := asc(substr(cBlock,1,1))           +;
             asc(substr(cBlock,2,1)) *256      +;
             asc(substr(cBlock,3,1)) *65536    +;
             asc(substr(cBlock,4,1)) *16777216
   for n := 1 to nFuncs
      QOut()
      cBlock := fReadStr( hFile, 1 )
      do while asc( cBlock ) != 0
         QQOut( cBlock )
         cBlock := fReadStr( hFile, 1 )
      enddo
      QOut( "Len = " )
      cBlock := fReadStr( hFile, 4 )

      nLenCount := asc(substr(cBlock,1,1))           +;
                   asc(substr(cBlock,2,1)) *256      +;
                   asc(substr(cBlock,3,1)) *65536    +;
                   asc(substr(cBlock,4,1)) *16777216 +1
      QQOut( str(nLenCount) )
      QOut()

      for m:=1 to nLenCount
         cBlock := fReadStr( hFile, 1 )
         nVal   := asc( cBlock )
         QQOut( Hex2Val( nVal ) )
         if nVal > 32 .and. nVal < 128
            QQOut( "("+cBlock+")" )
         endif
         if m != nLenCount
            QQOut(",")
         endif
      next m
   next n

   fClose( cFrom )

   set( _SET_ALTERNATE, .F. )
return nil


function Hex2Val( nVal )

return HexDigit( int(nVal / 16) ) + HexDigit( int(nVal % 16) )

function HexDigit( nDigit )

return if(nDigit>=10, chr( 55 + nDigit ), chr( 48 + nDigit ) )


