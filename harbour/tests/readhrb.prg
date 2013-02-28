/*
 * $Id$
 */

/*
 * This program will read the .hrb file and show its content
 *
 * readhrb <hrb file>
 *
 * Eddie Runia <eddie@runia.com>
 * Vailton Renato <vailtom@gmail.com> (Updated to support current harbour implementation)
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

#include "fileio.ch"

#define HRB_HEADER      ( hb_BChar( 192 ) + hb_BChar( 72 ) + hb_BChar( 82 ) + hb_BChar( 66 ) )
#define HB_FS_PUBLIC    ( 0x0001 )
#define HB_FS_STATIC    ( 0x0002 )
#define HB_FS_FIRST     ( 0x0004 )
#define HB_FS_INIT      ( 0x0008 )
#define HB_FS_EXIT      ( 0x0010 )
#define HB_FS_MESSAGE   ( 0x0020 )
#define HB_FS_MEMVAR    ( 0x0080 )
#define HB_FS_PCODEFUNC ( 0x0100 )
#define HB_FS_LOCAL     ( 0x0200 )
#define HB_FS_DYNCODE   ( 0x0400 )
#define HB_FS_DEFERRED  ( 0x0800 )
#define HB_FS_FRAME     ( 0x1000 )
#define HB_FS_INITEXIT  ( HB_FS_INIT + HB_FS_EXIT )

STATIC s_aScopes := { ;
   { HB_FS_PUBLIC    , "HB_FS_PUBLIC" }, ;
   { HB_FS_STATIC    , "HB_FS_STATIC" }, ;
   { HB_FS_FIRST     , "HB_FS_FIRST" }, ;
   { HB_FS_INIT      , "HB_FS_INIT" }, ;
   { HB_FS_EXIT      , "HB_FS_EXIT" }, ;
   { HB_FS_MESSAGE   , "HB_FS_MESSAGE" }, ;
   { HB_FS_MEMVAR    , "HB_FS_MEMVAR" }, ;
   { HB_FS_PCODEFUNC , "HB_FS_PCODEFUNC" }, ;
   { HB_FS_LOCAL     , "HB_FS_LOCAL" }, ;
   { HB_FS_DYNCODE   , "HB_FS_DYNCODE" }, ;
   { HB_FS_DEFERRED  , "HB_FS_DEFERRED" }, ;
   { HB_FS_FRAME     , "HB_FS_FRAME" } }

PROCEDURE Main( cFrom )

   LOCAL hFile
   LOCAL cBlock
   LOCAL n, m
   LOCAL nVal
   LOCAL nSymbols
   LOCAL nFuncs
   LOCAL cScope
   LOCAL nLenCount
   LOCAL nIdx
   LOCAL cSymbol

   Set( _SET_ALTERNATE, "readhrb.out" )
   Set( _SET_ALTERNATE, .T. )

   hb_default( @cFrom, "default" )

   cFrom := hb_FNameExtSetDef( cFrom, ".hrb" )

   hFile := FOpen( cFrom )
   IF hFile == F_ERROR
      ? "No such file:", cFrom
   ELSE
      cBlock := FReadStr( hFile, 4 )

      IF !( cBlock == HRB_HEADER )
         ? "Invalid input file detected!"
      ELSE
         FReadStr( hFile, 2 )
         cBlock := FReadStr( hFile, 4 )
         nSymbols := ;
            hb_BCode( hb_BSubStr( cBlock, 1, 1 ) ) + ;
            hb_BCode( hb_BSubStr( cBlock, 2, 1 ) ) * 256 + ;
            hb_BCode( hb_BSubStr( cBlock, 3, 1 ) ) * 65536 + ;
            hb_BCode( hb_BSubStr( cBlock, 4, 1 ) ) * 16777216

         ?? "+--------------------------+------------+---------------------------------+"
         ? "| Symbol Name              |  Type      | Scope                           |"
         ? "+--------------------------+------------+---------------------------------+"

         FOR n := 1 TO nSymbols
            cBlock := FReadStr( hFile, 1 )
            cSymbol := ""
            DO WHILE hb_BCode( cBlock ) != 0
               cSymbol += cBlock
               cBlock := FReadStr( hFile, 1 )
            ENDDO
            cScope := FReadStr( hFile, 1 )
            cBlock := FReadStr( hFile, 1 )
            nIdx   := hb_BCode( cBlock ) + 1
            PrintItem( cSymbol, nIdx, hb_BCode( cScope ) )
         NEXT
         ? "+--------------------------+------------+---------------------------------+"
         ? " ", hb_ntos( nSymbols ), "SYMBOL(s) found."
         ?

         ACCEPT "Do you want do list all pcode values? (y/N) " TO m

         IF m $ "Yy"
            ?
            cBlock := FReadStr( hFile, 4 )
            nFuncs := ;
               hb_BCode( hb_BSubStr( cBlock, 1, 1 ) ) + ;
               hb_BCode( hb_BSubStr( cBlock, 2, 1 ) ) * 256 + ;
               hb_BCode( hb_BSubStr( cBlock, 3, 1 ) ) * 65536 + ;
               hb_BCode( hb_BSubStr( cBlock, 4, 1 ) ) * 16777216
            FOR n := 1 TO nFuncs
               cBlock := FReadStr( hFile, 1 )
               cSymbol := ""
               DO WHILE hb_BCode( cBlock ) != 0
                  cSymbol += cBlock
                  cBlock := FReadStr( hFile, 1 )
               ENDDO
               cBlock := FReadStr( hFile, 4 )
               nLenCount := ;
                  hb_BCode( hb_BSubStr( cBlock, 1, 1 ) ) + ;
                  hb_BCode( hb_BSubStr( cBlock, 2, 1 ) ) * 256 + ;
                  hb_BCode( hb_BSubStr( cBlock, 3, 1 ) ) * 65536 + ;
                  hb_BCode( hb_BSubStr( cBlock, 4, 1 ) ) * 16777216

               ? "Symbol:", cSymbol
               ? "Length:", hb_ntos( nLenCount ), "byte(s)"
               ? "PCode: "

               FOR m := 1 TO nLenCount
                  cBlock := FReadStr( hFile, 1 )
                  nVal   := hb_BCode( cBlock )
                  ?? hb_NumToHex( nVal, 2 )
                  IF nVal > 32 .AND. nVal < 128
                     ?? "(" + cBlock + ")"
                  ENDIF
                  IF m != nLenCount
                     ?? ","
                  ENDIF
               NEXT
               ? "----"
            NEXT
         ENDIF
      ENDIF
      FClose( hFile )
   ENDIF
   Set( _SET_ALTERNATE, .F. )

   RETURN

PROCEDURE PrintItem( cSymbol, nType, nScope )

   LOCAL aTypes := { "NOLINK", "FUNC", "EXTERN", "SYM_DEF" }

   ? "| " + PadR( cSymbol, 25 ) + "| " + ;
      PadR( hb_ntos( nType - 1 ) + " (" + aTypes[ nType ] + ")", 11 ) + "| " + ;
      PadR( DecodeScope( nScope ), 32 ) + "|"

   RETURN

FUNCTION DecodeScope( nScope )

   LOCAL cScope := ""
   LOCAL i

   FOR i := 1 TO Len( s_aScopes )
      IF hb_bitAnd( nScope, s_aScopes[ i ][ 1 ] ) == s_aScopes[ i ][ 1 ]
         cScope += "+" + s_aScopes[ i ][ 2 ]
      ENDIF
   NEXT

   RETURN hb_NumToHex( nScope, 2 ) + iif( Empty( cScope ), "", " (" + SubStr( cScope, 2 ) + ")" )
