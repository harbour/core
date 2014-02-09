/*
 * This program will read the .hrb file and show its content
 *
 * readhrb <hrb file>
 *
 * Eddie Runia <eddie@runia.com>
 * Vailton Renato <vailtom@gmail.com> (Updated to support current Harbour implementation)
 * www - http://harbour-project.org
 *
 * Placed in the public domain
 */

#include "fileio.ch"

#define HRB_HEADER      ( hb_BChar( 0xC0 ) + "HRB" )

#define HB_FS_PUBLIC    0x0001
#define HB_FS_STATIC    0x0002
#define HB_FS_FIRST     0x0004
#define HB_FS_INIT      0x0008
#define HB_FS_EXIT      0x0010
#define HB_FS_MESSAGE   0x0020
#define HB_FS_MEMVAR    0x0080
#define HB_FS_PCODEFUNC 0x0100
#define HB_FS_LOCAL     0x0200
#define HB_FS_DYNCODE   0x0400
#define HB_FS_DEFERRED  0x0800
#define HB_FS_FRAME     0x1000

STATIC s_aScopes := { ;
   { HB_FS_PUBLIC    , "HB_FS_PUBLIC"    }, ;
   { HB_FS_STATIC    , "HB_FS_STATIC"    }, ;
   { HB_FS_FIRST     , "HB_FS_FIRST"     }, ;
   { HB_FS_INIT      , "HB_FS_INIT"      }, ;
   { HB_FS_EXIT      , "HB_FS_EXIT"      }, ;
   { HB_FS_MESSAGE   , "HB_FS_MESSAGE"   }, ;
   { HB_FS_MEMVAR    , "HB_FS_MEMVAR"    }, ;
   { HB_FS_PCODEFUNC , "HB_FS_PCODEFUNC" }, ;
   { HB_FS_LOCAL     , "HB_FS_LOCAL"     }, ;
   { HB_FS_DYNCODE   , "HB_FS_DYNCODE"   }, ;
   { HB_FS_DEFERRED  , "HB_FS_DEFERRED"  }, ;
   { HB_FS_FRAME     , "HB_FS_FRAME"     } }

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

   hb_default( @cFrom, hb_FNameName( __FILE__ ) )

   cFrom := hb_FNameExtSetDef( cFrom, ".hrb" )

   IF ( hFile := FOpen( cFrom ) ) == F_ERROR
      ? "No such file:", cFrom
   ELSE
      IF FReadStr( hFile, 4 ) == HRB_HEADER

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
            cSymbol := ""
            DO WHILE hb_BCode( cBlock := FReadStr( hFile, 1 ) ) != 0
               cSymbol += cBlock
            ENDDO
            cScope := FReadStr( hFile, 1 )
            cBlock := FReadStr( hFile, 1 )
            nIdx   := hb_BCode( cBlock ) + 1
            PrintItem( cSymbol, nIdx, hb_BCode( cScope ) )
         NEXT
         ? "+--------------------------+------------+---------------------------------+"
         ? " ", hb_ntos( nSymbols ), "symbol(s) found."
         ?

         ACCEPT "Do you want to list all pcode values? (y/N) " TO m

         IF m $ "Yy"
            ?
            cBlock := FReadStr( hFile, 4 )
            nFuncs := ;
               hb_BCode( hb_BSubStr( cBlock, 1, 1 ) ) + ;
               hb_BCode( hb_BSubStr( cBlock, 2, 1 ) ) * 256 + ;
               hb_BCode( hb_BSubStr( cBlock, 3, 1 ) ) * 65536 + ;
               hb_BCode( hb_BSubStr( cBlock, 4, 1 ) ) * 16777216
            FOR n := 1 TO nFuncs
               cSymbol := ""
               DO WHILE hb_BCode( cBlock := FReadStr( hFile, 1 ) ) != 0
                  cSymbol += cBlock
               ENDDO
               cBlock := FReadStr( hFile, 4 )
               nLenCount := ;
                  hb_BCode( hb_BSubStr( cBlock, 1, 1 ) ) + ;
                  hb_BCode( hb_BSubStr( cBlock, 2, 1 ) ) * 256 + ;
                  hb_BCode( hb_BSubStr( cBlock, 3, 1 ) ) * 65536 + ;
                  hb_BCode( hb_BSubStr( cBlock, 4, 1 ) ) * 16777216

               ? "Symbol:", cSymbol
               ? "Length:", hb_ntos( nLenCount ), "byte(s)"
               ? "PCode:", ""

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
      ELSE
         ? "Invalid input file detected!"
      ENDIF
      FClose( hFile )
   ENDIF

   RETURN

STATIC PROCEDURE PrintItem( cSymbol, nType, nScope )

   ? "|", ;
      PadR( cSymbol, 25 ) + "|", ;
      PadR( hb_ntos( nType - 1 ) + " (" + { "NOLINK", "FUNC", "EXTERN", "SYM_DEF" }[ nType ] + ")", 11 ) + "|", ;
      PadR( DecodeScope( nScope ), 32 ) + "|"

   RETURN

STATIC FUNCTION DecodeScope( nScope )

   LOCAL cScope := ""
   LOCAL aScope

   FOR EACH aScope IN s_aScopes
      IF hb_bitAnd( nScope, aScope[ 1 ] ) == aScope[ 1 ]
         cScope += "+" + aScope[ 2 ]
      ENDIF
   NEXT

   RETURN hb_NumToHex( nScope, 2 ) + iif( Empty( cScope ), "", " (" + SubStr( cScope, 2 ) + ")" )
