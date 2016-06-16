/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */
/* Updated to support current Harbour implementation by Vailton Renato <vailtom@gmail.com> */

/* This program will read the .hrb file and show its content
 *
 * readhrb <hrb file>
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
   LOCAL cBorder

   cFrom := hb_FNameExtSetDef( hb_defaultValue( cFrom, hb_FNameName( __FILE__ ) ), ".hrb" )

   IF ( hFile := hb_vfOpen( cFrom, FO_READ ) ) == NIL
      ? "No such file:", cFrom
   ELSE
      IF hb_vfReadLen( hFile, 4 ) == HRB_HEADER

         hb_vfReadLen( hFile, 2 )
         cBlock := hb_vfReadLen( hFile, 4 )
         nSymbols := ;
            hb_BPeek( cBlock, 1 ) + ;
            hb_BPeek( cBlock, 2 ) * 0x100 + ;
            hb_BPeek( cBlock, 3 ) * 0x10000 + ;
            hb_BPeek( cBlock, 4 ) * 0x1000000

         ?? cBorder := "+" + ;
            Replicate( "-", 25 + 1 ) + "+" + ;
            Replicate( "-", 11 + 1 ) + "+" + ;
            Replicate( "-", 32 + 1 ) + "+"
         PrintItem( "Symbol Name", "Type", "Scope" )
         ? cBorder

         FOR n := 1 TO nSymbols
            cSymbol := ""
            DO WHILE hb_BCode( cBlock := hb_vfReadLen( hFile, 1 ) ) != 0
               cSymbol += cBlock
            ENDDO
            cScope := hb_vfReadLen( hFile, 1 )
            cBlock := hb_vfReadLen( hFile, 1 )
            nIdx   := hb_BCode( cBlock ) + 1
            PrintItem( cSymbol, ;
               hb_ntos( nIdx - 1 ) + " (" + { "NOLINK", "FUNC", "EXTERN", "SYM_DEF" }[ nIdx ] + ")", ;
               DecodeScope( hb_BCode( cScope ) ) )
         NEXT
         ? cBorder
         ? " ", hb_ntos( nSymbols ), "symbol(s) found."
         ?

         ACCEPT "Do you want to list all pcode values? (y/N) " TO m

         IF m $ "Yy"
            ?
            cBlock := hb_vfReadLen( hFile, 4 )
            nFuncs := ;
               hb_BPeek( cBlock, 1 ) + ;
               hb_BPeek( cBlock, 2 ) * 0x100 + ;
               hb_BPeek( cBlock, 3 ) * 0x10000 + ;
               hb_BPeek( cBlock, 4 ) * 0x1000000
            FOR n := 1 TO nFuncs
               cSymbol := ""
               DO WHILE hb_BCode( cBlock := hb_vfReadLen( hFile, 1 ) ) != 0
                  cSymbol += cBlock
               ENDDO
               cBlock := hb_vfReadLen( hFile, 4 )
               nLenCount := ;
                  hb_BPeek( cBlock, 1 ) + ;
                  hb_BPeek( cBlock, 2 ) * 0x100 + ;
                  hb_BPeek( cBlock, 3 ) * 0x10000 + ;
                  hb_BPeek( cBlock, 4 ) * 0x1000000

               ? "Symbol:", cSymbol
               ? "Length:", hb_ntos( nLenCount ), "byte(s)"
               ? "PCode:", ""

               FOR m := 1 TO nLenCount
                  cBlock := hb_vfReadLen( hFile, 1 )
                  nVal   := hb_BCode( cBlock )
                  ?? hb_NumToHex( nVal, 2 )
                  IF nVal > 32 .AND. nVal < 128
                     ?? "(" + cBlock + ")"
                  ENDIF
                  IF m != nLenCount
                     ?? ","
                  ENDIF
               NEXT
               ? "---"
            NEXT
         ENDIF
      ELSE
         ? "Invalid input file detected!"
      ENDIF
      hb_vfClose( hFile )
   ENDIF

   RETURN

STATIC PROCEDURE PrintItem( cSymbol, cType, cScope )

   ? "|", ;
      PadR( cSymbol, 25 ) + "|", ;
      PadR( cType, 11 ) + "|", ;
      PadR( cScope, 32 ) + "|"

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
