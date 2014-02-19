/*
 * Test program for hb_BuildInfo( [<nInfo>] )
 * Retrieve information on xHarbour Build Info
 * Please refer to xhbver.ch for more information on nInfo
 */

#require "xhb"

#include "xhbver.ch"

PROCEDURE Main()

   LOCAL aInfo := hb_BuildInfo()
   LOCAL i, j, uEl, cMember

   ? "hb_BuildInfo() -> without parameter..."
   ? "Returns:", hb_ntos( Len( aInfo ) ) + "-element single-dimensional-array"
   ? "Element types are varying: L, C, N or A"
   WAIT

   i := j := 0
   FOR EACH uEl IN aInfo
      i++
      IF HB_ISARRAY( uEl )
         FOR EACH cMember IN uEl
            ? ;
               "[" + PadL( hb_ntos( i ), 2 ) + "]" + ;
               "[" + PadL( hb_ntos( ++j ), 2 ) + "]", ;
               "=", cMember
         NEXT
      ELSE
         ? "[" + PadL( hb_ntos( i ), 2 ) + "]", "=", uEl
      ENDIF
   NEXT
   ?

   ? "hb_BuildInfo( nInfo ) -> with parameter..."
   ? "nInfo = information item number. Please refer to xhbver.ch"
   WAIT

   _BldInfo( "_HB_VER_MAJOR", _HB_VER_MAJOR )
   _BldInfo( "_HB_VER_MINOR", _HB_VER_MINOR )
   _BldInfo( "_HB_VER_REVISION", _HB_VER_REVISION )
   _BldInfo( "_HB_VER_LEX", _HB_VER_LEX )
   _BldInfo( "_HB_VER_AS_STRING", _HB_VER_AS_STRING )
   _BldInfo( "_HB_PCODE_VER", _HB_PCODE_VER )
   _BldInfo( "_HB_VER_COMPILER", _HB_VER_COMPILER )
   _BldInfo( "_HB_VER_PLATFORM", _HB_VER_PLATFORM )
   _BldInfo( "_HB_VER_BUILD_DATE", _HB_VER_BUILD_DATE )
   _BldInfo( "_HB_VER_BUILD_TIME", _HB_VER_BUILD_TIME )
   _BldInfo( "_HB_VER_LENTRY", _HB_VER_LENTRY )
   _BldInfo( "_HB_VER_CHLCVS", _HB_VER_CHLCVS )
   _BldInfo( "_HB_VER_C_USR", _HB_VER_C_USR )
   _BldInfo( "_HB_VER_L_USR", _HB_VER_L_USR )
   _BldInfo( "_HB_VER_PRG_USR", _HB_VER_PRG_USR )
   _BldInfo( "_HB_EXTENSION", _HB_EXTENSION )
   _BldInfo( "_HB_C52_UNDOC", _HB_C52_UNDOC )
   _BldInfo( "_HB_C52_STRICT", _HB_C52_STRICT )
   _BldInfo( "_HB_COMPAT_C53", _HB_COMPAT_C53 )
   _BldInfo( "_HB_COMPAT_XPP", _HB_COMPAT_XPP )
   _BldInfo( "_HB_COMPAT_VO", _HB_COMPAT_VO )
   _BldInfo( "_HB_COMPAT_FLAGSHIP", _HB_COMPAT_FLAGSHIP )
   _BldInfo( "_HB_COMPAT_FOXPRO", _HB_COMPAT_FOXPRO )
   _BldInfo( "_HB_COMPAT_DBASE", _HB_COMPAT_DBASE )
   _BldInfo( "_HB_HARBOUR_OBJ_GENERATION", _HB_HARBOUR_OBJ_GENERATION )
   _BldInfo( "_HB_HARBOUR_STRICT_ANSI_C", _HB_HARBOUR_STRICT_ANSI_C )
   _BldInfo( "_HB_CPLUSPLUS", _HB_CPLUSPLUS )
   _BldInfo( "_HB_HARBOUR_YYDEBUG", _HB_HARBOUR_YYDEBUG )
   _BldInfo( "_HB_SYMBOL_NAME_LEN", _HB_SYMBOL_NAME_LEN )
   _BldInfo( "_HB_MULTITHREAD", _HB_MULTITHREAD )
   _BldInfo( "_HB_VM_OPTIMIZATION", _HB_VM_OPTIMIZATION )
   _BldInfo( "_HB_LANG_ID", _HB_LANG_ID )
   _BldInfo( "_HB_ARRAY_MODE", _HB_ARRAY_MODE )
   _BldInfo( "_HB_CREDITS", _HB_CREDITS )

   RETURN

STATIC PROCEDURE _BldInfo( cInfo, nInfo )

   LOCAL uInfo := hb_BuildInfo( nInfo )

   ? PadR( cInfo, 26 ), ":", ValType( uInfo ), hb_ValToExp( uInfo )

   RETURN
