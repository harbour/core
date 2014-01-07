/*
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
 * See COPYING.txt for licensing terms.
 */

#include "hblang.ch"

PROCEDURE Main( cLng )

   LOCAL i, a, aDay, aMnt, aErr, aInt, cDtFrm, cTrue, cFalse

   hb_langSelect( "EN" )
   aDay := GET_DAYS()
   aMnt := GET_MONTHS()
   aErr := GET_ERR()
   aInt := GET_IERR()
   cDtFrm := hb_langMessage( HB_LANG_ITEM_BASE_TEXT )
   cTrue := hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 )
   cFalse := hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 )

   IF ! Empty( cLng )
      hb_langSelect( Upper( cLng ) )
   ENDIF

   ? hb_langName()
   ? "HB_LANG_ITEM_BASE_ID     ", "[" + hb_langMessage( HB_LANG_ITEM_BASE_ID     ) + "]"
   ? "HB_LANG_ITEM_BASE_MONTH  ", "[" + hb_langMessage( HB_LANG_ITEM_BASE_MONTH  ) + "]"
   ? "HB_LANG_ITEM_BASE_DAY    ", "[" + hb_langMessage( HB_LANG_ITEM_BASE_DAY    ) + "]"
   ? "HB_LANG_ITEM_BASE_NATMSG ", "[" + hb_langMessage( HB_LANG_ITEM_BASE_NATMSG ) + "]"
   ? "HB_LANG_ITEM_BASE_ERRDESC", "[" + hb_langMessage( HB_LANG_ITEM_BASE_ERRDESC ) + "]"
   ? "HB_LANG_ITEM_BASE_ERRINTR", "[" + hb_langMessage( HB_LANG_ITEM_BASE_ERRINTR ) + "]"
   ? "HB_LANG_ITEM_BASE_TEXT   ", "[" + hb_langMessage( HB_LANG_ITEM_BASE_TEXT   ) + "]"
   ? "hb_langErrMsg(0)", hb_langErrMsg( 0 )
   ?
   ? "date format:", hb_langMessage( HB_LANG_ITEM_BASE_TEXT ) + " (" + cDtFrm + ")"
   ? "   TRUE val:", hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + " (" + cTrue + ")"
   ? "  FALSE val:", hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 ) + " (" + cFalse + ")"
   ?
   Inkey( 0 )

   ? "Errors:"
   ? "-------"
   a := GET_ERR()
   FOR i := 1 TO Len( a )
      ? PadR( a[ i, 1 ], 15 ) + "|" + PadR( aErr[ i, 2 ], 30 ) + "|" + PadR( a[ i, 2 ], 32 )
   NEXT
   ?
   Inkey( 0 )

   ? "Internal errors:"
   ? "----------------"
   a := GET_IERR()
   FOR i := 1 TO Len( a )
      ? PadR( a[ i, 1 ], 15 ) + "|" + PadR( aInt[ i, 2 ], 30 ) + "|" + PadR( a[ i, 2 ], 32 )
   NEXT
   ?
   Inkey( 0 )

   ? "Days:"
   ? "-----"
   a := GET_DAYS()
   FOR i := 1 TO Len( a )
      ? "  " + PadR( aDay[ i ], 15 ) + a[ i ]
   NEXT
   ?
   Inkey( 0 )

   ? "Months:"
   ? "-------"
   a := GET_MONTHS()
   FOR i := 1 TO Len( a )
      ? "  " + PadR( aMnt[ i ], 15 ) + a[ i ]
   NEXT
   ?
   Inkey( 0 )

   RETURN

STATIC FUNCTION GET_DAYS()

   LOCAL i, n, aDays[ 7 ], dt := Date()

   FOR i := 1 TO 7
      n := DoW( dt )
      aDays[ n ] := CDoW( dt )
      ++dt
   NEXT

   RETURN aDays

STATIC FUNCTION GET_MONTHS()

   LOCAL i, n, aMonths[ 12 ], dt := Date()

   dt -= Day( dt ) - 1
   FOR i := 1 TO 12
      n := Month( dt )
      aMonths[ n ] := CMonth( dt )
      dt += 31
      dt -= Day( dt ) - 1
   NEXT

   RETURN aMonths

STATIC FUNCTION GET_ERR()
   RETURN { ;
      { "EG_ARG         ", hb_langErrMsg(  1 ) }, ;
      { "EG_BOUND       ", hb_langErrMsg(  2 ) }, ;
      { "EG_STROVERFLOW ", hb_langErrMsg(  3 ) }, ;
      { "EG_NUMOVERFLOW ", hb_langErrMsg(  4 ) }, ;
      { "EG_ZERODIV     ", hb_langErrMsg(  5 ) }, ;
      { "EG_NUMERR      ", hb_langErrMsg(  6 ) }, ;
      { "EG_SYNTAX      ", hb_langErrMsg(  7 ) }, ;
      { "EG_COMPLEXITY  ", hb_langErrMsg(  8 ) }, ;
      { "               ", hb_langErrMsg(  9 ) }, ;
      { "               ", hb_langErrMsg( 10 ) }, ;
      { "EG_MEM         ", hb_langErrMsg( 11 ) }, ;
      { "EG_NOFUNC      ", hb_langErrMsg( 12 ) }, ;
      { "EG_NOMETHOD    ", hb_langErrMsg( 13 ) }, ;
      { "EG_NOVAR       ", hb_langErrMsg( 14 ) }, ;
      { "EG_NOALIAS     ", hb_langErrMsg( 15 ) }, ;
      { "EG_NOVARMETHOD ", hb_langErrMsg( 16 ) }, ;
      { "EG_BADALIAS    ", hb_langErrMsg( 17 ) }, ;
      { "EG_DUPALIAS    ", hb_langErrMsg( 18 ) }, ;
      { "EG_NOOBJECT    ", hb_langErrMsg( 19 ) }, ;
      { "EG_CREATE      ", hb_langErrMsg( 20 ) }, ;
      { "EG_OPEN        ", hb_langErrMsg( 21 ) }, ;
      { "EG_CLOSE       ", hb_langErrMsg( 22 ) }, ;
      { "EG_READ        ", hb_langErrMsg( 23 ) }, ;
      { "EG_WRITE       ", hb_langErrMsg( 24 ) }, ;
      { "EG_PRINT       ", hb_langErrMsg( 25 ) }, ;
      { "               ", hb_langErrMsg( 26 ) }, ;
      { "               ", hb_langErrMsg( 27 ) }, ;
      { "               ", hb_langErrMsg( 28 ) }, ;
      { "               ", hb_langErrMsg( 29 ) }, ;
      { "EG_UNSUPPORTED ", hb_langErrMsg( 30 ) }, ;
      { "EG_LIMIT       ", hb_langErrMsg( 31 ) }, ;
      { "EG_CORRUPTION  ", hb_langErrMsg( 32 ) }, ;
      { "EG_DATATYPE    ", hb_langErrMsg( 33 ) }, ;
      { "EG_DATAWIDTH   ", hb_langErrMsg( 34 ) }, ;
      { "EG_NOTABLE     ", hb_langErrMsg( 35 ) }, ;
      { "EG_NOORDER     ", hb_langErrMsg( 36 ) }, ;
      { "EG_SHARED      ", hb_langErrMsg( 37 ) }, ;
      { "EG_UNLOCKED    ", hb_langErrMsg( 38 ) }, ;
      { "EG_READONLY    ", hb_langErrMsg( 39 ) }, ;
      { "EG_APPENDLOCK  ", hb_langErrMsg( 40 ) }, ;
      { "EG_LOCK        ", hb_langErrMsg( 41 ) }, ;
      { "               ", hb_langErrMsg( 42 ) }, ;
      { "               ", hb_langErrMsg( 43 ) }, ;
      { "               ", hb_langErrMsg( 44 ) }, ;
      { "               ", hb_langErrMsg( 45 ) }, ;
      { "EG_ARRACCESS   ", hb_langErrMsg( 46 ) }, ;
      { "EG_ARRASSIGN   ", hb_langErrMsg( 47 ) }, ;
      { "EG_ARRDIMENSION", hb_langErrMsg( 48 ) }, ;
      { "EG_NOTARRAY    ", hb_langErrMsg( 49 ) }, ;
      { "EG_CONDITION   ", hb_langErrMsg( 50 ) } }

STATIC FUNCTION HB_LANGERRINTR( n )
   RETURN hb_langMessage( HB_LANG_ITEM_BASE_ERRINTR + n - 9000 )

STATIC FUNCTION GET_IERR()
   RETURN { ;
      { "HB_EI_ERRUNRECOV      ", HB_LANGERRINTR( 9000 ) }, ;
      { "HB_EI_ERRRECFAILURE   ", HB_LANGERRINTR( 9001 ) }, ;
      { "HB_EI_ERRNOBLOCK      ", HB_LANGERRINTR( 9002 ) }, ;
      { "HB_EI_ERRTOOMANY      ", HB_LANGERRINTR( 9003 ) }, ;
      { "HB_EI_RDDINVALID      ", HB_LANGERRINTR( 9004 ) }, ;
      { "HB_EI_CLSINVMETHOD    ", HB_LANGERRINTR( 9005 ) }, ;
      { "HB_EI_XGRABALLOC      ", HB_LANGERRINTR( 9006 ) }, ;
      { "HB_EI_XREALLOCNULL    ", HB_LANGERRINTR( 9007 ) }, ;
      { "HB_EI_XREALLOCINV     ", HB_LANGERRINTR( 9008 ) }, ;
      { "HB_EI_XREALLOC        ", HB_LANGERRINTR( 9009 ) }, ;
      { "HB_EI_XFREEINV        ", HB_LANGERRINTR( 9010 ) }, ;
      { "HB_EI_XFREENULL       ", HB_LANGERRINTR( 9011 ) }, ;
      { "HB_EI_VMBADSTARTUP    ", HB_LANGERRINTR( 9012 ) }, ;
      { "HB_EI_VMNOSTARTUP     ", HB_LANGERRINTR( 9013 ) }, ;
      { "HB_EI_VMBADOPCODE     ", HB_LANGERRINTR( 9014 ) }, ;
      { "HB_EI_VMNOTSYMBOL     ", HB_LANGERRINTR( 9015 ) }, ;
      { "HB_EI_VMINVSYMBOL     ", HB_LANGERRINTR( 9016 ) }, ;
      { "HB_EI_VMNOTCBLOCK     ", HB_LANGERRINTR( 9017 ) }, ;
      { "HB_EI_VMPOPINVITEM    ", HB_LANGERRINTR( 9018 ) }, ;
      { "HB_EI_STACKUFLOW      ", HB_LANGERRINTR( 9019 ) }, ;
      { "HB_EI_ITEMBADCOPY     ", HB_LANGERRINTR( 9020 ) }, ;
      { "HB_EI_MVBADSYMBOL     ", HB_LANGERRINTR( 9021 ) }, ;
      { "HB_EI_XMEMOVERFLOW    ", HB_LANGERRINTR( 9022 ) }, ;
      { "HB_EI_XGRABNULLSIZE   ", HB_LANGERRINTR( 9023 ) }, ;
      { "HB_EI_XREALLOCNULLSIZE", HB_LANGERRINTR( 9024 ) }, ;
      { "HB_EI_XALLOCNULLSIZE  ", HB_LANGERRINTR( 9025 ) } }
