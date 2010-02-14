/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    DLL call test.
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

#include "simpleio.ch"

#include "hbdyn.ch"

PROCEDURE Main()
   LOCAL cFileName
   LOCAL a

#if defined( __ARCH64BIT__ )
   cFileName := "test_x64.dll"
#else
   cFileName := "test_x86.dll"
#endif

   ? "-", cFileName
   a := NIL ; a := 567.89               ; ? ">", a, win_dllCall( { "TESTD" , cFileName, hb_bitOr( HB_DYN_CTYPE_DOUBLE         , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_DOUBLE         }, a )
   a := NIL ; a := 567.89               ; ? ">", a, win_dllCall( { "TESTF" , cFileName, hb_bitOr( HB_DYN_CTYPE_FLOAT          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_FLOAT          }, a )
   a := NIL ; a := -( 2 ^  7 )          ; ? ">", a, win_dllCall( { "TESTC" , cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR           , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_CHAR           }, a )
   a := NIL ; a :=  ( 2 ^  8 ) - 1      ; ? ">", a, win_dllCall( { "TESTUC", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_UNSIGNED  , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_CHAR_UNSIGNED  }, a )
   a := NIL ; a := -( 2 ^ 15 )          ; ? ">", a, win_dllCall( { "TESTS" , cFileName, hb_bitOr( HB_DYN_CTYPE_SHORT          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_SHORT          }, a )
   a := NIL ; a :=  ( 2 ^ 16 ) - 1      ; ? ">", a, win_dllCall( { "TESTUS", cFileName, hb_bitOr( HB_DYN_CTYPE_SHORT_UNSIGNED , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_SHORT_UNSIGNED }, a )
   a := NIL ; a := -( 2 ^ 31 )          ; ? ">", a, win_dllCall( { "TESTI" , cFileName, hb_bitOr( HB_DYN_CTYPE_INT            , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_INT            }, a )
   a := NIL ; a :=  ( 2 ^ 32 ) - 1      ; ? ">", a, win_dllCall( { "TESTUI", cFileName, hb_bitOr( HB_DYN_CTYPE_INT_UNSIGNED   , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_INT_UNSIGNED   }, a )
   a := NIL ; a := -( 2 ^ 31 )          ; ? ">", a, win_dllCall( { "TESTL" , cFileName, hb_bitOr( HB_DYN_CTYPE_LONG           , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LONG           }, a )
   a := NIL ; a :=  ( 2 ^ 32 ) - 1      ; ? ">", a, win_dllCall( { "TESTUL", cFileName, hb_bitOr( HB_DYN_CTYPE_LONG_UNSIGNED  , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LONG_UNSIGNED  }, a )
   a := NIL ; a := -( 2 ^ 63 )          ; ? ">", a, win_dllCall( { "TEST6" , cFileName, hb_bitOr( HB_DYN_CTYPE_LLONG          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LLONG          }, a )
   a := NIL ; a := 18446744073709600000 ; ? ">", a, win_dllCall( { "TESTU6", cFileName, hb_bitOr( HB_DYN_CTYPE_LLONG_UNSIGNED , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LLONG_UNSIGNED }, a )
   a := NIL ; a := "hello world!"       ; ? ">", a, win_dllCall( { "TESTST", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_PTR       , HB_DYN_CALLCONV_CDECL )                              }, a )
   ? "=="

   a := NIL ; a := "hello world!"       ; ? ">", a, win_dllCall( { "TESTST", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_PTR , HB_DYN_CALLCONV_CDECL, HB_DYN_ENC_RAW ), hb_bitOr( HB_DYN_CTYPE_CHAR_PTR, HB_DYN_ENC_RAW ) }, a )
   a := NIL ; a := "hello world!"       ; ? ">", a, win_dllCallFoxPro( "DECLARE STRING TESTST IN " + cFileName + " STRING", a )

   RETURN
