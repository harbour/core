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

#define _ISOK_( a, b ) iif( a == b, "OK", "WRONG" )

PROCEDURE Main()
   LOCAL cFileName
   LOCAL a, b

#if defined( __ARCH64BIT__ )
   cFileName := "test_x64.dll"
#else
   cFileName := "test_x86.dll"
#endif

   ? "-", cFileName
   a := 567.89               ; ? ">", a, b := win_dllCall( { "TESTD" , cFileName, hb_bitOr( HB_DYN_CTYPE_DOUBLE         , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_DOUBLE         }, a ), _ISOK_( a, b )
   a := 567.89               ; ? ">", a, b := win_dllCall( { "TESTF" , cFileName, hb_bitOr( HB_DYN_CTYPE_FLOAT          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_FLOAT          }, a ), _ISOK_( a, b )
   a := -( 2 ^  7 )          ; ? ">", a, b := win_dllCall( { "TESTC" , cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR           , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_CHAR           }, a ), _ISOK_( a, b )
   a :=  ( 2 ^  8 ) - 1      ; ? ">", a, b := win_dllCall( { "TESTUC", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_UNSIGNED  , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_CHAR_UNSIGNED  }, a ), _ISOK_( a, b )
   a := -( 2 ^ 15 )          ; ? ">", a, b := win_dllCall( { "TESTS" , cFileName, hb_bitOr( HB_DYN_CTYPE_SHORT          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_SHORT          }, a ), _ISOK_( a, b )
   a :=  ( 2 ^ 16 ) - 1      ; ? ">", a, b := win_dllCall( { "TESTUS", cFileName, hb_bitOr( HB_DYN_CTYPE_SHORT_UNSIGNED , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_SHORT_UNSIGNED }, a ), _ISOK_( a, b )
   a := -( 2 ^ 31 )          ; ? ">", a, b := win_dllCall( { "TESTI" , cFileName, hb_bitOr( HB_DYN_CTYPE_INT            , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_INT            }, a ), _ISOK_( a, b )
   a :=  ( 2 ^ 32 ) - 1      ; ? ">", a, b := win_dllCall( { "TESTUI", cFileName, hb_bitOr( HB_DYN_CTYPE_INT_UNSIGNED   , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_INT_UNSIGNED   }, a ), _ISOK_( a, b )
   a := -( 2 ^ 31 )          ; ? ">", a, b := win_dllCall( { "TESTL" , cFileName, hb_bitOr( HB_DYN_CTYPE_LONG           , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LONG           }, a ), _ISOK_( a, b )
   a :=  ( 2 ^ 32 ) - 1      ; ? ">", a, b := win_dllCall( { "TESTUL", cFileName, hb_bitOr( HB_DYN_CTYPE_LONG_UNSIGNED  , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LONG_UNSIGNED  }, a ), _ISOK_( a, b )
   a := -( 2 ^ 63 )          ; ? ">", a, b := win_dllCall( { "TEST6" , cFileName, hb_bitOr( HB_DYN_CTYPE_LLONG          , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LLONG          }, a ), _ISOK_( a, b )
   a := 18446744073709600000 ; ? ">", a, b := win_dllCall( { "TESTU6", cFileName, hb_bitOr( HB_DYN_CTYPE_LLONG_UNSIGNED , HB_DYN_CALLCONV_CDECL ), HB_DYN_CTYPE_LLONG_UNSIGNED }, a ), _ISOK_( a, b )
   a := "hello world!"       ; ? ">", a, b := win_dllCall( { "TESTST", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_PTR       , HB_DYN_CALLCONV_CDECL )                              }, a ), _ISOK_( a, b )
   ? "=="

   a := "hello world!"       ; ? ">", a, win_dllCall( { "TESTST", cFileName, hb_bitOr( HB_DYN_CTYPE_CHAR_PTR , HB_DYN_CALLCONV_CDECL, HB_DYN_ENC_RAW ), hb_bitOr( HB_DYN_CTYPE_CHAR_PTR, HB_DYN_ENC_RAW ) }, a )
   a := "hello world!"       ; ? ">", a, win_dllCallFoxPro( "DECLARE STRING TESTST IN " + cFileName + " STRING", a )

   RETURN
