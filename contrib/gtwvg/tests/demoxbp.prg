//                     EnOnkar
//               ( The Lord is ONE )
//                        .
// Xbase++ Compatible XbpDialog() based Application
//
//       Pritpal Bedi <pritpal@vouchcac.com>
//                   2008-12-08

#require "gtwvg"

#include "inkey.ch"
#include "hbgtinfo.ch"

#define GetResource( x )  ( x )

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVG" )
#endif

   demoxbp()

   RETURN

#include "_xbp.prg"

#if ! defined( __HBSCRIPT__HBSHELL )

PROCEDURE hb_GTSYS()  /* must be a public function */

   REQUEST HB_GT_GUI_DEFAULT
   REQUEST HB_GT_WVG
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WGU

   RETURN

#endif
