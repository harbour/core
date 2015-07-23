/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   ? OS()
   ?

   ? "CurrentVersion", win_regRead( "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\CurrentVersion" )
   ? "CurrentBuild"  , win_regRead( "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\CurrentBuild" )
   ?

   ? "2000OrUpper"   , win_osIs2000OrUpper()
   ? "WinXPOrUpper"  , win_osIsWinXPOrUpper()
   ? "VistaOrUpper"  , win_osIsVistaOrUpper()
   ?

   ? "2000"          , win_osIs2000()
   ? "XP"            , win_osIsXP()
   ? "2003"          , win_osIs2003()
   ? "Vista"         , win_osIsVista()
   ? "7"             , win_osIs7()
   ? "8"             , win_osIs8()
   ? "81"            , win_osIs81()
   ? "10"            , win_osIs10()
   ?

   ? "95"            , win_osIs95()
   ? "98"            , win_osIs98()
   ? "9x"            , win_osIs9x()
   ? "ME"            , win_osIsME()
   ? "NT"            , win_osIsNT()
   ? "NT351"         , win_osIsNT351()
   ? "NT4"           , win_osIsNT4()
   ? "TSClient"      , win_osIsTSClient()
   ?

   ? hb_ValToExp( win_osVersionInfo() )

   RETURN
