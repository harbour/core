/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

PROCEDURE Main()

   ? OS()
   ?

   ? "2000OrUpper" , win_osIs2000OrUpper()
   ? "WinXPOrUpper", win_osIsWinXPOrUpper()
   ? "VistaOrUpper", win_osIsVistaOrUpper()

   ? "2000"        , win_osIs2000()
   ? "XP"          , win_osIsXP()
   ? "2003"        , win_osIs2003()
   ? "Vista"       , win_osIsVista()
   ? "7"           , win_osIs7()
   ? "8"           , win_osIs8()
   ? "81"          , win_osIs81()
   ? "10"          , win_osIs10()

   RETURN
