#require "hbnf"

PROCEDURE Main()

   LOCAL nLoaded := ft_IsShare()

   SWITCH nLoaded
   CASE 0   ; ? "Share not loaded, but ok to load" ; EXIT
   CASE 1   ; ? "Share not loaded, but NOT ok to load!" ; EXIT
   CASE 255 ; ? "Share is loaded!" ; EXIT
   ENDSWITCH

   ? "Retcode:", hb_ntos( nLoaded )

   RETURN
