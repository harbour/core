#require "hbnf"

PROCEDURE Main( cDrv )

   ? "Disk size:   " + Str( ft_DskSize( cDrv ) )
   ? "Free bytes:  " + Str( ft_DskFree( cDrv ) )

   RETURN
