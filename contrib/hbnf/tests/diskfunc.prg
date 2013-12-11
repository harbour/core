#require "hbnf"

PROCEDURE Main( cDrv )

   hb_default( @cDrv, hb_CurDrive() )

   ? "Disk size: ", hb_ntos( ft_DskSize( cDrv ) )
   ? "Free bytes:", hb_ntos( ft_DskFree( cDrv ) )

   RETURN
