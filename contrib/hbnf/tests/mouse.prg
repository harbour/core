#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL nVer, nMar, nType, nir, ppp

   nMar := ft_MVersion( @nVer, @nType, @nir )
   ppp := nMar + nVer
   ? hb_ntos( nMar ) + "." + hb_ntos( nVer )
   ? ppp / 100
   Inkey( 0 )
   ? "is mouse on", ft_MReset()
   Inkey( 0 )
   ? ft_MShowCrs()
   Inkey( 0 )
   ? ft_MXLimit( 0, 8 * MaxCol() )
   Inkey( 0 )
   ? ft_MYLimit( 0, 8 * MaxRow() )
   Inkey( 0 )

   DO WHILE Inkey() != K_ESC
      ? "mouse row is", ft_MGetX()
      ? "mouse col is", ft_MGetY()
   ENDDO
   ft_MHideCrs()

   RETURN
