#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL nver, nmar, ntype, nir, ppp

   nmar := ft_MVersion( @nver, @ntype, @nir )
   ppp := nmar + nver
   ? hb_ntos( nmar ) + "." + hb_ntos( nver )
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

   DO WHILE LastKey() != K_ESC
      ? "mouse row is", ft_MGetX()
      ? "mouse col is", ft_MGetY()
      IF LastKey() == K_ESC
         EXIT
      ENDIF
   ENDDO
   ft_MHideCrs()

   RETURN
