/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL nver, nmar, ntype, nir, ppp

   nmar := FT_MVERSION( @nver, @ntype, @nir )
   ppp := nmar + nver
   ? Str( nmar, 2, 0 ), '.', Str( nver, 2, 0 )
   ? ppp/100
   Inkey( 0 )
   ? 'is mouse on', ft_mreset()
   Inkey( 0 )
   ? FT_MSHOWCRS()
   Inkey( 0 )
   ? ft_mxlimit( 0, 8 * MaxCol() )
   Inkey( 0 )
   ? ft_mylimit( 0, 8 * MaxRow() )
   Inkey( 0 )

   DO WHILE LastKey() != 27
      ? 'mouse row is', ft_mgetx()
      ? 'mouse col is', ft_mgety()
      IF LastKey() == 27
         EXIT
      ENDIF
   ENDDO
   FT_MHIDECRS()

   RETURN
