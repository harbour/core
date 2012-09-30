/*
 * $Id$
 */

#include "common.ch"

PROCEDURE Main( cNum, cPrec )

   DEFAULT cPrec TO Str( DEFAULT_PRECISION )
   QOut( ft_d2e( Val( cNum ), Val( cPrec ) ) )

   RETURN
