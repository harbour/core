/*
 * $Id$
 */

#require "hbnf"

#include "common.ch"

PROCEDURE Main( cNum, cPrec )

   DEFAULT cPrec TO 6

   QOut( ft_d2e( Val( cNum ), Val( cPrec ) ) )

   RETURN
