/*
 * $Id$
 */

#require "hbnf"

#include "common.ch"

PROCEDURE Main( cNum, cPrec )

   DEFAULT cPrec TO 6

   ? ft_d2e( Val( cNum ), Val( cPrec ) )

   RETURN
