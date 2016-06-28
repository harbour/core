/* Copyright 2011 Viktor Szakats (vszakats.net/harbour) */

#include "inkey.ch"

PROCEDURE Main()

   CLS

   hb_keyPut( { K_DOWN, K_UP } )
   AChoice( 0, 0, 0, 0, { "1", "2" } )

   RETURN
