/*
 * Harbour Project source code:
 * SIM test code
 *
 * Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
 * www - http://harbour-project.org
 */

#require "hbwin"

#include "hbsim.ch"

PROCEDURE Main()

#if defined( __PLATFORM__WINCE )

   LOCAL o := wce_Sim():New()
   LOCAL a

   IF o:lInitialize()
      Alert( "API SIM initialized!! " + hb_ntos( o:nLastError ) )
   ELSE
      Alert( "API SIM not initialized!! Error " + hb_ntos( o:nLastError ) )
   ENDIF

#if 0
   IF o:lNumberOfPhoneBookEntries( SIM_PBSTORAGE_SIM, @nTotal, @nUsed )
      Alert( "Total Entries: " + hb_ntos( nTotal ) + " ---- Used Entries: " + hb_ntos( nUsed ) )
   ELSE
      Alert( "Error " + hb_ntos( o:nLastError ) + " phone book entries" )
   ENDIF

   a := o:aGetAllPhoneBookEntries()
   AEval( a, {| n | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Addr. Type: " + hb_ntos( n[ 3 ] ) + hb_eol() + "Plan Type: " + hb_ntos( n[ 4 ] ) ) } )
#endif

   IF ! o:lGetSimPhoneEntry( 1, SIM_PBSTORAGE_SIM, @a )
      Alert( "Pos 1 error" )
   ELSE
      Alert( "ValType() -> " + ValType( a ) + " Len() -> " + hb_ntos( Len( a ) ), a[ 1 ][ 1 ] )
      AEval( a, {| n | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Addr. Type: " + hb_ntos( n[ 3 ] ) + hb_eol() + "Plan Type: " + hb_ntos( n[ 4 ] ), "pos 1" ) } )
   ENDIF

   IF ! o:lGetSimPhoneEntry( 110, SIM_PBSTORAGE_SIM, @a )
      Alert( "Pos 123 error" )
   ELSE
      AEval( a, {| n | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Addr. Type: " + hb_ntos( n[ 3 ] ) + hb_eol() + "Plan Type: " + hb_ntos( n[ 4 ] ), "pos 110" ) } )
   ENDIF

   IF ! o:lSetSimPhoneEntry( 80, SIM_PBSTORAGE_SIM, "660099696", "pepeluis", SIM_NUMPLAN_UNKNOWN, SIM_ADDRTYPE_NATIONAL )
      Alert( "Error writing pos 80 Error => " + hb_ntos( o:nLastError ) )
   ENDIF
   IF ! o:lDelSimPhoneEntry( 80, SIM_PBSTORAGE_SIM )
      Alert( "Error deleting pos 80 Error => " + hb_ntos( o:nLastError ) )
   ENDIF

   IF ! o:lDeInitialize()
      Alert( "Not de-initialized SIM API. Error: " + hb_ntos( o:nLastError ) )
   ELSE
      Alert( "API SIM deinitialized!!!" )
   ENDIF

   o:End()

#else

   ? "This test requires WinCE to run."

#endif

   RETURN
