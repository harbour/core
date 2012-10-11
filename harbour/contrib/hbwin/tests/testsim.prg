/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SIM test code
 *
 * Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
 * www - http://harbour-project.org
 */

#include "hbsim.ch"

PROCEDURE Main()

#if defined( __PLATFORM__WINCE )

   LOCAL o
   LOCAL l
   LOCAL nTotal, nUsed
   LOCAL a

   o := wce_sim():New()

   l := o:lInitialize()

   IF l
      Alert( "Api Sim initialized!!" + Str( o:nLastError ) )
   ELSE
      Alert( "Api Sim NOT initialized!!.  Error " + Str( o:nLastError ) )
   ENDIF

#if 0
   IF o:lNumberOfPhoneBookEntries( SIM_PBSTORAGE_SIM, @nTotal, @nUsed )
      Alert( "Total Entries: " + Str( nTotal ) + " ---- Used Entries: " + Str( nUsed ) )
   ELSE
      Alert( "Error " + Str( o:nLastError ) + " phone book entries" )
   ENDIF

   a := o:aGetAllPhoneBookEntries()
   AEval( a, {| n, m | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Adr. Type: " + Str( n[ 3 ] ) + hb_eol() + "Plan Type: " + Str( n[ 4 ] ) ) } )
#endif

   IF !o:lGetSimPhoneEntry( 1, SIM_PBSTORAGE_SIM, @a )
      Alert( "Pos 1 error" )
   ELSE
      Alert( "VALTYPE-> " + ValType( a ) + " LEN -> " + Str( Len(a ) ), a[ 1 ][ 1 ] )
      AEval( a, {| n, m | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Adr. Type: " + Str( n[ 3 ] ) + hb_eol() + "Plan Type: " + Str( n[ 4 ] ),"pos 1" ) } )
   ENDIF

   IF !o:lGetSimPhoneEntry( 110, SIM_PBSTORAGE_SIM, @a )
      Alert( "Pos 123 error" )
   ELSE
      AEval( a, {| n, m | Alert( "Phone: " + n[ 1 ] + hb_eol() + "Name: " + n[ 2 ] + hb_eol() + "Adr. Type: " + Str( n[ 3 ] ) + hb_eol() + "Plan Type: " + Str( n[ 4 ] ),"pos 110" ) } )
   ENDIF

   IF !o:lSetSimPhoneEntry( 80, SIM_PBSTORAGE_SIM, "660099696", "pepeluis", SIM_NUMPLAN_UNKNOWN, SIM_ADDRTYPE_NATIONAL )
      Alert( "Error writing pos 80 Errpr=>" + Str( o:nLastError ) )
   ENDIF
   IF !o:lDelSimPhoneEntry( 80, SIM_PBSTORAGE_SIM )
      Alert( "Error deletein pos 80 Errpr=>" + Str( o:nLastError ) )
   ENDIF

   l := o:lDeInitialize()
   IF !l
      Alert( "Not De-Initialized SIM api.  Error :" + Str( o:nLastError ) )
   ELSE
      Alert( "Api Sim Deinitialized!!!" )
   ENDIF

   o:End()

#endif

   RETURN
