/*
 * $Id$
 */

/*******
*
*  Ilina Stoilkovska <anili100/at/gmail.com> 2011
*  Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
*
*  Basic routines for communications using AMFIO
*  over standard IO pipes and TCP/IP
*
********/

#include "hbclass.ch"

CREATE CLASS AMF_Obj

   METHOD New( hCachedData ) CONSTRUCTOR
   ERROR HANDLER noMessage
   METHOD msgNotFound

   PROTECTED:

   VAR nVersion INIT 0
   VAR cRealClass
   VAR hCachedData
   VAR nRpcOid
   VAR pConnection
   VAR nID

   EXPORTED:

   ACCESS RealClass INLINE ::cRealClass
   ACCESS RpcOid INLINE ::nRpcOid

END CLASS

METHOD New( hCachedData ) CLASS AMF_Obj

   ::hCachedData := hCachedData

   RETURN self

METHOD noMessage( ... ) CLASS AMF_Obj

   RETURN ::msgNotFound( __GetMessage(), ... )

METHOD msgNotFound( cMessage, ... ) CLASS AMF_Obj

   IF PCount() = 1 .AND. !( hb_BLeft( cMessage, 1 ) == "_" )
      IF ! Empty( ::hCachedData ) .AND. hb_HHasKey( ::hCachedData, cMessage )
         RETURN ::hCachedData[ cMessage ]
      ENDIF
   ELSEIF PCount() > 1 .AND. hb_BLeft( cMessage, 1 ) == "_"
      IF Empty( ::hCachedData )
         ::hCachedData := { => }
      ENDIF
      RETURN ::hCachedData[ hb_BSubStr( cMessage, 2 ) ] := hb_PValue( 2 )
   ENDIF

   RETURN NIL

CREATE CLASS AMF_Raw

   METHOD New( cData ) CONSTRUCTOR
   METHOD GetData INLINE ::cData

   PROTECTED:
   VAR cData

END CLASS

METHOD New( cData ) CLASS AMF_Raw

   ::cData := cData

   RETURN self
