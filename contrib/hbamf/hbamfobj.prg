/*
 * Harbour Project source code:
 * Basic routines for communications using AMFIO over standard IO pipes and TCP/IP
 *
 * Copyright 2011-2012 Aleksander Czajczynski <hb/at/fki.pl>
 * Copyright 2011 Ilina Stoilkovska <anili100/at/gmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"

CREATE CLASS amf_Obj

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

METHOD New( hCachedData ) CLASS amf_Obj

   ::hCachedData := hCachedData

   RETURN self

METHOD noMessage( ... ) CLASS amf_Obj

   RETURN ::msgNotFound( __GetMessage(), ... )

METHOD msgNotFound( cMessage, ... ) CLASS amf_Obj

   IF PCount() == 1 .AND. !( hb_BLeft( cMessage, 1 ) == "_" )
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

CREATE CLASS amf_Raw

   METHOD New( cData ) CONSTRUCTOR
   METHOD GetData INLINE ::cData

   PROTECTED:
   VAR cData

END CLASS

METHOD New( cData ) CLASS amf_Raw

   ::cData := cData

   RETURN self
