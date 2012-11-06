/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SIM interface code
 *
 * Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbsim.ch"

CREATE CLASS wce_sim

   VAR lInitialized INIT .F.
   VAR hSim
   VAR nLastError INIT SIM_E_OK

   METHOD New()
   METHOD End()

   METHOD lInitialize()   // Must be initialized before any other method
   METHOD lDeInitialize() // Must be deinitialized....

   METHOD lNumberOfPhoneBookEntries( nType, nTotal, nUsed ) // nTotal, nUsed both by reference
   METHOD aGetAllPhoneBookEntries( nType ) // -> array with phoneBook entries of nType storage
   METHOD lGetSimPhoneEntry( nPos, nType, aEntry ) // -> aEntry by refence contains phonebook entry
   METHOD lSetSimPhoneEntry( nPos, nType, cNumber, cName, nPlan, nAddrType ) // -> .T. / .F. if phonebook entry written
   METHOD lDelSimPhoneEntry( nPos, nType ) // -> .T. / .F. if phonebook entry deleted

ENDCLASS

METHOD New() CLASS wce_sim
   RETURN Self

METHOD lInitialize() CLASS wce_sim

   LOCAL nResult := wce_SimInitialize( @::hSim )

   ::lInitialized := ( nResult == SIM_E_OK )
   ::nLastError   := nResult

   RETURN ::lInitialized

METHOD lDeInitialize() CLASS wce_sim

   LOCAL nResult

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   nResult := wce_SimDeInitialize( ::hSim )

   ::lInitialized := !( nResult == SIM_E_OK )
   ::nLastError   := nResult

   RETURN nResult == SIM_E_OK

METHOD lNumberOfPhoneBookEntries( nType, /* @ */ nTotal, /* @ */ nUsed ) CLASS wce_sim

   LOCAL nResult

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   IF ! HB_ISNUMERIC( nType )
      nType := SIM_PBSTORAGE_SIM
   ENDIF

   nResult       := wce_SimPhonebookStatus( ::hSim, nType, @nTotal, @nUsed )
   ::nLastError  := nResult

   RETURN nResult == SIM_E_OK

METHOD aGetAllPhoneBookEntries( nType ) CLASS wce_sim

   LOCAL nResult
   LOCAL nTotal
   LOCAL nUsed
   LOCAL aEntry
   LOCAL aEntries
   LOCAL nPos

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   IF ! HB_ISNUMERIC( nType )
      nType := SIM_PBSTORAGE_SIM
   ENDIF
   IF ! HB_ISARRAY( aEntries )
      aEntries := {}
   ENDIF

   IF ! ::lNumberOfPhoneBookEntries( nType, @nTotal, @nUsed )
      RETURN {}
   ENDIF

   FOR nPos := 1 TO nUsed
      aEntry := {}
      nResult := wce_SimReadPhonebookEntry( ::hSim, nType, nPos, @aEntry )
      IF nResult != SIM_E_OK
         ::nLastError := nResult
         EXIT
      ELSE
         AAdd( aEntries, aEntry )
      ENDIF
   NEXT

   ::nLastError := nResult

   RETURN aEntries

METHOD lGetSimPhoneEntry( nPos, nType, /* @ */ aEntry ) CLASS wce_sim

   LOCAL nResult
   LOCAL a

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   IF ! HB_ISNUMERIC( nType )
      nType := SIM_PBSTORAGE_SIM
   ENDIF

   nResult := wce_SimReadPhonebookEntry( ::hSim, nType, nPos, @a )

   aEntry := { a }
   ::nLastError := nResult

   RETURN nResult == SIM_E_OK

METHOD lSetSimPhoneEntry( nPos, nType, cNumber, cName, nPlan, nAddrType ) CLASS wce_sim

   LOCAL nResult

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   IF ! HB_ISNUMERIC( nType )
      nPos := SIM_PBINDEX_FIRSTAVAILABLE
   ENDIF
   IF ! HB_ISNUMERIC( nType )
      nType := SIM_PBSTORAGE_SIM
   ENDIF

   nResult := wce_SimWritePhonebookEntry( ::hSim, nType, nPos, cNumber, cName, nPlan, nAddrType )
   ::nLastError := nResult

   RETURN nResult == SIM_E_OK

METHOD lDelSimPhoneEntry( nPos, nType ) CLASS wce_sim

   LOCAL nResult

   IF ! ::lInitialized
      ::nLastError := SIM_E_HB_NOTINITIALIZED
      RETURN .F.
   ENDIF

   IF ! HB_ISNUMERIC( nType )
      nType := SIM_PBSTORAGE_SIM
   ENDIF

   nResult := wce_SimDeletePhonebookEntry( ::hSim, nType, nPos )
   ::nLastError := nResult

   RETURN nResult == SIM_E_OK

METHOD End() CLASS wce_sim

   IF ::lInitialized
      ::lDeInitialize()
   ENDIF

   RETURN NIL
