/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SIM header
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

/* Error codes for SIM API */
#define SIM_E_OK                        0               // This is non standard...
#define SIM_E_HB_MISC                   -1              // This is non standard...
#define SIM_E_HB_NOTINITIALIZED         -10             // This is non standard (class tsim not initialized sim)
#define SIM_E_SIMFAILURE                -100            // SIM failure was detected
#define SIM_E_SIMBUSY                   -200            // SIM is busy
#define SIM_E_SIMWRONG                  -300            // Inorrect SIM was inserted
#define SIM_E_NOSIMMSGSTORAGE           -400            // SIM isn't capable of storing messages
#define SIM_E_SIMTOOLKITBUSY            -500            // SIM Application Toolkit is busy
#define SIM_E_SIMDOWNLOADERROR          -600            // SIM data download error
#define SIM_E_SIMNOTINSERTED            -700            // SIM isn't inserted into the phone
#define SIM_E_PHSIMPINREQUIRED          -800            // PH-SIM PIN is required to perform this operation
#define SIM_E_PHFSIMPINREQUIRED         -900            // PH-FSIM PIN is required to perform this operation
#define SIM_E_PHFSIMPUKREQUIRED         -1000           // PH-FSIM PUK is required to perform this operation
#define SIM_E_SIMPINREQUIRED            -1100           // SIM PIN is required to perform this operation
#define SIM_E_SIMPUKREQUIRED            -1200           // SIM PUK is required to perform this operation
#define SIM_E_INCORRECTPASSWORD         -1300           // Incorrect password was supplied
#define SIM_E_SIMPIN2REQUIRED           -1400           // SIM PIN2 is required to perform this operation
#define SIM_E_SIMPUK2REQUIRED           -1500           // SIM PUK2 is required to perform this operation
#define SIM_E_NETWKPINREQUIRED          -1600           // Network Personalization PIN is required to perform this operation
#define SIM_E_NETWKPUKREQUIRED          -1700           // Network Personalization PUK is required to perform this operation
#define SIM_E_SUBSETPINREQUIRED         -1800           // Network Subset Personalization PIN is required to perform this operation
#define SIM_E_SUBSETPUKREQUIRED         -1900           // Network Subset Personalization PUK is required to perform this operation
#define SIM_E_SVCPINREQUIRED            -2000           // Service Provider Personalization PIN is required to perform this operation
#define SIM_E_SVCPUKREQUIRED            -2100           // Service Provider Personalization PUK is required to perform this operation
#define SIM_E_CORPPINREQUIRED           -2200           // Corporate Personalization PIN is required to perform this operation
#define SIM_E_CORPPUKREQUIRED           -2300           // Corporate Personalization PUK is required to perform this operation
#define SIM_E_MEMORYFULL                -2400           // Storage memory is full
#define SIM_E_INVALIDINDEX              -2500           // Invalid storage index was supplied
#define SIM_E_NOTFOUND                  -2600           // A requested storage entry was not found
#define SIM_E_MEMORYFAILURE             -2700           // Storage memory failure
#define SIM_E_SIMMSGSTORAGEFULL         -2800           // Message storage on the SIM is full
#define SIM_E_EMPTYINDEX                -2900           // Storage location is empty
#define SIM_E_NOTREADY                  -3100           // SIM isn't yet ready to perform the requested operation
#define SIM_E_SECURITYFAILURE           -3200           // SIM isn't yet ready to perform the requested operation
#define SIM_E_BUFFERTOOSMALL            -3300           // Buffer too small
#define SIM_E_NOTTEXTMESSAGE            -3400           // Requested SMS message is not a text message
#define SIM_E_NOSIM                     -3500           // Device doesn't have a SIM
#define SIM_E_NETWORKERROR              -3600           // There was a network error
#define SIM_E_MOBILEERROR               -3700           // Mobile error
#define SIM_E_UNSUPPORTED               -3800           // The command is unsupported
#define SIM_E_BADPARAM                  -3900           // Bad parameter
#define SIM_E_UNDETERMINED              -4000           // Undetermined error
#define SIM_E_RADIONOTPRESENT           -4100           // The Radio is not present
#define SIM_E_RADIOOFF                  -4200           // The Radio is off

/* Phone book storage locations */
#define SIM_PBSTORAGE_EMERGENCY         0x00000001      // Emergency dial list
#define SIM_PBSTORAGE_FIXEDDIALING      0x00000002      // SIM fixed dialing list
#define SIM_PBSTORAGE_LASTDIALING       0x00000004      // SIM last dialing list
#define SIM_PBSTORAGE_OWNNUMBERS        0x00000008      // SIM ownnumbers lists
#define SIM_PBSTORAGE_SIM               0x00000010      // General SIM Storage
#define SIM_NUMPBSTORAGES               5               // Number of phonebook storages

/* Phonebook Misc | Special phonebook constants */
#define SIM_PBINDEX_FIRSTAVAILABLE      0xFFFFFFFF      // Use first phonebook storage entry available

/* Numbering Plan | Defines different numbering plans for SIM_ADDRTYPE_UNKNOWN,
              SIM_ADDRTYPE_INTERNATIONAL, and SIM_ADDRTYPE_NATIONAL */
#define SIM_NUMPLAN_UNKNOWN             0x00000000      // Unknown
#define SIM_NUMPLAN_TELEPHONE           0x00000001      // ISDN/telephone numbering plan (E.164/E.163)
#define SIM_NUMPLAN_DATA                0x00000002      // Data numbering plan (X.121)
#define SIM_NUMPLAN_TELEX               0x00000003      // Telex numbering plan
#define SIM_NUMPLAN_NATIONAL            0x00000004      // National numbering plan
#define SIM_NUMPLAN_PRIVATE             0x00000005      // Private numbering plan
#define SIM_NUMPLAN_ERMES               0x00000006      // ERMES numbering plan (ETSI DE/PS 3 01-3)

/* Address Type | Defines different address types */
#define SIM_ADDRTYPE_UNKNOWN            0x00000000      // Unknown
#define SIM_ADDRTYPE_INTERNATIONAL      0x00000001      // International number
#define SIM_ADDRTYPE_NATIONAL           0x00000002      // National number
#define SIM_ADDRTYPE_NETWKSPECIFIC      0x00000003      // Network specific number
#define SIM_ADDRTYPE_SUBSCRIBER         0x00000004      // Subscriber number (protocol-specific)
#define SIM_ADDRTYPE_ALPHANUM           0x00000005      // Alphanumeric address
#define SIM_ADDRTYPE_ABBREV             0x00000006      // Abbreviated number
