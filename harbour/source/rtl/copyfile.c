/*
 * $Id$

   Copyright(C) 1999 by Andi Jahja

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: andij@aonlippo.co.id
*/

#include "itemapi.h"
#include "extend.h"
#include "errorapi.h"
#include "ctoharb.h"
#include "filesys.h"
#include "init.h"

#ifdef OS_UNIX_COMPATIBLE
   #include <sys/stat.h>
   #include <unistd.h>
#endif

#define BUFFER_SIZE 8192

HARBOUR HB___COPYFILE(void);

HB_INIT_SYMBOLS_BEGIN( CopyFile__InitSymbols )
{ "__COPYFILE", FS_PUBLIC, HB___COPYFILE, 0 }
HB_INIT_SYMBOLS_END( CopyFile__InitSymbols );
#if ! defined(__GNUC__)
   #pragma CopyFile__InitSymbols
#endif

static BOOL hb_fsCopy(char* szSource, char* szDest, ULONG* ulWrittenTotal);

/* INCOMPATIBILITY: Clipper returns .F. on failure and NIL on success */

HARBOUR HB___COPYFILE( void )
{
   if ( ISCHAR(1) && ISCHAR(2) )
   {
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      if (!hb_fsCopy(hb_parc(1), hb_parc(2)))
      {
         hb_retl(FALSE);
      }
#else
      ULONG ulWrittenTotal;
      hb_fsCopy(hb_parc(1), hb_parc(2), &ulWrittenTotal);
      hb_retnl(ulWrittenTotal);
#endif
   }
   else
   {
      hb_errorRT_BASE(EG_ARG, 2010, NULL, "__COPYFILE");
   }
}

static BOOL hb_fsCopy(char* szSource, char* szDest, ULONG* ulWrittenTotal)
{
   BOOL bRetVal = FALSE;
   FHANDLE fhndSource;
   FHANDLE fhndDest;

   *ulWrittenTotal = 0L;

   while ((fhndSource = hb_fsOpen((BYTEP)szSource, FO_READ)) == FS_ERROR)
   {
      if (hb_errorRT_BASE_Ext1(EG_OPEN, 2012, NULL, szSource, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY ) == E_DEFAULT)
      {
         *ulWrittenTotal = (ULONG)-1L;
         break;
      }
   }

   if (fhndSource != FS_ERROR)
   {
      while ((fhndDest = hb_fsCreate((BYTEP)szDest, FC_NORMAL)) == FS_ERROR)
      {
         if (hb_errorRT_BASE_Ext1(EG_CREATE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY ) == E_DEFAULT)
         {
            *ulWrittenTotal = (ULONG)-2L;
            break;
         }
      }

      if (fhndDest != FS_ERROR)
      {
#ifdef OS_UNIX_COMPATIBLE
         struct stat struFileInfo;
         int iSuccess = fstat( fhndSource, &struFileInfo );
#endif
         PBYTE buffer;
         USHORT usRead;
         USHORT usWritten;

         buffer = (PBYTE)hb_xgrab( BUFFER_SIZE );

         /* QUESTION: Does Clipper throw an error on read or write operation ? */
         /* QUESTION: What is the E_DEFAULT behaviour on that error ? */

         bRetVal = TRUE;

         while ((usRead = hb_fsRead(fhndSource, buffer, BUFFER_SIZE)) != 0)
         {
            while ((usWritten = hb_fsWrite(fhndDest, buffer, usRead)) != usRead)
            {
               if (hb_errorRT_BASE_Ext1(EG_WRITE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY ) == E_DEFAULT)
               {
                  bRetVal = FALSE;
                  break;
               }
            }

            *ulWrittenTotal += (ULONG)usWritten;
         }

         hb_xfree(buffer);

#ifdef OS_UNIX_COMPATIBLE
         if( iSuccess == 0 )
            fchmod( fhndDest, struFileInfo.st_mode );
#endif

         hb_fsClose(fhndDest);
      }

      hb_fsClose(fhndSource);
   }

   return bRetVal;
}

