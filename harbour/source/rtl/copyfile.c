/*
 * $Id$
 */

#include <itemapi.h>
#include <extend.h>
#include <errorapi.h>
#include <ctoharb.h>
#include <filesys.h>

#ifdef OS_UNIX_COMPATIBLE
   #include <sys/stat.h>
   #include <unistd.h>
#endif

#define BUFFER_SIZE 8192

static BOOL hb_fsCopy(char* szSource, char* szDest, ULONG* ulWrittenTotal);

/* INCOMPATIBILITY: Clipper returns .F. on failure and NIL on success */
/* TODO: hb_errorRT_BASE() or a replacement should also handle DOS error */
/*       and canRetry/canDefault flags */

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
      hb_errorRT_BASE(EG_ARG, 2010, "Argument error", "__COPYFILE");
   }
}

static BOOL hb_fsCopy(char* szSource, char* szDest, ULONG* ulWrittenTotal)

{
   BOOL bRetVal = FALSE;
   FHANDLE fhndSource;
   FHANDLE fhndDest;

   *ulWrittenTotal = 0L;

   while ((fhndSource = hb_fsOpen(szSource, FO_READ)) == FS_ERROR)
   {
      if (hb_errorRT_BASE(EG_ARG, 2012, "Open error", szSource) == E_DEFAULT)
      {
         *ulWrittenTotal = (ULONG)-1L;
         break;
      }
   }

   if (fhndSource != FS_ERROR)
   {
      while ((fhndDest = hb_fsCreate(szDest, FC_NORMAL)) == FS_ERROR)
      {
         if (hb_errorRT_BASE(EG_ARG, 2012, "Create error", szDest) == E_DEFAULT)
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
               if (hb_errorRT_BASE(EG_ARG, 2012, "Write error", szDest) == E_DEFAULT)
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

