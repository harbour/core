/*
 * $Id$
 */

#include <itemapi.h>
#include <extend.h>
#include <errorapi.h>
#include <ctoharb.h>
#include <filesys.h>

#define BUFFER_SIZE 8096

static long hb_fsCopy(BYTEP ,BYTEP ) ;

HARBOUR HB___COPYFILE() {
    if ( ISCHAR(1) && ISCHAR(2) ) {
        hb_retnl(hb_fsCopy((BYTEP)hb_parc(1),(BYTEP)hb_parc(2)));
    }
    return;
} 

static long hb_fsCopy(BYTEP source,BYTEP dest) {
   FHANDLE dHANDLE,sHANDLE;
   char    *buffer;
   USHORT  usCount;
   ULONG   ulCount = 0L;
   sHANDLE = hb_fsOpen(source, FO_READ);
   if ( ! hb_fsError() ) {
       dHANDLE = hb_fsCreate(dest,FC_NORMAL);
       if ( hb_fsError() ) {
           hb_fsClose(sHANDLE);
           return( -2L) ;
       }
       buffer = hb_xgrab(BUFFER_SIZE);
       if (buffer == NULL) {
           hb_fsClose(sHANDLE);
           hb_fsClose(dHANDLE);
           return(-3L);
       }
       usCount = hb_fsRead(sHANDLE,buffer,BUFFER_SIZE);
       while(1) {
           ulCount += (ULONG)usCount;
           hb_fsWrite(dHANDLE,buffer, usCount);
           usCount = hb_fsRead(sHANDLE,buffer, BUFFER_SIZE);
           if (usCount != BUFFER_SIZE ) {
               break;
           }
       }
       ulCount += (ULONG)usCount;
       hb_fsWrite(dHANDLE,buffer,usCount);
       hb_xfree(buffer);
       hb_fsClose(sHANDLE);
       hb_fsClose(dHANDLE);
   }
   return( ulCount );
}
