/*
 * $Id$
 */

#include <itemapi.h>
#include <extend.h>
#include <errorapi.h>
#include <ctoharb.h>
#include <filesys.api>

#define BUFFER_SIZE 8096

static long _fsCopy(BYTEP ,BYTEP ) ;

HARBOUR hb___copyfile() {
    if ( ISCHAR(1) && ISCHAR(2) ) {
        hb_retnl(_fsCopy((BYTEP)hb_parc(1),(BYTEP)hb_parc(2)));
    }
    return;
} 

static long _fsCopy(BYTEP source,BYTEP dest) {
   FHANDLE dHANDLE,sHANDLE;
   char    *buffer;
   USHORT  usCount;
   ULONG   ulCount = 0L;
   sHANDLE = _fsOpen(source, FO_READ);
   if ( ! _fsError() ) {
       dHANDLE = _fsCreate(dest,FC_NORMAL);
       if ( _fsError() ) {
           _fsClose(sHANDLE);
           return( -2L) ;
       }
       buffer = hb_xgrab(BUFFER_SIZE);
       if (buffer == NULL) {
           _fsClose(sHANDLE);
           _fsClose(dHANDLE);
           return(-3L);
       }
       usCount = _fsRead(sHANDLE,buffer,BUFFER_SIZE);
       while(1) {
           ulCount += (ULONG)usCount;
           _fsWrite(dHANDLE,buffer, usCount);
           usCount = _fsRead(sHANDLE,buffer, BUFFER_SIZE);
           if (usCount != BUFFER_SIZE ) {
               break;
           }
       }
       ulCount += (ULONG)usCount;
       _fsWrite(dHANDLE,buffer,usCount);
       hb_xfree(buffer);
       _fsClose(sHANDLE);
       _fsClose(dHANDLE);
   }
   return( ulCount );
}
