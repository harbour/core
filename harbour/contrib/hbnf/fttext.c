/*
 * $Id$
 */

/*
 * File......: text.c
 * Author....: Brice de Ganahl and Steve Larsen
 * CIS ID....: 76370,1532
 *
 * This is an original work by Brice de Ganahl and Steve Larsen
 * and is placed in the public domain.
 *
 * Doc headers by Glenn Scott, Don Caton, and Steve Larsen
 *
 * Extensively revised by Steve Larsen
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.8   01 May 1995 04:36:22   TED
 * Major overhaul by Steve Larsen to fix several bugs/quirkiness,
 * add some requested features, clean up source for readability.
 *
 * -  Added ft_fError() test
 * -  Added ft_fBOF() test
 * -  Provided protected mode compatibility
 * -  Increased buffer to 4k, added logic to allow lines longer than
 *    the buffer size.
 * -  Revised seek logic
 * -  Changed undocumented calls to API functions wherever possible
 *
 *
 *    Rev 1.7   17 Oct 1992 16:25:16   GLENN
 * Leo cleaned up the documentation, including an errant SEEALSO
 * reference.
 *
 *    Rev 1.6   03 Oct 1992 02:07:38   GLENN
 * Minor adjustments to file header block.
 *
 *    Rev 1.5   03 Oct 1992 02:03:44   GLENN
 * Major modifications by Steve Larsen, as follows:
 *
 * Brice laid some wonderful groundwork with his initial release of
 * these functions, however I needed more capability.  With his per-
 * mission, I have made the following additions/changes to Rev. 1.4:
 *
 * -  Eliminated the problem of memory for buffers being re-allocated every
 *    time a file got used.
 * -  Further reduced memory impact by converting from extend system memory
 *    allocation techniques to virtual memory.  To accomplish this, we
 *    use the Clipper v5.01 r1.29 variants of the "_v" undocumented
 *    internal functions.  If these functions change in future releases, you
 *    will need to locate them herein and make the appropriate changes.
 *
 *    NOTE: these functions allocate and deallocate virtual memory on an
 *    "as-needed" basis.  If your application makes heavy and frequent use
 *    of those functions that perform a lot of buffering (ft_fInsert(),
 *    ft_fDelete() and ft_fWrite()), you might consider modifying the memory
 *    management scheme used herein, that is, allocate the required buffers
 *    only once upon the first call to these functions, then recycle them.
 * -  Added the ability to specify file open mode.
 * -  Added a function to write to a record, which through a switch can either
 *    over-write the current record, or insert a new one.
 * -  Added functions to insert, delete and append a specified number of lines.
 * -  Fixed the existing functions so that they properly handle "trailers",
 *    that is, a case where the last chars in a file are not CRLF delimited.
 * -  Provided checking for the possibility that the file might be terminated
 *    with ^Z (1Ah), if so, ignoring it (providing consistency with non-^Z
 *    terminated files).  This only occurs on the last record of a file.
 * -  Eliminated a potential problem if one were to issue an ft_fUse() prior
 *    actually opening any files.
 * -  Replaced the original C parsing logic to determine the end-of-line (CRLF)
 *    with an optimized assembler routine.  This bypassed a significant
 *    performance hit.
 * -  The original header (FTTEXT.h) file in now incorporated in this one file.
 *    This is not necessarily an enhancement, more like laziness.
 * -  Provided the (followup) author with his very first C experience!
 *
 *    Steve Larsen, Dec. 7, 1991   CIS 76370,1532
 *
 * -  Function changes/additions (refer to the individual doc headers for
 *    details):
 *
 *    FT_FSELECT( [ < nArea  > ] )                 -> nArea
 *    FT_FUSE(    [ < cFile  > ][, < nMode >   ] ) -> nHandle | NIL
 *    FT_FWRITELN(  < cData  >  [, < lInsert > ] ) -> NIL
 *    FT_FINSERT( [ < nLines > ] )                 -> NIL
 *    FT_FDELETE( [ < nLines > ] )                 -> NIL
 *    FT_FAPPEND( [ < nLines > ] )                 -> NIL
 *
 *    Internal Steve Larsen revisions:
 *
 *     12/07/91  Original rework
 *     02/13/92  Fixed _findeol(), FT_FREADLN() and FT_FGOBOT() to
 *               better handle files with CRLF, LF, ^Z or nothing
 *               at the EOF.  Previously, under some conditions the
 *               last record was chopped by a character, depending
 *               on the last character(s).
 *     05/02/92  Fixed buffering and VMM allocation problem with
 *               FT_FGOBOT().
 *     08/26/92  Correcting problem when appending blank lines to an
 *               empty file (ft_fAppend() and ft_fWriteLn()).
 *
 *
 *    Rev 1.4   17 Aug 1991 15:31:08   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.3   15 Aug 1991 23:08:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   29 Apr 1991 08:02:12   GLENN
 * Minor adjustments to documentation block
 *
 *    Rev 1.1   29 Apr 1991 08:00:26   GLENN
 * ft_flastrec() -- name was longer than 10 characters so linkers couldn't
 * find the symbol.  Just hacked off the last "c" so it is really
 * ft_flastre().  Sorry, folks.  -- Glenn
 *
 *    Rev 1.0   01 Apr 1991 01:02:48   GLENN
 * Nanforum Toolkit
 *
 */

/*  Notes:

     The Clipper internal functions used seem to be stable across
     versions but nothing is guaranteed.  These functions begin
     with _t, are used for file I/O, and are compatible with their
     ANSI counterparts (just strip the _t and you have the ANSI name).
     See text.h for the prototypes.

     This revision utilizes the in-line assembler feature found in MSC
     6.0.  If compiling with TurboC substitute "_asm" with "asm".

     I compile these functions with the following MicroSoft C parameters:

          cl  /c /AL /Od /Zl /Zi /FPa /Gs /W3 fttext.c

     Note that the /Od defeats optimization and is necessary only for
     compatibility with Blinker, Warplink, etc.  If you are not overlaying
     this code you may want to change this to /Oalt.  Likewise, the
     /Zi is for symbolic debugging info which you will want to omit in
     any final compiles.

     Some sample Clipper code which would use these functions is listed
     below.  It will print out the contents of this file.

              ft_fuse( "text.c" )
              do while !ft_feof()
                 ? ft_freadln()
                 ft_fskip()
              enddo
              ft_fuse()

 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2008 Viktor Szakats (harbour syenar.net)
 *    _findeol(), _findbol()
 *
 * See COPYING for licensing terms.
 *
 */

/* up this number if you need more than 10 text file areas */
#define TEXT_WORKAREAS  10
/* raise or lower this number for best performance on your system
   (larger for dealing with large files or long records, smaller for
    faster reads) */
#define BUFFSIZE        4096

#include "hbapi.h"
#include "hbapifs.h"

/* MSC compiler switch */
#if defined( _MSC_VER )
#pragma warning( disable : 4035 )
#pragma warning( disable : 4704 )
#endif

#define VALLOC_FLAG 0

/* routines internal to this module */
static HB_ISIZ _findeol( char * buf, HB_ISIZ buf_len );
static HB_ISIZ _findbol( char * buf, HB_ISIZ buf_len );
static int _ins_buff( HB_ISIZ bytes );
static int _del_buff( HB_ISIZ bytes );
static long _ft_skip( long recs );
static int _writeLine( const char * theData, HB_SIZE iDataLen );
static HB_BOOL _writeeol( HB_FHANDLE fhnd );

/* arrays used by the text workareas */
static int        area = 0;
static long       recno[ TEXT_WORKAREAS ];
static HB_FOFFSET offset[ TEXT_WORKAREAS ];
static HB_FHANDLE handles[ TEXT_WORKAREAS ];
static long       last_rec[ TEXT_WORKAREAS ];
static HB_FOFFSET last_off[ TEXT_WORKAREAS ];
static HB_FOFFSET lastbyte[ TEXT_WORKAREAS ];
static HB_BOOL    isBof[ TEXT_WORKAREAS ];
static HB_BOOL    isEof[ TEXT_WORKAREAS ];
static HB_ERRCODE error[ TEXT_WORKAREAS ];

/* for debugging purposes */
static int        doInt = 0;

HB_FUNC_EXTERN( FT_GOBOT );

HB_FUNC( FTSETINT )
{
   doInt ^= 0xFF;
}

HB_FUNC( FT_FOFFSET )
{
   hb_retnint( offset[ area ] );
}

#define FT_CHR_CR    13
#define FT_CHR_LF    10
#define FT_CHR_EOF   26

HB_FUNC( FT_FUSE )
{
   int attr = hb_parnidef( 2, FO_READWRITE | FO_DENYNONE );

   error[ area ] = 0;

   if( HB_ISCHAR( 1 ) )
   {
      handles[ area ]   = hb_fsOpen( hb_parc( 1 ), ( HB_USHORT ) attr );
      if( handles[ area ] <= 0 )
         error[ area ] = hb_fsError();
      offset[ area ]    = 0;
      recno[ area ]     = 1;
      lastbyte[ area ]  = hb_fsSeekLarge( handles[ area ], 0, FS_END );
      hb_retnint( handles[ area ] );
   }
   else
   {
      if( handles[ area ] != 0 )
      {
         hb_fsClose( handles[ area ] );
         hb_retnint( 0 );
         recno[ area ]     = 0L;
         offset[ area ]    = 0L;
         handles[ area ]   = 0;
         last_rec[ area ]  = 0L;
         last_off[ area ]  = 0L;
         lastbyte[ area ]  = 0L;
         isBof[ area ]     = HB_FALSE;
         isEof[ area ]     = HB_FALSE;
      }
   }
}

HB_FUNC( FT_FSELECT )
{
   int   oldarea = area + 1;
   int   newArea;

   if( HB_ISNUM( 1 ) )
   {
      newArea = hb_parni( 1 );
      if( newArea <= TEXT_WORKAREAS )
      {
         if( newArea == 0 )
         {
            for(; newArea < TEXT_WORKAREAS - 1; newArea++ )
            {
               if( handles[ newArea ] == 0 )
               {
                  area = newArea;
                  break;
               }
            }
         }
         else
            area = newArea - 1;
      }
   }
   hb_retni( oldarea );
}

HB_FUNC( FT_FGOTOP )
{
   error[ area ]  = 0;
   offset[ area ] = 0L;
   recno[ area ]  = 1L;
   isBof[ area ]  = HB_FALSE;
   isEof[ area ]  = HB_FALSE;
}

HB_FUNC( FT_FERROR )
{
   hb_retni( error[ area ] );
}

HB_FUNC( FT_FRECNO )
{
   hb_retnl( recno[ area ] );
}

HB_FUNC( FT_FGOBOT )
{

   error[ area ] = 0;
   if( ! last_rec[ area ] )
   {
      /* if the last record has not already been found */
      _ft_skip( 0 );
   }

   recno[ area ]  = last_rec[ area ];
   offset[ area ] = last_off[ area ];
   isBof[ area ]  = HB_FALSE;
   isEof[ area ]  = HB_FALSE;

}

HB_FUNC( FT_FSKIP )
{
   if( HB_ISNUM( 1 ) )
   {
      if( hb_parnl( 1 ) )
         hb_retnl( _ft_skip( hb_parnl( 1 ) ) );
      else
         hb_retnl( 0L );
   }
   else
      hb_retnl( _ft_skip( 1L ) );
}

/* internal routine to do buffer skips.  Passing a positive value performs
   a downward skip, a negative number does an upward skip.  Passing 0
   skips to the end of file.
   Returns a long indicating the number of records skipped */
static long _ft_skip( long iRecs )
{

   HB_ISIZ     iByteCount;
   HB_ISIZ     iBytesRead, iBytesRemaining;
   char *      cPtr;
   long        iSkipped = 0;

   char *      cBuff    = ( char * ) hb_xgrab( BUFFSIZE );
   HB_FOFFSET  fpOffset = offset[ area ];

   isBof[ area ]  = HB_FALSE;
   isEof[ area ]  = HB_FALSE;
   error[ area ]  = 0;

   /* iRecs is zero if they want to find the EOF, start a top of file */
   if( iRecs == 0 )
   {
      fpOffset       = 0L;
      recno[ area ]  = 1;
   }

   if( iRecs >= 0 )
   {
      do
      {
         cPtr = cBuff;

         /* position file pointer to beginning of current record */
         hb_fsSeekLarge( handles[ area ], fpOffset, FS_SET );

         /* read a chunk */
         iBytesRead = hb_fsRead(  handles[ area ], cBuff, BUFFSIZE );

         if( ! iBytesRead )
         {
            /* buffer is empty thus EOF, set vars and quit */
            isEof[ area ]     = HB_TRUE;
            last_rec[ area ]  = recno[ area ];
            last_off[ area ]  = offset[ area ];
            error[ area ]     = hb_fsError();
            break;

         }

         iBytesRemaining = iBytesRead;
         /* parse the buffer while there's still stuff in it */
         do
         {

            /* get count of chars in this line */
            iByteCount = _findeol( cPtr, iBytesRemaining );

            if( iByteCount > 0 && iByteCount != iBytesRemaining )
            {
               /* found a CRLF, iByteCount points to first char of next
                  record */
               iBytesRemaining   -= iByteCount;
               fpOffset          += iByteCount;
               cPtr              += iByteCount;
               offset[ area ]    = fpOffset;
               recno[ area ]++;
               iSkipped++;
               if( iRecs && ( iSkipped == iRecs ) )
                  iBytesRemaining = iBytesRead = 0;
            }
            else
            {

               /* no more CRLFs in this buffer, or CRLF is last
                  chars in the buffer */

               /* check for EOF */
               if( iBytesRead != BUFFSIZE )
               {
                  /* buffer was not full, thus EOF, set vars and quit */
                  iBytesRemaining   = 0;
                  last_rec[ area ]  = recno[ area ];
                  last_off[ area ]  = offset[ area ];
                  if( iRecs )
                     isEof[ area ] = HB_TRUE;
               }
               else
               {
                  /* buffer was full, so probably not EOF, but maybe
                     CRLF straddled end of buffer, so back up pointer a bit
                     before doing the next read */
                  fpOffset          = hb_fsSeekLarge( handles[ area ], 0, FS_RELATIVE ) - 1;
                  iBytesRemaining   = 0;
               }
            }
         }
         while( iBytesRemaining > 0 );
      }
      while( iBytesRead == BUFFSIZE );
   }
   else
   {
      /* skip backwards */
      iRecs = -iRecs;

      if( recno[ area ] > iRecs )
      {
         do
         {
            /* calc offset to read area of file ahead of current pointer */
            fpOffset = HB_MAX( offset[ area ] - BUFFSIZE, 0L );

            /* move file pointer */
            hb_fsSeekLarge( handles[ area ], fpOffset, FS_SET );

            /* read a chunk */
            iBytesRead =
               hb_fsRead(  handles[ area ], cBuff, BUFFSIZE );

            if( ! iBytesRead )
            {
               /* buffer is empty thus file is zero len, set vars and quit */
               isBof[ area ]     = HB_TRUE;
               isEof[ area ]     = HB_TRUE;
               recno[ area ]     = 0;
               offset[ area ]    = 0;
               last_rec[ area ]  = 0;
               error[ area ]     = hb_fsError();
               break;
            }

            /* set pointer within buffer */

            iBytesRemaining   = ( int ) ( offset[ area ] - fpOffset );

            cPtr              = cBuff + iBytesRemaining;

            /* parse the buffer while there's still stuff in it */
            do
            {

               /* get count of chars in this line */
               iByteCount = _findbol( cPtr, iBytesRemaining );

               if( iByteCount > 0 )
               {
                  /* found a CRLF, iByteCount points to first char of next
                     record */
                  iBytesRemaining   -= iByteCount;
                  offset[ area ]    -= iByteCount;
                  cPtr              -= iByteCount;
                  fpOffset          = offset[ area ];
                  recno[ area ]--;
                  iSkipped++;
                  if( iSkipped == iRecs )
                     iBytesRemaining = iBytesRead = 0;
               }
               else
               {

                  /* no more CRLFs in this buffer so we're either at
                     BOF or record crosses buffer boundary */
                  /* check for BOF */
                  if( iBytesRead != BUFFSIZE )
                  {
                     /* buffer was not full, thus BOF, set vars and quit */
                     iBytesRemaining   = 0;
                     offset[ area ]    = 0;
                     recno[ area ]     = 1;
                     isBof[ area ]     = HB_TRUE;
                  }
                  else
                  {
                     /* buffer was full, so not BOF */
                     iBytesRemaining = 0;
                  }
               }
            }
            while( iBytesRemaining > 0 );
         }
         while( fpOffset > 0 && iBytesRead == BUFFSIZE );
      }
      else
      {

         offset[ area ] = 0;
         recno[ area ]  = 1;
         isBof[ area ]  = HB_TRUE;
      }
   }

   hb_xfree( cBuff );
   return iSkipped;
}

HB_FUNC( FT_FREADLN )
{

   HB_ISIZ  iByteCount;
   HB_ISIZ  iBytesRead;
   char *   cPtr = ( char * ) hb_xgrab( BUFFSIZE );

   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );
   iBytesRead     = hb_fsReadLarge( handles[ area ], cPtr, BUFFSIZE );

   error[ area ]  = 0;

   if( ! iBytesRead )
      error[ area ] = hb_fsError();

   iByteCount = _findeol( cPtr, iBytesRead );

   if( iByteCount )
      hb_retclen( cPtr, iByteCount - 2 );
   else
      hb_retclen( cPtr, iBytesRead );

   hb_xfree( cPtr );
}

HB_FUNC( FT_FDELETE )
{
   int         iBytesRead;
   HB_FOFFSET  srcPtr;
   HB_FOFFSET  destPtr;
   long        cur_rec  = recno[ area ];
   HB_FOFFSET  cur_off  = offset[ area ];
   char *      Buff     = ( char * ) hb_xgrab( BUFFSIZE );

   /* save address to current record ( first record to be deleted ) */
   destPtr = offset[ area ];

   /* skip over deleted records, point to first 'to be retained' record */
   _ft_skip( hb_parnldef( 1, 1 ) );
   srcPtr = hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

   /* buffer read retained data, write atop old data */
   do
   {
      hb_fsSeekLarge( handles[ area ], srcPtr, FS_SET );
      iBytesRead  = hb_fsRead( handles[ area ], Buff, BUFFSIZE );  /* now read in a big glob */
      srcPtr      += iBytesRead;
      hb_fsSeekLarge( handles[ area ], destPtr, FS_SET );
      destPtr     += hb_fsWriteLarge( handles[ area ], Buff, iBytesRead );
   }
   while( iBytesRead > 0 );

   /* move DOS EOF marker */
   hb_fsSeekLarge( handles[ area ], srcPtr, FS_SET );
   hb_fsWrite( handles[ area ], Buff, 0 );

   error[ area ]     = hb_fsError();

   /* restore pointers */
   recno[ area ]     = cur_rec;
   offset[ area ]    = cur_off;

   /* re_calc EOF */
   lastbyte[ area ]  = hb_fsSeekLarge( handles[ area ], 0L, FS_END );
   _ft_skip( 0 );

   /* restore pointers again */
   recno[ area ]  = cur_rec;
   offset[ area ] = cur_off;

   /* if we've deleted to EOF, leave EOF flag set, otherwise clear it */
   if( recno[ area ] != last_rec[ area ] )
      isEof[ area ] = HB_FALSE;

   hb_xfree( Buff );

   hb_retl( error[ area ] ? 0 : 1 );
}

HB_FUNC( FT_FINSERT )
{
   int      no_lines = hb_parnidef( 1, 1 );
   HB_ISIZ  no_bytes = no_lines * 2;
   int      err      = 1;

   if( _ins_buff( no_bytes ) )
      err = 0;
   else
   {
      while( no_lines-- )
         if( ! _writeeol( handles[ area ] ) )
         {
            error[ area ]  = hb_fsError();
            err            = 0;
            break;
         }
   }

   hb_retl( err );
}

HB_FUNC( FT_FAPPEND )
{
   int      no_lines = hb_parnidef( 1, 1 );
   HB_ISIZ  iRead;
   HB_ISIZ  iByteCount;

   char *   buff = ( char * ) hb_xgrab( BUFFSIZE );

   error[ area ] = 0;

/* go to end of file */

   HB_FUNC_EXEC( FT_FGOBOT );

/* find end of record */

   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );
   iRead = hb_fsRead( handles[ area ], buff, BUFFSIZE );   /* now read in a big glob */

/* determine if CRLF pair exists, if not, add one */

   /* get count of chars in this line */
   iByteCount = _findeol( buff, iRead );
   if( iByteCount == 0 )
      hb_fsSeekLarge( handles[ area ], 0, FS_END );
   else
   {
      offset[ area ] = hb_fsSeekLarge( handles[ area ], offset[ area ] + iByteCount, FS_SET );
      recno[ area ]++;
      no_lines--;
   }

   while( no_lines-- )
   {
      if( ! _writeeol( handles[ area ] ) )
      {
         error[ area ] = hb_fsError();
         break;
      }
      recno[ area ]++;
      offset[ area ] = hb_fsSeekLarge( handles[ area ], 0, FS_RELATIVE );
/*    no_lines--;  !Harbour FIX! */
   }

   if( ! error[ area ] )
   {
      /* move DOS eof marker */
      hb_fsWrite( handles[ area ], buff, 0 );
      error[ area ] = hb_fsError();
   }

   /* force recalc of last record/offset */
   last_rec[ area ] = 0;

   hb_xfree( buff );

   hb_retl( error[ area ] ? 0 : 1 );

}

HB_FUNC( FT_FWRITELN )
{
   const char *   theData  = hb_parc( 1 );
   HB_ISIZ        iDataLen = hb_parclen( 1 );
   HB_BOOL        bInsert  = hb_parl( 2 );
   int            err;
   HB_ISIZ        iLineLen = 0;
   HB_ISIZ        iRead, iEOL;

   char *         buffer;

   /* position file pointer to insertion point */
   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

   if( bInsert )
   {
      /* insert mode, insert the length of new string + crlf */
      err = _ins_buff( iDataLen + 2 );

      if( ! err )
      {
         hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );
         err = _writeLine( theData, iDataLen );
      }
   }
   else
   {
      /* overwrite mode, determine how many bytes over/under */
      buffer = ( char * ) hb_xgrab( BUFFSIZE );

      /* find length of current line, loop if longer than buffer */
      do
      {
         iRead = hb_fsRead( handles[ area ], buffer, BUFFSIZE );
         iEOL  = _findeol( buffer, iRead );
         if( iEOL == 0 )
         {
            iLineLen += iRead;
         }
         else
         {
            iLineLen += iEOL;
            break;
         }
      }
      while( iRead == BUFFSIZE );

      hb_xfree( buffer );

      if( ( iDataLen + 2 ) <= iLineLen )
      {
         /* delete excess bytes from current record */
         _del_buff( iLineLen - iDataLen - 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( handles[ area ], theData, iDataLen );
      }
      else
      {
         /* insert extra bytes into current record */
         _ins_buff( iDataLen - iLineLen + 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( handles[ area ], theData, iDataLen );
      }
      error[ area ]  = hb_fsError();
      err            = ( error[ area ] ) ? 0 : 1;
   }
   hb_retl( err );
}

HB_FUNC( FT_FWRITEL )
{
   HB_FUNC_EXEC( FT_FWRITELN );
}

HB_FUNC( FT_FLASTRE )
{
   long        cur_rec;
   HB_FOFFSET  cur_offset;

   cur_rec     = recno[ area ];
   cur_offset  = offset[ area ];

   HB_FUNC_EXEC( FT_FGOBOT );
   hb_retnl( last_rec[ area ] );

   recno[ area ]  = cur_rec;
   offset[ area ] = cur_offset;
}

HB_FUNC( FT_FEOF )
{
   hb_retl( isEof[ area ] );
}

HB_FUNC( FT_FBOF )
{
   hb_retl( isBof[ area ] );
}

HB_FUNC( FT_FGOTO )
{
   long target = hb_parnl( 1 );

   /* if a recno was passed, do a relative skip */
   if( target )
   {
      /* skip relative */
      target -= recno[ area ];

      if( target )
         _ft_skip( target );
   }
   else
   {
      /* goto 0 passed, go top then skip back */
      target         = recno[ area ];

      offset[ area ] = 0L;
      recno[ area ]  = 1L;
      isBof[ area ]  = HB_FALSE;
      isEof[ area ]  = HB_FALSE;

      if( --target )
         _ft_skip( target );
   }
   error[ area ] = hb_fsError();
}

/*----------------------------------------------------------------------

   _findeol()  -  In-line assembler routine to parse a buffer
                  for a CRLF pair

                   Returns count to first character _after_ next
                   CRLF pair (beginning of next line).  Current line
                   will contain the trailing CRLF.  1Ah and trailing
                  LFs will be ignored (included in count).

                  If no CRLF found return is zero.  (could mean EOF or
                  line is longer than buffer end)

   ------------------------------------------------------------------------*/
static HB_ISIZ _findeol( char * buf, HB_ISIZ buf_len )
{
   HB_ISIZ tmp;

   for( tmp = 0; tmp < buf_len; tmp++ )
   {
      if( buf[ tmp ] == FT_CHR_CR && buf[ tmp + 1 ] == FT_CHR_LF )
         return tmp + 2;
      else if( buf[ tmp ] == FT_CHR_LF )
         return tmp + 1;
   }

   return 0;

/*
   ASM
   {
      push  di             ; save flags and registers
      push   es
      pushf
      cld                  ; move forward
      les   di, buf        ; point to buffer
      mov   bx, di         ; save buffer start for offset calc later
      mov   cx, buf_len    ; scan entire buffer
      mov   al, 13
   _feol1:repne  scasb        ; look for a CR
      jcxz  _feolerr       ; no find, return entire buffer

      cmp   es:[di], 10    ; got a CRLF pair?
      jne   _feol1         ; no, try again

      inc   di             ; yes, point to first character after CR and return
      mov   ax, di         ; subtract current pointer pos from start to
      sub   ax, bx         ;  learn offset within buffer
      jmp   _feoldone

   _feolerr:
      mov   ax, 0
   _feoldone:
      popf
       pop      es
      pop   di
   }
 */
}     /* end _findeol() */

/*----------------------------------------------------------------------

   _findbol()  -  In-line assembler routine to parse a buffer
                  for a CRLF pair

                   buf pointer points at beginning of search (end
                    of the buffer), all searches are conducted
                   backwards, returns No. of characters betw.
                   initial position and first character _after_
                   the preceding CRLF pair (beginning of line).

   ------------------------------------------------------------------------*/
static HB_ISIZ _findbol( char * buf, HB_ISIZ buf_len )
{
   HB_ISIZ tmp = buf_len - 1;

   if( tmp != 0 )
   {
      char *   p  = buf - 1;
      char     b  = *p;

      if( b == FT_CHR_EOF )
      {
         p--;
         tmp--;

         if( tmp == 0 )
            return buf_len;
      }

      if( b == FT_CHR_LF )
      {
         p--;
         tmp--;

         if( tmp == 0 )
            return buf_len;

         if( *p == FT_CHR_CR )
         {
            p--;
            tmp--;

            if( tmp == 0 )
               return buf_len;
         }
      }

      for(; tmp > 0; tmp--, p-- )
      {
         if( *p == FT_CHR_LF && *( p - 1 ) == FT_CHR_CR )
            return buf_len - ( tmp + 2 ) + 1;
         else if( *p == FT_CHR_LF )
            return buf_len - ( tmp + 1 ) + 1;
      }
   }

   return buf_len;

/*
   ASM
   {
      std                  ; move backwards
      les   di, buf        ; point to buffer tail
      mov   bx, di         ; save buffer start for offset calc later
      dec   di               ; point to preceeding character
      mov   cx, buf_len    ; scan entire buffer
      dec   cx
      jcxz   _fbolerr

      mov   al, es:[di]    ; if we're at EOF there might be a 1Ah there
      cmp   al, 1Ah         ;  if so, ignore it
      jne   _fbst

      dec   di
      dec   cx
      jnz   _fbst
      jmp   _fbolerr
   _fbst:
      cmp   al, 0Ah         ; check if pointer is sitting on a CRLF pair
      jne   _fbol1         ; if not a LF go ahead and scan entire buffer
      dec   di             ; got LF, look at next character for CR
      dec   cx
      jcxz   _fbolerr

      cmp   es:[di], 0Dh
      jne   _fbol1         ;
      dec   di             ; skip over the CR
      dec   cx
      jcxz   _fbolerr
   _fbol1:
      mov   al, 0Ah         ; look for a LF
      repne  scasb
      jcxz  _fbolerr       ; if no find return entire buffer
      cmp   es:[di], 0Dh   ; got one, check for CRLF pair?
      jne   _fbol1         ; no keep trying
      add   di, 2            ; adjust pointer to beginning of string
      return bx - di        ;  subtract current pointer pos from start to learn offset within buffer
   _fbolerr:
      return buf_len       ; on no find return length of buffer
   }
 */
}     /* end _findbol() */

/*--------------------------------------------------------------------------*/
/* inserts xxx bytes into the current file, beginning at the current record */
/* the contents of the inserted bytes are indeterminate, i.e. you'll have to
     write to them before they mean anything */
static int _ins_buff( HB_ISIZ iLen )
{

   char *      ReadBuff    = ( char * ) hb_xgrab( BUFFSIZE );
   char *      WriteBuff   = ( char * ) hb_xgrab( BUFFSIZE );
   char *      SaveBuff;
   HB_FOFFSET  fpRead, fpWrite;
   HB_ISIZ     WriteLen, ReadLen;
   HB_ISIZ     SaveLen;
   HB_ISIZ     iLenRemaining = iLen;

   /* set target move distance, this allows iLen to be greater than
      BUFFSIZE */
   iLen           = HB_MIN( iLenRemaining, BUFFSIZE );
   iLenRemaining  -= iLen;

   /* initialize file pointers */
   fpRead         = offset[ area ];
   fpWrite        = offset[ area ] + iLen;

   /* do initial load of both buffers */
   hb_fsSeekLarge( handles[ area ], fpRead, FS_SET );
   WriteLen       = hb_fsRead( handles[ area ], WriteBuff, BUFFSIZE );
   fpRead         += WriteLen;

   ReadLen        = hb_fsRead( handles[ area ], ReadBuff, BUFFSIZE );
   fpRead         += ReadLen;

   error[ area ]  = 0;

   while( ! error[ area ] && iLen > 0 )
   {
      while( WriteLen > 0 )
      {
         /* position to beginning of write area */
         if( hb_fsSeekLarge( handles[ area ], fpWrite, FS_SET ) != fpWrite )
         {
            error[ area ] = hb_fsError();
            break;
         }

         SaveLen = hb_fsWriteLarge( handles[ area ], WriteBuff, WriteLen );

         if( ! SaveLen )
         {
            error[ area ] = hb_fsError();
            break;
         }

         /* move write pointer */
         fpWrite += SaveLen;

         if( SaveLen != WriteLen )
         {
            /* error, fetch errcode and quit */
            error[ area ] = hb_fsError();
            break;
         }
         /* WriteLen = SaveLen; */

         /* swap buffers */
         SaveBuff    = WriteBuff;
         WriteBuff   = ReadBuff;
         ReadBuff    = SaveBuff;
         WriteLen    = ReadLen;

         /* return to read area and read another buffer */
         hb_fsSeekLarge( handles[ area ], fpRead, FS_SET );
         ReadLen  = hb_fsRead( handles[ area ], ReadBuff, BUFFSIZE );
         fpRead   += ReadLen;
      }

      iLen           = HB_MIN( iLenRemaining, BUFFSIZE );
      iLenRemaining  -= iLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   lastbyte[ area ] = hb_fsSeekLarge( handles[ area ], fpWrite, FS_SET );
   hb_fsWrite( handles[ area ], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[ area ] = 0L;
   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

   hb_xfree( ReadBuff  );
   hb_xfree( WriteBuff );

   return error[ area ];
}
/* _ins_buff */

/*--------------------------------------------------------------------------*/
/* deletes xxx bytes from the current file, beginning at the current record */
static int _del_buff( HB_ISIZ iLen )
{
   char *      WriteBuff = ( char * ) hb_xgrab( BUFFSIZE );
   HB_FOFFSET  fpRead, fpWrite;
   HB_ISIZ     WriteLen;
   HB_ISIZ     SaveLen;

   /* initialize file pointers */
   fpWrite  = offset[ area ];
   fpRead   = offset[ area ] + iLen;

   /* do initial load of buffer */
   hb_fsSeekLarge( handles[ area ], fpRead, FS_SET );
   WriteLen       = hb_fsRead( handles[ area ], WriteBuff, BUFFSIZE );
   fpRead         += WriteLen;

   error[ area ]  = 0;

   while( WriteLen > 0 )
   {
      /* position to beginning of write area */
      hb_fsSeekLarge( handles[ area ], fpWrite, FS_SET );
      SaveLen  = hb_fsWriteLarge( handles[ area ], WriteBuff, WriteLen );

      /* move write pointer */
      fpWrite  += SaveLen;

      if( SaveLen != WriteLen )
      {
         /* error, fetch errcode and quit */
         error[ area ] = hb_fsError();
         break;
      }

      /* return to read area and read another buffer */
      hb_fsSeekLarge( handles[ area ], fpRead, FS_SET );
      WriteLen = hb_fsRead( handles[ area ], WriteBuff, BUFFSIZE );
      fpRead   += WriteLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   lastbyte[ area ] = hb_fsSeekLarge( handles[ area ], fpWrite, FS_SET );
   hb_fsWrite( handles[ area ], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[ area ] = 0L;
   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

   hb_xfree( WriteBuff );

   return error[ area ];
}
/* _del_buff */

/*--------------------------------------------------------------------------*/
/* writes a line of data to the file, including the terminating CRLF */
static int _writeLine( const char * theData, HB_SIZE iDataLen )
{
   int err = 0;

   if( ! ( hb_fsWriteLarge( handles[ area ], theData, iDataLen ) == iDataLen ) )
   {
      err            = 1;
      error[ area ]  = hb_fsError();
   }
   else
   if( ! _writeeol( handles[ area ] ) )
   {
      err            = 1;
      error[ area ]  = hb_fsError();
   }
   return err;
}

static HB_BOOL _writeeol( HB_FHANDLE fhnd )
{
   const char *   crlf  = hb_conNewLine();
   HB_SIZE        len   = strlen( crlf );

   return hb_fsWriteLarge( fhnd, crlf, len ) == len;
}

/* eof */
