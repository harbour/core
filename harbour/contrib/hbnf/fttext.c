/*
 * $Id$
 */

/*
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
 *    FT_FSELECT( [ <nArea > ] )               -> nArea
 *    FT_FUSE(    [ <cFile>  ][, <nMode>   ] ) -> nHandle | NIL
 *    FT_FWRITELN(  <cData>   [, <lInsert> ] ) -> NIL
 *    FT_FINSERT( [ <nLines> ] )               -> NIL
 *    FT_FDELETE( [ <nLines> ] )               -> NIL
 *    FT_FAPPEND( [ <nLines> ] )               -> NIL
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

/*
     Some sample Clipper code which would use these functions is listed
     below.  It will print out the contents of this file.

              ft_fuse( "text.c" )
              DO WHILE ! ft_feof()
                 ? ft_freadln()
                 ft_fskip()
              ENDDO
              ft_fuse()

 */

/* up this number if you need more than 10 text file areas */
#define TEXT_WORKAREAS  10
/* raise or lower this number for best performance on your system
   (larger for dealing with large files or long records, smaller for
    faster reads) */
#define BUFFSIZE        4096

#include "hbapi.h"
#include "hbapifs.h"
#include "hbstack.h"

/* MSC compiler switch */
#if defined( _MSC_VER )
#pragma warning( disable : 4035 )
#pragma warning( disable : 4704 )
#endif

typedef struct
{
   int        area;
   int        doInt; /* for debugging purposes */
   /* arrays used by the text workareas */
   long       recno[ TEXT_WORKAREAS ];
   HB_FOFFSET offset[ TEXT_WORKAREAS ];
   HB_FHANDLE handles[ TEXT_WORKAREAS ];
   long       last_rec[ TEXT_WORKAREAS ];
   HB_FOFFSET last_off[ TEXT_WORKAREAS ];
   HB_FOFFSET lastbyte[ TEXT_WORKAREAS ];
   HB_BOOL    isBof[ TEXT_WORKAREAS ];
   HB_BOOL    isEof[ TEXT_WORKAREAS ];
   HB_ERRCODE error[ TEXT_WORKAREAS ];
} FT_TEXT, * PFT_TEXT;

static void s_fttext_init_init( void * cargo )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) cargo;

   ft_text->area  = 0;
   ft_text->doInt = 0;
}

static HB_TSD_NEW( s_fttext, sizeof( FT_TEXT ), s_fttext_init_init, NULL );

/* routines internal to this module */
static HB_ISIZ _findeol( char * buf, HB_ISIZ buf_len );
static HB_ISIZ _findbol( char * buf, HB_ISIZ buf_len );
static int _ins_buff( PFT_TEXT ft_text, HB_ISIZ iLen );
static int _del_buff( PFT_TEXT ft_text, HB_ISIZ iLen );
static long _ft_skip( long recs );
static int _writeLine( PFT_TEXT ft_text, const char * theData, HB_SIZE iDataLen );
static HB_BOOL _writeeol( HB_FHANDLE fhnd );

HB_FUNC( FTSETINT )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   ft_text->doInt ^= 0xFF;
}

HB_FUNC( FT_FOFFSET )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retnint( ft_text->offset[ ft_text->area ] );
}

#define FT_CHR_CR    13
#define FT_CHR_LF    10
#define FT_CHR_EOF   26

HB_FUNC( FT_FUSE )
{
   PFT_TEXT ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   int      attr     = hb_parnidef( 2, FO_READWRITE | FO_DENYNONE );

   ft_text->error[ ft_text->area ] = 0;

   if( HB_ISCHAR( 1 ) )
   {
      ft_text->handles[ ft_text->area ]   = hb_fsOpen( hb_parc( 1 ), ( HB_USHORT ) attr );
      if( ft_text->handles[ ft_text->area ] <= 0 )
         ft_text->error[ ft_text->area ] = hb_fsError();
      ft_text->offset[ ft_text->area ]    = 0;
      ft_text->recno[ ft_text->area ]     = 1;
      ft_text->lastbyte[ ft_text->area ]  = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0, FS_END );
      hb_retnint( ft_text->handles[ ft_text->area ] );
   }
   else
   {
      if( ft_text->handles[ ft_text->area ] != 0 )
      {
         hb_fsClose( ft_text->handles[ ft_text->area ] );
         hb_retnint( 0 );
         ft_text->recno[ ft_text->area ]     = 0L;
         ft_text->offset[ ft_text->area ]    = 0L;
         ft_text->handles[ ft_text->area ]   = 0;
         ft_text->last_rec[ ft_text->area ]  = 0L;
         ft_text->last_off[ ft_text->area ]  = 0L;
         ft_text->lastbyte[ ft_text->area ]  = 0L;
         ft_text->isBof[ ft_text->area ]     = HB_FALSE;
         ft_text->isEof[ ft_text->area ]     = HB_FALSE;
      }
   }
}

HB_FUNC( FT_FSELECT )
{
   PFT_TEXT ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   int      oldarea  = ft_text->area + 1;
   int      newArea;

   if( HB_ISNUM( 1 ) )
   {
      newArea = hb_parni( 1 );
      if( newArea <= TEXT_WORKAREAS )
      {
         if( newArea == 0 )
         {
            for(; newArea < TEXT_WORKAREAS - 1; newArea++ )
            {
               if( ft_text->handles[ newArea ] == 0 )
               {
                  ft_text->area = newArea;
                  break;
               }
            }
         }
         else
            ft_text->area = newArea - 1;
      }
   }
   hb_retni( oldarea );
}

HB_FUNC( FT_FGOTOP )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   ft_text->error[ ft_text->area ]  = 0;
   ft_text->offset[ ft_text->area ] = 0L;
   ft_text->recno[ ft_text->area ]  = 1L;
   ft_text->isBof[ ft_text->area ]  = HB_FALSE;
   ft_text->isEof[ ft_text->area ]  = HB_FALSE;
}

HB_FUNC( FT_FERROR )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retni( ft_text->error[ ft_text->area ] );
}

HB_FUNC( FT_FRECNO )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retnl( ft_text->recno[ ft_text->area ] );
}

HB_FUNC( FT_FGOBOT )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   ft_text->error[ ft_text->area ] = 0;
   if( ! ft_text->last_rec[ ft_text->area ] )
   {
      /* if the last record has not already been found */
      _ft_skip( 0 );
   }

   ft_text->recno[ ft_text->area ]  = ft_text->last_rec[ ft_text->area ];
   ft_text->offset[ ft_text->area ] = ft_text->last_off[ ft_text->area ];
   ft_text->isBof[ ft_text->area ]  = HB_FALSE;
   ft_text->isEof[ ft_text->area ]  = HB_FALSE;
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
   PFT_TEXT    ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   HB_ISIZ     iByteCount;
   HB_ISIZ     iBytesRead, iBytesRemaining;
   char *      cPtr;
   long        iSkipped = 0;

   char *      cBuff    = ( char * ) hb_xgrab( BUFFSIZE );
   HB_FOFFSET  fpOffset = ft_text->offset[ ft_text->area ];

   ft_text->isBof[ ft_text->area ]  = HB_FALSE;
   ft_text->isEof[ ft_text->area ]  = HB_FALSE;
   ft_text->error[ ft_text->area ]  = 0;

   /* iRecs is zero if they want to find the EOF, start a top of file */
   if( iRecs == 0 )
   {
      fpOffset                         = 0L;
      ft_text->recno[ ft_text->area ]  = 1;
   }

   if( iRecs >= 0 )
   {
      do
      {
         cPtr = cBuff;

         /* position file pointer to beginning of current record */
         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpOffset, FS_SET );

         /* read a chunk */
         iBytesRead = hb_fsRead( ft_text->handles[ ft_text->area ], cBuff, BUFFSIZE );

         if( ! iBytesRead )
         {
            /* buffer is empty thus EOF, set vars and quit */
            ft_text->isEof[ ft_text->area ]     = HB_TRUE;
            ft_text->last_rec[ ft_text->area ]  = ft_text->recno[ ft_text->area ];
            ft_text->last_off[ ft_text->area ]  = ft_text->offset[ ft_text->area ];
            ft_text->error[ ft_text->area ]     = hb_fsError();
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
               iBytesRemaining                  -= iByteCount;
               fpOffset                         += iByteCount;
               cPtr                             += iByteCount;
               ft_text->offset[ ft_text->area ] = fpOffset;
               ft_text->recno[ ft_text->area ]++;
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
                  iBytesRemaining                     = 0;
                  ft_text->last_rec[ ft_text->area ]  = ft_text->recno[ ft_text->area ];
                  ft_text->last_off[ ft_text->area ]  = ft_text->offset[ ft_text->area ];
                  if( iRecs )
                     ft_text->isEof[ ft_text->area ] = HB_TRUE;
               }
               else
               {
                  /* buffer was full, so probably not EOF, but maybe
                     CRLF straddled end of buffer, so back up pointer a bit
                     before doing the next read */
                  fpOffset          = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0, FS_RELATIVE ) - 1;
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

      if( ft_text->recno[ ft_text->area ] > iRecs )
      {
         do
         {
            /* calc offset to read area of file ahead of current pointer */
            fpOffset = HB_MAX( ft_text->offset[ ft_text->area ] - BUFFSIZE, 0L );

            /* move file pointer */
            hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpOffset, FS_SET );

            /* read a chunk */
            iBytesRead =
               hb_fsRead(  ft_text->handles[ ft_text->area ], cBuff, BUFFSIZE );

            if( ! iBytesRead )
            {
               /* buffer is empty thus file is zero len, set vars and quit */
               ft_text->isBof[ ft_text->area ]     = HB_TRUE;
               ft_text->isEof[ ft_text->area ]     = HB_TRUE;
               ft_text->recno[ ft_text->area ]     = 0;
               ft_text->offset[ ft_text->area ]    = 0;
               ft_text->last_rec[ ft_text->area ]  = 0;
               ft_text->error[ ft_text->area ]     = hb_fsError();
               break;
            }

            /* set pointer within buffer */

            iBytesRemaining   = ( int ) ( ft_text->offset[ ft_text->area ] - fpOffset );

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
                  iBytesRemaining                  -= iByteCount;
                  ft_text->offset[ ft_text->area ] -= iByteCount;
                  cPtr                             -= iByteCount;
                  fpOffset                         = ft_text->offset[ ft_text->area ];
                  ft_text->recno[ ft_text->area ]--;
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
                     iBytesRemaining                  = 0;
                     ft_text->offset[ ft_text->area ] = 0;
                     ft_text->recno[ ft_text->area ]  = 1;
                     ft_text->isBof[ ft_text->area ]  = HB_TRUE;
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

         ft_text->offset[ ft_text->area ] = 0;
         ft_text->recno[ ft_text->area ]  = 1;
         ft_text->isBof[ ft_text->area ]  = HB_TRUE;
      }
   }

   hb_xfree( cBuff );
   return iSkipped;
}

HB_FUNC( FT_FREADLN )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   HB_ISIZ  iByteCount;
   HB_ISIZ  iBytesRead;
   char *   cPtr = ( char * ) hb_xgrab( BUFFSIZE );

   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );
   iBytesRead                       = hb_fsReadLarge( ft_text->handles[ ft_text->area ], cPtr, BUFFSIZE );

   ft_text->error[ ft_text->area ]  = 0;

   if( ! iBytesRead )
      ft_text->error[ ft_text->area ] = hb_fsError();

   iByteCount = _findeol( cPtr, iBytesRead );

   if( iByteCount )
      hb_retclen( cPtr, iByteCount - 2 );
   else
      hb_retclen( cPtr, iBytesRead );

   hb_xfree( cPtr );
}

HB_FUNC( FT_FDELETE )
{
   PFT_TEXT    ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   int         iBytesRead;
   HB_FOFFSET  srcPtr;
   HB_FOFFSET  destPtr;
   long        cur_rec  = ft_text->recno[ ft_text->area ];
   HB_FOFFSET  cur_off  = ft_text->offset[ ft_text->area ];
   char *      Buff     = ( char * ) hb_xgrab( BUFFSIZE );

   /* save address to current record ( first record to be deleted ) */
   destPtr = ft_text->offset[ ft_text->area ];

   /* skip over deleted records, point to first 'to be retained' record */
   _ft_skip( hb_parnldef( 1, 1 ) );
   srcPtr = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

   /* buffer read retained data, write atop old data */
   do
   {
      hb_fsSeekLarge( ft_text->handles[ ft_text->area ], srcPtr, FS_SET );
      iBytesRead  = hb_fsRead( ft_text->handles[ ft_text->area ], Buff, BUFFSIZE );  /* now read in a big glob */
      srcPtr      += iBytesRead;
      hb_fsSeekLarge( ft_text->handles[ ft_text->area ], destPtr, FS_SET );
      destPtr     += hb_fsWriteLarge( ft_text->handles[ ft_text->area ], Buff, iBytesRead );
   }
   while( iBytesRead > 0 );

   /* move DOS EOF marker */
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], srcPtr, FS_SET );
   hb_fsWrite( ft_text->handles[ ft_text->area ], Buff, 0 );

   ft_text->error[ ft_text->area ]     = hb_fsError();

   /* restore pointers */
   ft_text->recno[ ft_text->area ]     = cur_rec;
   ft_text->offset[ ft_text->area ]    = cur_off;

   /* re_calc EOF */
   ft_text->lastbyte[ ft_text->area ]  = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0L, FS_END );
   _ft_skip( 0 );

   /* restore pointers again */
   ft_text->recno[ ft_text->area ]  = cur_rec;
   ft_text->offset[ ft_text->area ] = cur_off;

   /* if we've deleted to EOF, leave EOF flag set, otherwise clear it */
   if( ft_text->recno[ ft_text->area ] != ft_text->last_rec[ ft_text->area ] )
      ft_text->isEof[ ft_text->area ] = HB_FALSE;

   hb_xfree( Buff );

   hb_retl( ft_text->error[ ft_text->area ] ? 0 : 1 );
}

HB_FUNC( FT_FINSERT )
{
   PFT_TEXT ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   int      no_lines = hb_parnidef( 1, 1 );
   HB_ISIZ  no_bytes = no_lines * 2;
   int      err      = 1;

   if( _ins_buff( ft_text, no_bytes ) )
      err = 0;
   else
   {
      while( no_lines-- )
         if( ! _writeeol( ft_text->handles[ ft_text->area ] ) )
         {
            ft_text->error[ ft_text->area ]  = hb_fsError();
            err                              = 0;
            break;
         }
   }

   hb_retl( err );
}

HB_FUNC( FT_FAPPEND )
{
   PFT_TEXT ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   int      no_lines = hb_parnidef( 1, 1 );
   HB_ISIZ  iRead;
   HB_ISIZ  iByteCount;

   char *   buff = ( char * ) hb_xgrab( BUFFSIZE );

   ft_text->error[ ft_text->area ] = 0;

   /* go to end of file */

   HB_FUNC_EXEC( FT_FGOBOT );

   /* find end of record */

   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );
   iRead = hb_fsRead( ft_text->handles[ ft_text->area ], buff, BUFFSIZE );   /* now read in a big glob */

   /* determine if CRLF pair exists, if not, add one */

   /* get count of chars in this line */
   iByteCount = _findeol( buff, iRead );
   if( iByteCount == 0 )
      hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0, FS_END );
   else
   {
      ft_text->offset[ ft_text->area ] = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ] + iByteCount, FS_SET );
      ft_text->recno[ ft_text->area ]++;
      no_lines--;
   }

   while( no_lines-- )
   {
      if( ! _writeeol( ft_text->handles[ ft_text->area ] ) )
      {
         ft_text->error[ ft_text->area ] = hb_fsError();
         break;
      }
      ft_text->recno[ ft_text->area ]++;
      ft_text->offset[ ft_text->area ] = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0, FS_RELATIVE );
/*    no_lines--;  !Harbour FIX! */
   }

   if( ! ft_text->error[ ft_text->area ] )
   {
      /* move DOS eof marker */
      hb_fsWrite( ft_text->handles[ ft_text->area ], buff, 0 );
      ft_text->error[ ft_text->area ] = hb_fsError();
   }

   /* force recalc of last record/offset */
   ft_text->last_rec[ ft_text->area ] = 0;

   hb_xfree( buff );

   hb_retl( ft_text->error[ ft_text->area ] ? 0 : 1 );

}

HB_FUNC( FT_FWRITELN )
{
   PFT_TEXT       ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   const char *   theData  = hb_parc( 1 );
   HB_ISIZ        iDataLen = hb_parclen( 1 );
   HB_BOOL        bInsert  = hb_parl( 2 );
   int            err;
   HB_ISIZ        iLineLen = 0;
   HB_ISIZ        iRead, iEOL;

   char *         buffer;

   /* position file pointer to insertion point */
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

   if( bInsert )
   {
      /* insert mode, insert the length of new string + crlf */
      err = _ins_buff( ft_text, iDataLen + 2 );

      if( ! err )
      {
         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );
         err = _writeLine( ft_text, theData, iDataLen );
      }
   }
   else
   {
      /* overwrite mode, determine how many bytes over/under */
      buffer = ( char * ) hb_xgrab( BUFFSIZE );

      /* find length of current line, loop if longer than buffer */
      do
      {
         iRead = hb_fsRead( ft_text->handles[ ft_text->area ], buffer, BUFFSIZE );
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
         _del_buff( ft_text, iLineLen - iDataLen - 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( ft_text->handles[ ft_text->area ], theData, iDataLen );
      }
      else
      {
         /* insert extra bytes into current record */
         _ins_buff( ft_text, iDataLen - iLineLen + 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( ft_text->handles[ ft_text->area ], theData, iDataLen );
      }
      ft_text->error[ ft_text->area ]  = hb_fsError();
      err                              = ( ft_text->error[ ft_text->area ] ) ? 0 : 1;
   }
   hb_retl( err );
}

HB_FUNC( FT_FWRITEL )
{
   HB_FUNC_EXEC( FT_FWRITELN );
}

HB_FUNC( FT_FLASTRE )
{
   PFT_TEXT    ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   long        cur_rec;
   HB_FOFFSET  cur_offset;

   cur_rec     = ft_text->recno[ ft_text->area ];
   cur_offset  = ft_text->offset[ ft_text->area ];

   HB_FUNC_EXEC( FT_FGOBOT );
   hb_retnl( ft_text->last_rec[ ft_text->area ] );

   ft_text->recno[ ft_text->area ]  = cur_rec;
   ft_text->offset[ ft_text->area ] = cur_offset;
}

HB_FUNC( FT_FEOF )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retl( ft_text->isEof[ ft_text->area ] );
}

HB_FUNC( FT_FBOF )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retl( ft_text->isBof[ ft_text->area ] );
}

HB_FUNC( FT_FGOTO )
{
   PFT_TEXT ft_text  = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   long     target   = hb_parnl( 1 );

   /* if a recno was passed, do a relative skip */
   if( target )
   {
      /* skip relative */
      target -= ft_text->recno[ ft_text->area ];

      if( target )
         _ft_skip( target );
   }
   else
   {
      /* goto 0 passed, go top then skip back */
      target                           = ft_text->recno[ ft_text->area ];

      ft_text->offset[ ft_text->area ] = 0L;
      ft_text->recno[ ft_text->area ]  = 1L;
      ft_text->isBof[ ft_text->area ]  = HB_FALSE;
      ft_text->isEof[ ft_text->area ]  = HB_FALSE;

      if( --target )
         _ft_skip( target );
   }
   ft_text->error[ ft_text->area ] = hb_fsError();
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
}

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
}

/*--------------------------------------------------------------------------*/
/* inserts xxx bytes into the current file, beginning at the current record */
/* the contents of the inserted bytes are indeterminate, i.e. you'll have to
     write to them before they mean anything */
static int _ins_buff( PFT_TEXT ft_text, HB_ISIZ iLen )
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
   fpRead         = ft_text->offset[ ft_text->area ];
   fpWrite        = ft_text->offset[ ft_text->area ] + iLen;

   /* do initial load of both buffers */
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpRead, FS_SET );
   WriteLen = hb_fsRead( ft_text->handles[ ft_text->area ], WriteBuff, BUFFSIZE );
   fpRead   += WriteLen;

   ReadLen  = hb_fsRead( ft_text->handles[ ft_text->area ], ReadBuff, BUFFSIZE );
   fpRead   += ReadLen;

   ft_text->error[ ft_text->area ]  = 0;

   while( ! ft_text->error[ ft_text->area ] && iLen > 0 )
   {
      while( WriteLen > 0 )
      {
         /* position to beginning of write area */
         if( hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpWrite, FS_SET ) != fpWrite )
         {
            ft_text->error[ ft_text->area ] = hb_fsError();
            break;
         }

         SaveLen = hb_fsWriteLarge( ft_text->handles[ ft_text->area ], WriteBuff, WriteLen );

         if( ! SaveLen )
         {
            ft_text->error[ ft_text->area ] = hb_fsError();
            break;
         }

         /* move write pointer */
         fpWrite += SaveLen;

         if( SaveLen != WriteLen )
         {
            /* error, fetch errcode and quit */
            ft_text->error[ ft_text->area ] = hb_fsError();
            break;
         }
         /* WriteLen = SaveLen; */

         /* swap buffers */
         SaveBuff    = WriteBuff;
         WriteBuff   = ReadBuff;
         ReadBuff    = SaveBuff;
         WriteLen    = ReadLen;

         /* return to read area and read another buffer */
         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpRead, FS_SET );
         ReadLen  = hb_fsRead( ft_text->handles[ ft_text->area ], ReadBuff, BUFFSIZE );
         fpRead   += ReadLen;
      }

      iLen           = HB_MIN( iLenRemaining, BUFFSIZE );
      iLenRemaining  -= iLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   ft_text->lastbyte[ ft_text->area ] = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpWrite, FS_SET );
   hb_fsWrite( ft_text->handles[ ft_text->area ], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   ft_text->last_rec[ ft_text->area ] = 0L;
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

   hb_xfree( ReadBuff  );
   hb_xfree( WriteBuff );

   return ft_text->error[ ft_text->area ];
}

/*--------------------------------------------------------------------------*/
/* deletes xxx bytes from the current file, beginning at the current record */
static int _del_buff( PFT_TEXT ft_text, HB_ISIZ iLen )
{
   char *      WriteBuff = ( char * ) hb_xgrab( BUFFSIZE );
   HB_FOFFSET  fpRead, fpWrite;
   HB_ISIZ     WriteLen;
   HB_ISIZ     SaveLen;

   /* initialize file pointers */
   fpWrite  = ft_text->offset[ ft_text->area ];
   fpRead   = ft_text->offset[ ft_text->area ] + iLen;

   /* do initial load of buffer */
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpRead, FS_SET );
   WriteLen = hb_fsRead( ft_text->handles[ ft_text->area ], WriteBuff, BUFFSIZE );
   fpRead   += WriteLen;

   ft_text->error[ ft_text->area ]  = 0;

   while( WriteLen > 0 )
   {
      /* position to beginning of write area */
      hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpWrite, FS_SET );
      SaveLen  = hb_fsWriteLarge( ft_text->handles[ ft_text->area ], WriteBuff, WriteLen );

      /* move write pointer */
      fpWrite  += SaveLen;

      if( SaveLen != WriteLen )
      {
         /* error, fetch errcode and quit */
         ft_text->error[ ft_text->area ] = hb_fsError();
         break;
      }

      /* return to read area and read another buffer */
      hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpRead, FS_SET );
      WriteLen = hb_fsRead( ft_text->handles[ ft_text->area ], WriteBuff, BUFFSIZE );
      fpRead   += WriteLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   ft_text->lastbyte[ ft_text->area ] = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], fpWrite, FS_SET );
   hb_fsWrite( ft_text->handles[ ft_text->area ], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   ft_text->last_rec[ ft_text->area ] = 0L;
   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

   hb_xfree( WriteBuff );

   return ft_text->error[ ft_text->area ];
}

/*--------------------------------------------------------------------------*/
/* writes a line of data to the file, including the terminating CRLF */
static int _writeLine( PFT_TEXT ft_text, const char * theData, HB_SIZE iDataLen )
{
   int err = 0;

   if( !( hb_fsWriteLarge( ft_text->handles[ ft_text->area ], theData, iDataLen ) == iDataLen ) )
   {
      err                              = 1;
      ft_text->error[ ft_text->area ]  = hb_fsError();
   }
   else
   if( ! _writeeol( ft_text->handles[ ft_text->area ] ) )
   {
      err                              = 1;
      ft_text->error[ ft_text->area ]  = hb_fsError();
   }
   return err;
}

static HB_BOOL _writeeol( HB_FHANDLE fhnd )
{
   const char *   crlf  = hb_conNewLine();
   HB_SIZE        len   = strlen( crlf );

   return hb_fsWriteLarge( fhnd, crlf, len ) == len;
}
