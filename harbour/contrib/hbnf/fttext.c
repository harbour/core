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
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2008 Viktor Szakats (harbour.01 syenar.hu)
 *    _findeol(), _findbol()
 *
 * See COPYING for licensing terms.
 *
 */

/* up this number if you need more than 10 text file areas */
#define TEXT_WORKAREAS 10
/* raise or lower this number for best performance on your system
   (larger for dealing with large files or long records, smaller for
    faster reads) */
#define BUFFSIZE  4096


#include "hbapi.h"
#include "hbapifs.h"

/* MSC compiler switch */
#if defined(_MSC_VER)
#pragma warning( disable : 4035 )
#pragma warning( disable : 4704 )
#endif

#define VALLOC_FLAG   0

/* routines internal to this module */
static int _findeol( BYTE * buf, int buf_len );
static int _findbol( BYTE * buf, int buf_len );
static int _ins_buff( int bytes );
static int _del_buff( int bytes );
static long _ft_skip( long recs );
static int _writeLine( BYTE * theData, ULONG iDataLen );
static BOOL _writeeol( HB_FHANDLE fhnd );

/* arrays used by the text workareas */
static int        area = 0;
static long       recno[   TEXT_WORKAREAS];
static long       offset[  TEXT_WORKAREAS];
static HB_FHANDLE handles[ TEXT_WORKAREAS];
static long       last_rec[TEXT_WORKAREAS];
static long       last_off[TEXT_WORKAREAS];
static long       lastbyte[TEXT_WORKAREAS];
static int        isBof[   TEXT_WORKAREAS];
static int        isEof[   TEXT_WORKAREAS];
static int        error[   TEXT_WORKAREAS];

/* for debugging purposes */
static int doInt = 0;

HB_FUNC_EXTERN( FT_GOBOT );

HB_FUNC( FTSETINT )
{
   doInt ^= 0xFF;
}

HB_FUNC( FT_FOFFSET )
{
   hb_retnl( offset[area] );
}

/* standard macros */
#define __max(a,b)  (((a) > (b)) ? (a) : (b))
#define __min(a,b)  (((a) < (b)) ? (a) : (b))

#define FT_CHR_CR  13
#define FT_CHR_LF  10
#define FT_CHR_EOF 26





/*  $DOC$
 *  $FUNCNAME$
 *     FT_FUSE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Open or close a text file for use by the FT_F* functions
 *  $SYNTAX$
 *
 *     FT_FUSE( [ <cFile> ] [, <nMode> ] ) -> nHandle | 0
 *
 *  $ARGUMENTS$
 *
 *     ^b<cFile>^n is the text file you want to open.  If not specified,
 *     the file currently open, if any, will be closed.
 *
 *     ^b<nMode>^n is the open mode for the file.  Please refer to the
 *     discussion of open modes under FOPEN() in the Clipper manual
 *     and fileio.ch for a list of allowable open modes.  If not
 *     specified, the file will be opened with a mode of
 *     FO_READ + FO_SHARED (64).
 *
 *  $RETURNS$
 *
 *     If ^b<cFile>^n is passed and the file is opened successfully, an
 *     integer containing the text file's workarea.  If the file cannot be
 *     opened, -1 will be returned.  In this case, check the return value
 *     of ^bft_fError()^n for the cause of the error.
 *
 *     If FT_FUSE() is called without any arguments, it will close the
 *     text file in the current "text area" and return 0.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *  $DESCRIPTION$
 *
 *     The FT_F*() file functions are for reading text files, that is,
 *     files where each line (record) is delimited by a CRLF pair.
 *
 *     Each file is opened in its own "workarea", similar to the concept
 *     use by dbf files.  As provided, a maximum of 10 files (in 10
 *     workareas) can be opened (assuming there are sufficient file
 *     handles available).  That number may be increased by modifying
 *     the #define TEXT_WORKAREAS in the C source code and recompiling.
 *
 *  $EXAMPLES$
 *
 *     #include "fileio.ch"
 *
 *     // open a text file for reading
 *     ft_fUse( "text.txt" )
 *
 *     // open a text file for reading and writing
 *     ft_fUse( "text.txt", FO_READWRITE + FO_SHARED )
 *
 *     // close file
 *     ft_fUse()
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FSELECT()
 *  $END$
 */

HB_FUNC( FT_FUSE )
{
   int attr = ISNUM( 2 ) ? hb_parni(2) : FO_READWRITE|FO_DENYNONE;

   error[area] = 0;

   if ( ISCHAR(1) )
   {
      handles[area] = hb_fsOpen( ( BYTE * ) hb_parc(1), ( USHORT ) attr ) ;
      if( handles[area] <= 0 )
         error[area] = hb_fsError();
      offset[area] = 0 ;
      recno[area] = 1;
      lastbyte[area] = hb_fsSeek( handles[area], 0L, FS_END );
      hb_retni( handles[area] );
   }
   else
   {
      if ( handles[area] != 0 )
      {
         hb_fsClose( handles[area] );
         hb_retni(0);
         recno[area]    = 0L;
         offset[area]   = 0L;
         handles[area]  = 0;
         last_rec[area] = 0L;
         last_off[area] = 0L;
         lastbyte[area] = 0L;
         isEof[area]    = 0;
      }
   }
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FSELECT()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Select a text file workarea
 *  $SYNTAX$
 *
 *     FT_FSELECT( [ <nNewArea> ] ) -> nPreviousArea
 *
 *  $ARGUMENTS$
 *
 *     ^b<nNewArea>^n is the text file workarea to select.
 *
 *  $RETURNS$
 *
 *     The current selected text file area.
 *
 *  $DESCRIPTION$
 *
 *     This function selects a text file "workarea" from 1 to 10.  A
 *     file may or may not be open in the selected area.
 *
 *     Passing 0 for ^b<nNewArea>^n selects the next available workarea,
 *     similar to Clipper's SELECT 0 command.  If no more workareas are
 *     available the current workarea is not changed.
 *
 *     Each file is opened in its own "workarea", similar to the concept
 *     used by dbf files.  As provided, a maximum of 10 files (in 10
 *     workareas) can be opened (assuming there are sufficient file
 *     handles available).  That number may be increased by modifying
 *     the #define TEXT_WORKAREAS in the C source code and recompiling.
 *
 *     All the FT_F*() file functions operate on the file in the currently
 *     selected text file workarea.
 *
 *     Text file workareas are separate from and independent of Clipper's
 *     database workareas.
 *
 *  $EXAMPLES$
 *
 *     FT_FSELECT(1)
 *
 *     nFile1 := FT_FUSE( "temp.c" )
 *
 *     ? FT_FLASTRE()                 // no. of lines in temp.c
 *
 *     FT_FSELECT(2)
 *
 *     nFile2 := FT_FUSE( "temp.h" )
 *
 *     ? FT_FLASTRE()                 // no. of lines in temp.h
 *
 *  $SEEALSO$
 *     FT_FUSE()
 *  $END$
 */

HB_FUNC( FT_FSELECT )
{
   int   oldarea = area + 1;
   int   newArea;

   if ( ISNUM(1) )
   {
      newArea = hb_parni(1);
      if( newArea <= TEXT_WORKAREAS )
      {
         if ( newArea == 0 )
         {
            for ( ; newArea < TEXT_WORKAREAS - 1; newArea++ )
            {
               if ( handles[ newArea] == 0 )
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

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOTOP()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Go to the first record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOTOP() -> NIL
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to the first record
 *     in the currently selected text file workarea.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )      // open text file
 *
 *     DO WHILE !FT_FEOF()
 *
 *        ? FT_FREADLN()        // read thru file
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FGOTOP()              // go back to top
 *
 *     ? FT_FRECNO()            // 1
 *
 *  $SEEALSO$
 *     FT_FSELECT() FT_FUSE() FT_FRECNO() FT_FGOBOT()
 *  $END$
 */

HB_FUNC( FT_FGOTOP )
{
   error[area]  = 0;
   offset[area] = 0L;
   recno[area]  = 1L;
   isBof[area]  = FALSE;
   isEof[area]  = FALSE;
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FERROR()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Return the error code for a text file operation
 *  $SYNTAX$
 *
 *     FT_FERROR() -> nErrorNo
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     The DOS error code if one occurred.  See a reference on DOS error
 *     codes for an explanation of what the code means.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the DOS error code associated with a file
 *     operation on the currently selected text file.
 *
 *     Errors could stem from any open, create, read or write operation,
 *     among others.
 *
 *  $EXAMPLES$
 *
 *     if ft_fUse( "text.c" ) < 0     // open text file
 *        err := ft_fError();
 *        QOUT( 'Error opening file "Text.c", error code (' + ;
 *                  LTRIM( STR( err ) ) + ')' )
 *     endif
 *
 *  $SEEALSO$
 *
 *  $END$
 */


HB_FUNC( FT_FERROR )
{
   hb_retni( error[area] );
}



/*  $DOC$
 *  $FUNCNAME$
 *     FT_FRECNO()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Return the current record number of a text file
 *  $SYNTAX$
 *
 *     FT_FRECNO() -> nRecNo
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     The current record number of a text file or 0 if no file is open.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the current record number of the file open
 *     in the currently selected text file workarea.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )      // open text file
 *
 *     DO WHILE !FT_FEOF()
 *
 *        ? FT_FREADLN()        // read thru file
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FGOTOP()              // go back to top
 *
 *     ? FT_FRECNO()            // 1
 *
 *  $SEEALSO$
 *      FT_FSELECT() FT_FUSE() FT_FGOTOP() FT_FGOBOT()
 *  $END$
 */


HB_FUNC( FT_FRECNO )
{
   hb_retnl( recno[area] );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOBOT()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Go to the last record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOBOT() -> NIL
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to the last record of the
 *     file in the currently selected text file workarea.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // read last line
 *     FT_FUSE( "text.c" )
 *
 *     FT_FGOBOT()
 *
 *     ? FT_FREADLN()
 *
 *  $SEEALSO$
 *     FT_FSELECT() FT_FUSE() FT_FGOTOP() FT_FRECNO() FT_FREADLN()
 *  $END$
 */


HB_FUNC( FT_FGOBOT )
{

   error[area]  = 0;
   if( !last_rec[area] )
   {
      /* if the last record has not already been found */
      _ft_skip( 0 );
   }

   recno[ area] = last_rec[area];
   offset[area] = last_off[area];
   isBof[area]  = FALSE;
   isEof[area]  = FALSE;

}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FSKIP()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Move the record pointer to a new position in a text file
 *  $SYNTAX$
 *
 *     FT_FSKIP( [ <nLines> ] ) -> nLinesSkipped
 *
 *  $ARGUMENTS$
 *
 *     <nLines> is the number of lines to skip.  Defaults to 1 if
 *     not specified.
 *
 *  $RETURNS$
 *
 *     The number of lines actually skipped.  If the file's EOF or
 *     BOF was encountered before ^b<nLines>^n could be skipped, the
 *     return value will be less than ^b<nLines>^n.
 *
 *  $DESCRIPTION$
 *
 *     This function moves the text file record pointer, similar to
 *     the CLIPPER SKIP command.
 *
 *     Use the return value to determine how many records were actually
 *     skipped, for example to write a custom skipper function for
 *     TBrowse'g text files.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *   $EXAMPLES$
 *
 *     // display each record of a text file
 *
 *     FT_FUSE( "text.c" )
 *
 *     DO WHILE ! FT_FEOF()
 *
 *        ? FT_FREADLN()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *
 *  $SEEALSO$
 *     FT_FRECNO() FT_FGOTOP()
 *  $END$
 */

HB_FUNC( FT_FSKIP )
{
   if ( ISNUM(1) )
   {
       if( hb_parnl(1) )
          hb_retnl( _ft_skip( hb_parnl(1) ) );
       else
          hb_retnl( 0L );
   }
   else
      hb_retnl( _ft_skip(1L) );
}


/* internal routine to do buffer skips.  Passing a positive value performs
   a downward skip, a negative number does an upward skip.  Passing 0
   skips to the end of file.
   Returns a long indicating the number of records skipped */
static long _ft_skip( long iRecs )
{

   int          iByteCount;
   int          iBytesRead, iBytesRemaining;
   BYTE *       cPtr;
   long         iSkipped = 0;

   BYTE *       cBuff    = ( BYTE * ) hb_xgrab( BUFFSIZE );
   long         fpOffset = offset[area];

   isBof[area] = FALSE;
   isEof[area] = FALSE;
   error[area] = 0;

   /* iRecs is zero if they want to find the EOF, start a top of file */
   if( iRecs  == 0 )
   {
      fpOffset = 0L;
      recno[area] = 1;
   }

   if ( iRecs >= 0 )
   {
      do {
         cPtr  = cBuff;

         /* position file pointer to beginning of current record */
         hb_fsSeek( handles[area], fpOffset, FS_SET );

         /* read a chunk */
         iBytesRead = hb_fsRead(  handles[area], cBuff, BUFFSIZE );

         if( !iBytesRead )
         {
            /* buffer is empty thus EOF, set vars and quit */
            isEof[area]    = TRUE;
            last_rec[area] = recno[ area];
            last_off[area] = offset[area];
            error[area]    = hb_fsError();
            break;

         }

         iBytesRemaining = iBytesRead;
         /* parse the buffer while there's still stuff in it */
         do {

            /* get count of chars in this line */
            iByteCount = _findeol( cPtr, iBytesRemaining );

            if( ( iByteCount > 0 ) && ( iByteCount != iBytesRemaining ) )
            {
               /* found a CRLF, iByteCount points to first char of next
                  record */
               iBytesRemaining -= iByteCount;
               fpOffset        += iByteCount;
               cPtr            += iByteCount;
               offset[area]     = fpOffset;
               recno[area]++;
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
                  iBytesRemaining = 0;
                  last_rec[area]  = recno[area];
                  last_off[area]  = offset[area];
                  if( iRecs )
                     isEof[area]  = TRUE;
               }
               else
               {
                  /* buffer was full, so probably not EOF, but maybe
                     CRLF straddled end of buffer, so back up pointer a bit
                     before doing the next read */
                  fpOffset        = hb_fsSeek( handles[area], 0, FS_RELATIVE ) - 1;
                  iBytesRemaining = 0;
               }
            }
         } while ( ( iBytesRemaining > 0 ) );
      } while( ( iBytesRead == BUFFSIZE ) );
   }
   else
   {
      /* skip backwards */
      iRecs = -iRecs;

      if( recno[area] > iRecs )
      {
         do
         {
            /* calc offset to read area of file ahead of current pointer */
            fpOffset = __max( offset[area] - BUFFSIZE, 0L );

            /* move file pointer */
            hb_fsSeek( handles[area], fpOffset, FS_SET );

            /* read a chunk */
            iBytesRead =
                  hb_fsRead(  handles[area], cBuff, BUFFSIZE );

            if( !iBytesRead )
            {
               /* buffer is empty thus file is zero len, set vars and quit */
               isBof[area]        = TRUE;
               isEof[area]        = TRUE;
               recno[area]        = 0;
               offset[area]       = 0;
               last_rec[area]     = 0;
               error[area] = hb_fsError();
               break;
            }

            /* set pointer within buffer */

            iBytesRemaining = (int) ( offset[area] - fpOffset );

            cPtr = cBuff + iBytesRemaining;

            /* parse the buffer while there's still stuff in it */
            do {

               /* get count of chars in this line */
               iByteCount = _findbol( cPtr, iBytesRemaining );

               if( iByteCount > 0 )
               {
                  /* found a CRLF, iByteCount points to first char of next
                     record */
                  iBytesRemaining -= iByteCount;
                  offset[area]    -= iByteCount;
                  cPtr            -= iByteCount;
                  fpOffset         = offset[area];
                  recno[area]--;
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
                     iBytesRemaining = 0;
                     offset[area]    = 0;
                     recno[area]     = 1;
                     isBof[area]     = TRUE;
                  }
                  else
                  {
                     /* buffer was full, so not BOF */
                     iBytesRemaining  = 0;
                  }
               }
            } while ( ( iBytesRemaining > 0 ) );
         } while( ( fpOffset > 0 ) && ( iBytesRead == BUFFSIZE ) );
      }
      else
      {

         offset[area] = 0;
         recno[area]  = 1;
         isBof[area]  = TRUE;
      }
   }

   hb_xfree( ( void * ) cBuff );
   return ( iSkipped );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FREADLN()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Read a line from the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FREADLN() -> cLine
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     A string containing the current record in a text file.
 *
 *  $DESCRIPTION$
 *
 *     This function returns a line of text read from the file in the
 *     currently selected text file workarea.  Text lines are delimited
 *     with a CRLF pair.  The record pointer is not moved.
 *
 *     Currently the maximum record size is 4096 characters.  You may
 *     increase the maximum record size by changing the value of ^b#define
 *     ^bBUFFSIZE^n in the C source and recompiling, however you should
 *     consider the performance implications if you do (all read and writes
 *     use this buffer size, including ft_fSkip()'s and ft_fGoto()'s).
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // display each record of a text file
 *
 *     FT_FUSE( "text.c" )
 *
 *     DO WHILE ! FT_FEOF()
 *
 *        ? FT_FREADLN()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FWRITELN() FT_FRECNO() FT_FGOTOP()
 *  $END$
 */


HB_FUNC( FT_FREADLN )
{

   int        iByteCount;
   int        iBytesRead;
   BYTE *     cPtr = ( BYTE * ) hb_xgrab( BUFFSIZE );

   hb_fsSeek( handles[area], offset[area], FS_SET );
   iBytesRead = (int) hb_fsReadLarge( handles[area], cPtr, BUFFSIZE );

   error[area] = 0;

   if( !iBytesRead )
   {
      error[area] = hb_fsError();
   }

   iByteCount = _findeol( cPtr, iBytesRead );

   if( iByteCount )
      hb_retclen( ( char * ) cPtr, iByteCount-2 );
   else
      hb_retclen( ( char * ) cPtr, iBytesRead );

   hb_xfree( ( void * ) cPtr );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FDELETE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Deletes a line from the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FDELETE( [ < nLines > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     ^b<nLines>^n is the number of lines to be eliminated, beginning with
 *     the current record position.
 *
 *     If ^b<nLines>^n is omitted, the current record is deleted only.
 *
 *  $RETURNS$
 *
 *     TRUE if successful, otherwise check ^ft_fError()^n for error code.
 *
 *  $DESCRIPTION$
 *
 *     This function deletes one or several lines of text from the file
 *     in the currently selected text file workarea.  Text lines are
 *     delimited with a CRLF pair.  The record pointer is not moved,
 *     unless the deleted lines occur at the end of the file, in which
 *     case ^bft_fRecno()^n will equal ^bft_fLastRe()^n and ^bft_fEOF()^n
 *     will be set to TRUE.
 *
 *  $EXAMPLES$
 *
 *     // delete the next 4 lines from a file
 *     FT_FUSE( "test.txt" )
 *
 *     FT_FDELETE( 4 )
 *
 *  $SEEALSO$
 *     FT_FAPPEND() FT_FRECNO() FT_FINSERT()
 *  $END$
 */

HB_FUNC( FT_FDELETE )
{
   int    iBytesRead ;
   long   srcPtr     ;
   long   destPtr    ;
   long   cur_rec  = recno[area];
   long   cur_off  = offset[area];
   BYTE * Buff     = ( BYTE * ) hb_xgrab( BUFFSIZE );

   /* save address to current record ( first record to be deleted ) */
   destPtr = offset[area] ;

   /* skip over deleted records, point to first 'to be retained' record */
   _ft_skip( ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 ) ) ;
   srcPtr = hb_fsSeek( handles[area], offset[area], FS_SET );

   /* buffer read retained data, write atop old data */
   do
   {
      hb_fsSeek( handles[area], srcPtr, FS_SET );
      iBytesRead  = hb_fsRead( handles[area], Buff , BUFFSIZE );   /* now read in a big glob */
      srcPtr  += iBytesRead;
      hb_fsSeek( handles[area], destPtr, FS_SET );
      destPtr += hb_fsWriteLarge( handles[area], Buff, iBytesRead );
   } while( iBytesRead > 0 );


   /* move DOS EOF marker */
   hb_fsSeek( handles[area],  srcPtr, FS_SET );
   hb_fsWrite( handles[area], Buff, 0 );

   error[area] = hb_fsError();

   /* restore pointers */
   recno[area] = cur_rec;
   offset[area]= cur_off;

   /* re_calc EOF */
   lastbyte[area] = hb_fsSeek( handles[area], 0L, FS_END );
   _ft_skip( 0 );

   /* restore pointers again */
   recno[area] = cur_rec;
   offset[area]= cur_off;

   /* if we've deleted to EOF, leave EOF flag set, otherwise clear it */
   if( recno[area] != last_rec[area] )
      isEof[area]  = FALSE;

   hb_xfree( ( void * ) Buff );

   hb_retl( (error[area]) ? 0 : 1 );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FINSERT()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Inserts a line in the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FINSERT( [ < nLines > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     ^b<nLines>^n is the number of lines that should be inserted at the
 *     current record position.
 *
 *     If ^b<nLines>^n is omitted, one record is inserted.
 *
 *  $RETURNS$
 *
 *     ^blSuccess^n is TRUE if the insert succeeded, FALSE if not.  If
 *     false check the return value of ^bft_fError()^n for the reason.
 *
 *  $DESCRIPTION$
 *
 *     This function inserts a line of text in the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.
 *
 *     The record pointer is not moved.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *     Each line inserted with this function will be empty.
 *
 *  $EXAMPLES$
 *
 *     // add a couple of blank lines of text to a file
 *     ft fUse( "test.txt" )
 *
 *     ft_fGoTo( 10 )
 *
 *     ft_fInsert( 5 )
 *
 *  $SEEALSO$
 *     FT_FAPPEND() FT_FRECNO() FT_FDELETE() FT_FLASTRE()
 *  $END$
 */

HB_FUNC( FT_FINSERT )
{
   int   no_lines = ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 );
   int   no_bytes = no_lines * 2 ;
   int   err = 1;

   if( _ins_buff( no_bytes ) )
      err = 0;
   else
   {
      while( no_lines-- )
         if( !_writeeol( handles[area] ) )
         {
            error[area] = hb_fsError();
            err = 0;
            break;
         }
   }

   hb_retl( err );
}



/*  $DOC$
 *  $FUNCNAME$
 *     FT_FAPPEND()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Appends a line to the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FAPPEND( [ < nLines > ] ) -> NIL
 *
 *  $ARGUMENTS$
 *
 *     <nLines> is the number of lines that should be appended to the
 *     end of the currently selected text file.
 *
 *     If <nLines> is omitted, one record is appended.
 *
 *  $RETURNS$
 *
 *     lSuccess.  If FALSE, check ^bft_fError()^n for the error code.
 *
 *  $DESCRIPTION$
 *
 *     This function appends a line of text to the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.  The record pointer is moved to the last appended
 *     record.
 *
 *     Multiple lines may be appended with one call to FT_FAPPEND().
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *     Each line appended with this function will be empty.
 *
 *     NOTE:  Occasionally a text file may contain a non-CRLF terminated
 *     line, at the end of the file ("stragglers").  This function assumes
 *     these stragglers to be the last line of the file, and begins
 *     appending the new lines after this line.  In other words, if the
 *     last line in the text file is not terminated with a CRLF pair prior
 *     to calling FT_FAPPEND(), the function will terminate that last line
 *     before appending any new lines.
 *
 *  $EXAMPLES$
 *
 *     // add a blank line of text to a file
 *     FT_FUSE( "test.txt" )
 *
 *     ?FT_FRECNO()           // displays 5
 *
 *     FT_FAPPEND()
 *
 *     ?FT_FRECNO()           // displays 6
 *
 *  $SEEALSO$
 *     FT_FRECNO() FT_FDELETE() FT_FINSERT() FT_FLASTRE()
 *  $END$
 */

HB_FUNC( FT_FAPPEND )
{
   int   no_lines = ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 );
   int   iRead;
   int   iByteCount;

   BYTE  * buff = ( BYTE * ) hb_xgrab( BUFFSIZE );

   error[area] = 0;

/* go to end of file */

   HB_FUNC_EXEC( FT_FGOBOT );

/* find end of record */

   hb_fsSeek( handles[area], offset[area], FS_SET );
   iRead = hb_fsRead( handles[area], buff, BUFFSIZE );   /* now read in a big glob */

/* determine if CRLF pair exists, if not, add one */

   /* get count of chars in this line */
   iByteCount = _findeol( ( BYTE * ) buff, iRead );
   if( iByteCount == 0 )
      hb_fsSeek( handles[area], 0, FS_END );
   else
   {
      offset[area] = hb_fsSeek( handles[area], offset[area] + iByteCount, FS_SET );
      recno[area]++;
      no_lines--;
   }

   while( no_lines-- )
   {
      if( !_writeeol( handles[area] ) )
      {
         error[area] = hb_fsError();
         break;
      }
      recno[area]++;
      offset[area] = hb_fsSeek( handles[area], 0, FS_RELATIVE );
/*    no_lines--;  !Harbour FIX! */
   }

   if( !error[area] )
   {
      /* move DOS eof marker */
      hb_fsWrite( handles[area], buff, 0 );
      error[area] = hb_fsError();
   }

   /* force recalc of last record/offset */
   last_rec[area] = 0;

   hb_xfree( ( void * ) buff );

   hb_retl( (error[area]) ? 0 : 1 );

}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FWRITELN()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Write a line to the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FWRITELN( < cData >, [ < lInsert > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     <cData> is a string of data to write to the file at the current
 *      record position.
 *
 *     <lInsert> is a logical indicating whether the contents
 *     of the current record are to be preserved, that is, if lInsert
 *     evaluates to .T., the a new record is inserted at the current
 *     position.  The current record then is pushed down to FT_FRECNO()+1.
 *
 *     If lInsert is .F. or omitted, the current record is replaced by
 *     cData.
 *
 *  $RETURNS$
 *
 *     TRUE if successful, otherwise check ^ft_fError()^n for error code.
 *
 *  $DESCRIPTION$
 *
 *     This function writes a line of text to the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.  The record pointer is not moved.
 *
 *     The contents of the current record are updated to reflect the new
 *     new line written, unless the Insert option is selected.
 *
 *     Writing a null string has the effect of clearing the current line
 *     if in overstrike mode, else inserting a new line (same as
 *     FT_FINSERT()).
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // write a line of text to a file
 *
 *     FT_FUSE( "config.sys" )
 *
 *     DO WHILE UPPER( FT_FREADLN() ) != "FILES=" .AND. !F_FEOF()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FWRITELN( "FILES=30", FT_FEOF() )
 *
 *  $SEEALSO$
 *     FT_FREADLN() FT_FRECNO() FT_FINSERT() FT_FDELETE()
 *  $END$
 */

HB_FUNC( FT_FWRITEL )
{
   BYTE *   theData  = ( BYTE * ) hb_parc( 1 );
   int      iDataLen = hb_parclen( 1 );
   int      lInsert  = ( ISLOG( 2 ) ? hb_parl( 2 ) : 0 );
   int      err;
   int      iLineLen = 0;
   int      iRead, iEOL;

   BYTE *   buffer;


   /* position file pointer to insertion point */
   hb_fsSeek( handles[area], offset[area], FS_SET );

   if( lInsert )
   {
      /* insert mode, insert the length of new string + crlf */
      err = _ins_buff( iDataLen + 2 );

      if( !err )
      {
         hb_fsSeek( handles[area], offset[area], FS_SET );
         err = _writeLine( theData, iDataLen );
      }
   }
   else
   {
      /* overwrite mode, determine how many bytes over/under */
      buffer = ( BYTE * ) hb_xgrab( BUFFSIZE );

      /* find length of current line, loop if longer than buffer */
      do
      {
         iRead = hb_fsRead( handles[area], buffer, BUFFSIZE );
         iEOL  = _findeol( ( BYTE * ) buffer, iRead );
         if( iEOL == 0 )
         {
            iLineLen += iRead;
         }
         else
         {
            iLineLen += iEOL;
            break;
         }
      } while( iRead == BUFFSIZE );

          hb_xfree( ( void * ) buffer );

      if( (iDataLen+2) <= iLineLen )
      {
         /* delete excess bytes from current record */
         _del_buff( iLineLen - iDataLen - 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( handles[area], theData, iDataLen );
        }
      else
      {
         /* insert extra bytes into current record */
         _ins_buff( iDataLen - iLineLen + 2 );

         /* write the new record's contents */
         hb_fsWriteLarge( handles[area], theData, iDataLen );
      }
      error[area] = hb_fsError();
      err = (error[area]) ? 0 : 1;
   }
   hb_retl( err );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FLASTRE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Get the no. of records in the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FLASTRE() -> nLastRecordNum
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     An integer containing the number of records in the text file in
 *     the currently selected text file workarea, or zero if no file
 *     is currently open in the workarea.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the number of the last record in a text file.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )
 *
 *     ? FT_FLASTRE()
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FRECNO()
 *  $END$
 */

HB_FUNC( FT_FLASTRE )
{
   long cur_rec;
   long cur_offset;

   cur_rec      = recno[area];
   cur_offset   = offset[area];

   HB_FUNC_EXEC( FT_FGOBOT );
   hb_retnl( last_rec[area] );

   recno[area]  = cur_rec;
   offset[area] = cur_offset;
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FEOF()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Determine if end of text file has been encountered
 *  $SYNTAX$
 *
 *     FT_FEOF() -> lResult
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     .T. if an attempt was made to skip past the last record of
 *     the currently selected text file, otherwise .F.
 *
 *  $DESCRIPTION$
 *
 *     This function is similar to the CLIPPER Eof() function.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "fttext.c" )
 *
 *     ? FT_FEOF()        // .F.
 *
 *     FT_FSKIP()
 *
 *     ? FT_FEOF()        // .T.
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FSKIP()
 *  $END$
 */


HB_FUNC( FT_FEOF )
{
   hb_retl( isEof[area] );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FBOF()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Determine if attempt to skip past beginning of text file
 *  $SYNTAX$
 *
 *     FT_FBOF() -> lResult
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     .T. if an attempt was made to skip past the first record of
 *     the currently selected text file, otherwise .F.
 *
 *  $DESCRIPTION$
 *
 *     This function is similar to the CLIPPER Bof() function.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "fttext.c" )
 *
 *     FT_FGOTOP()
 *
 *     ? FT_FBOF()        // .F.
 *
 *     FT_FSKIP(-1)
 *
 *     ? FT_FBOF()        // .T.
 *
 *  $SEEALSO$
 *     FT_FSKIP() FT_EOF() FT_GOTOP()
 *  $END$
 */


HB_FUNC( FT_FBOF )
{
   hb_retl( isBof[area] );
}


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOTO()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Move record pointer to specific record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOTO( nLine ) -> NIL
 *
 *  $ARGUMENTS$
 *
 *     <nLine> is the record number to go to.
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to a specific record
 *     in the file in the currently selected text file workarea.  If
 *     the record number requested is greater than the number of records
 *     in the file, the record pointer will be positioned at the last
 *     record.
 *
 *     Internally, the function operates differently depending on how
 *     you invoke it.  Passing a value for ^b<nLine>^n results in what
 *     is effectively a skip operation, which is fairly quick.  However
 *     if you pass 0 for ^b<nLine>^n, e.g. ft_fGoTo( 0 ), the function
 *     internally goes to the top of the file, then skips down the
 *     required number of records.  Hence if your file is relatively
 *     large and the current record is a high number, you may see some
 *     delay as ft_fGoTo(0) skips through the file.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // read 5th line of text from file
 *
 *     ft_fUse( "fttext.c" )
 *
 *     ft_fGoTo(5)
 *
 *     cText := ft_fReadLN()
 *
 *  $SEEALSO$
 *
 *    FT_FRECNO() FT_FGOTOP() FT_FREADLN()
 *  $END$
 */

HB_FUNC( FT_FGOTO )
{
   long   target = hb_parnl(1);

   /* if a recno was passed, do a relative skip */
   if( target )
   {
      /* skip relative */
      target -= recno[area];

      if( target )
          _ft_skip( target );
   }
   else
   {
      /* goto 0 passed, go top then skip back */
      target = recno[area];

      offset[area] = 0L;
      recno[area]  = 1L;
      isBof[area]  = FALSE;
      isEof[area]  = FALSE;

      if( --target )
         _ft_skip( target );
   }
   error[area] = hb_fsError();
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
static int _findeol( BYTE * buf, int buf_len )
{
   int tmp;

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
static int _findbol( BYTE * buf, int buf_len )
{
   int tmp = buf_len - 1;

   if( tmp != 0 )
   {
      BYTE * p = buf - 1;
      BYTE b = *p;

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
      
      for( ; tmp > 0; tmp--, p-- )
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
static int _ins_buff( int iLen )
{

   BYTE *   ReadBuff    = ( BYTE * ) hb_xgrab( BUFFSIZE );
   BYTE *   WriteBuff   = ( BYTE * ) hb_xgrab( BUFFSIZE );
   BYTE *   SaveBuff;
   long     fpRead, fpWrite;
   int      WriteLen, ReadLen;
   int      SaveLen;
   int      iLenRemaining = iLen;

   /* set target move distance, this allows iLen to be greater than
      BUFFSIZE */
   iLen = __min( iLenRemaining, BUFFSIZE );
   iLenRemaining -= iLen;

   /* initialize file pointers */
   fpRead = offset[area];
   fpWrite= offset[area] + iLen;

   /* do initial load of both buffers */
   hb_fsSeek( handles[area], fpRead, FS_SET );
   WriteLen = hb_fsRead( handles[area], WriteBuff, BUFFSIZE );
   fpRead += WriteLen;

   ReadLen = hb_fsRead( handles[area], ReadBuff, BUFFSIZE );
   fpRead += ReadLen;

   error[area] = 0;

   while( !error[area] && iLen > 0 )
   {
      while( WriteLen > 0 )
      {
         /* position to beginning of write area */
         if( hb_fsSeek( handles[area], fpWrite, FS_SET ) != (unsigned long) fpWrite )
         {
            error[area] = hb_fsError();
            break;
         }

         SaveLen = hb_fsWriteLarge( handles[area], WriteBuff, WriteLen );

         if( !SaveLen )
         {
            error[area] = hb_fsError();
            break;
         }

         /* move write pointer */
         fpWrite += SaveLen;

         if(  SaveLen != WriteLen )
         {
            /* error, fetch errcode and quit */
            error[area] = hb_fsError();
            break;
         }
         /* WriteLen = SaveLen; */

         /* swap buffers */
         SaveBuff  = WriteBuff;
         WriteBuff = ReadBuff ;
         ReadBuff  = SaveBuff ;
         WriteLen  = ReadLen  ;

         /* return to read area and read another buffer */
         hb_fsSeek( handles[area], fpRead, FS_SET );
         ReadLen = hb_fsRead( handles[area], ReadBuff, BUFFSIZE );
         fpRead += ReadLen;
      }

      iLen = __min( iLenRemaining, BUFFSIZE );
      iLenRemaining -= iLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   lastbyte[area] = hb_fsSeek( handles[area], fpWrite, FS_SET );
   hb_fsWrite( handles[area], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[area] = 0L;
   hb_fsSeek( handles[area], offset[area], FS_SET );

   hb_xfree( ( void * ) ReadBuff  );
   hb_xfree( ( void * ) WriteBuff );

   return error[area];
}
/* _ins_buff */



/*--------------------------------------------------------------------------*/
/* deletes xxx bytes from the current file, beginning at the current record */
static int _del_buff( int iLen )
{
   BYTE *   WriteBuff   = ( BYTE * ) hb_xgrab( BUFFSIZE );
   long     fpRead, fpWrite;
   int      WriteLen;
   int      SaveLen;

   /* initialize file pointers */
   fpWrite = offset[area];
   fpRead  = offset[area] + iLen;

   /* do initial load of buffer */
   hb_fsSeek( handles[area], fpRead, FS_SET );
   WriteLen = hb_fsRead( handles[area], WriteBuff, BUFFSIZE );
   fpRead += WriteLen;

   error[area] = 0;

   while( WriteLen > 0 )
   {
      /* position to beginning of write area */
      hb_fsSeek( handles[area], fpWrite, FS_SET );
      SaveLen = hb_fsWriteLarge( handles[area], WriteBuff, WriteLen );

      /* move write pointer */
      fpWrite += SaveLen;

      if(  SaveLen != WriteLen )
      {
         /* error, fetch errcode and quit */
         error[area] = hb_fsError();
         break;
      }

      /* return to read area and read another buffer */
      hb_fsSeek( handles[area], fpRead, FS_SET );
      WriteLen = hb_fsRead( handles[area], WriteBuff, BUFFSIZE );
      fpRead  += WriteLen;
   }


   /* store length in bytes, set EOF marker for DOS */
   lastbyte[area] = hb_fsSeek( handles[area], fpWrite, FS_SET );
   hb_fsWrite( handles[area], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[area] = 0L;
   hb_fsSeek( handles[area], offset[area], FS_SET );

   hb_xfree( ( void * ) WriteBuff );

   return error[area];
}
/* _del_buff */


/*--------------------------------------------------------------------------*/
/* writes a line of data to the file, including the terminating CRLF */
static int _writeLine( BYTE * theData, ULONG iDataLen )
{
   int   err   = 0;

   if( !( hb_fsWriteLarge( handles[area], theData, iDataLen ) == ( ULONG ) iDataLen ) )
   {
      err = 1;
      error[area] = hb_fsError();
   }
   else
      if( !_writeeol( handles[area] ) )
      {
         err = 1;
         error[area] = hb_fsError();
      }
   return err;
}

static BOOL _writeeol( HB_FHANDLE fhnd )
{
   char * crlf = hb_conNewLine();
   ULONG len = strlen( crlf );

   return hb_fsWriteLarge( fhnd, ( BYTE * ) crlf, len ) == ( ULONG ) len;
}

/*  fttext.c  eof */
