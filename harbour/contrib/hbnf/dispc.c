/*
 * $Id$
 */

/*
 * File......: dispc.c
 * Author....: Mike Taylor
 * CIS ID....: ?
 *
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.10  22 Apr 2004 15:32:00   David G. Holm <dholm@jsd-llc.com>
 * Corrected all hb_fsSeek calls to use FS_ defines instead of using
 * redefined SEEK_ ones that conflict with the C-level SEEK_ defines.
 *    Rev 1.9   ? ?
 * An unknown number of changes were made between Rev 1.8 and Rev 1.10.
 *
 *    Rev 1.8   24 May 2002 19:25:00   David G. Holm <dholm@jsd-llc.com>
 * Fixed some problems that caused C++ compiles to fail.
 *
 *    Rev 1.7   29 Mar 2002 17:00:00   Walter Negro <anegro@overnet.com.ar>
 * Ported to Harbour
 *
 *    Rev 1.6   01 Jan 1995 03:01:00   TED
 * Changed some prototypes to eliminate compiler warnings.
 *
 *    Rev 1.5   14 Feb 1994 16:58:42   GLENN
 * Steve Tyrakowski and Kevin Maher modified to be CPMI-compliant.
 *
 *    Rev 1.4   18 Nov 1991 02:20:20   GLENN
 * Mike fixed a bug in _ft_dfinit() related to allocating memory.  Some
 * users had been reporting problems, but everyone who tested this patch
 * reported success.
 *
 *    Rev 1.3   17 Aug 1991 15:25:46   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:08:14   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:42   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:46   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"

#include "inkey.ch"

#define K_STRING     0
#define K_LIST       ( ! K_STRING )

#define CR           ( ( char ) 13 )
#define LF           ( ( char ) 10 )
#define FEOF         ( ( char ) 26 )

#define READONLY     0          /* open file modes */
#define WRITEONLY    1
#define READWRITE    2

#define BUFFERSIZE   4096        /* maximum size of the file buffer */
#define MAXLINE      255         /* default maximum size of a line  */

#define TABSET       8

HB_FOFFSET  buffoffset;       /* offset into buffer of current line  */
HB_FOFFSET  fsize;            /* file size in bytes                  */
HB_ISIZ     bufftop, buffbot; /* first and last character in buffer  */
int         wintop, winbot;   /* first and last character in window  */
int         winrow, wincol;   /* row and column of window highlight  */
int         sline, eline;     /* start and end line of window        */
int         scol, ecol;       /* start and end col of window         */
int         height, width;    /* height and width of window          */
HB_FHANDLE  infile;           /* input file handle                   */
int         maxlin;           /* line size                           */
HB_ISIZ     buffsize;         /* buffer size                         */
int         hlight;           /* highlight attribute                 */
int         norm;             /* normal attribute                    */
HB_ISIZ     kcount;           /* number of keys in terminate key list*/
int         colinc;           /* col increment amount                */
HB_BOOL     bBrowse;          /* browse flag                         */
HB_BOOL     bRefresh;         /* HB_TRUE means refresh screen        */
char        kstr[ 25 ];       /* terminate key string                */
int         keylist[ 24 ];    /* terminate key list                  */
int         keytype;          /* K_STRING or K_LIST                  */

HB_BOOL     bIsAllocated;     /* if buffers were allocated           */
char *      buffer;           /* file buffer pointer                 */
char *      lbuff;            /* line buffer pointer                 */
char *      vseg;             /* video segment variable              */

/* prototypes */

static void          chattr( int x, int y, int len, int attr );
static HB_FOFFSET    getblock( HB_FOFFSET offset );
static void          buff_align( void );
static void          win_align( void );
static void          disp_update( int offset );
static void          windown( void );
static void          winup( void );
static void          linedown( void );
static void          lineup( void );
static void          filetop( void );
static void          filebot( void );

/*
 * chattr() replace the color attribute with a new one starting at
 * location x, y and going for length len.
 *
 */

static void chattr( int x, int y, int len, int attr )
{
   int      i;
   char *   vmem;

   vmem = vseg + ( y * ( width + 1 ) * 2 ) + ( x * 2 ) + 1;
   /* calc the screen memory coord */

   for( i = 0; i <= len; i++, vmem += 2 )   /* write the new attribute value */
      *vmem = ( char ) attr;
}

/*
 * function getblock() reads the text file and returns the a block.
 *  the variables offset and buffsize tell it where to start reading and
 *  how many bytes to try to read.  if the block read in would not fill
 *  the buffer then the offset is adjusted so that the start or end of
 *  of the file is positioned at the head or tail of the buffer.
 *
 * it returns the offset into the file of the first byte of the buffer.
 *
 */

static HB_FOFFSET getblock( HB_FOFFSET offset )
{
   /*
       set the file pointer to the proper offset
       and if an error occured then check to see
       if a positive offset was requested, if so
       then set the pointer to the offset from
       the end of the file, otherwise set it from
       the beginning of the file.
    */

   hb_fsSeekLarge( infile, offset, FS_SET );

   /* read in the file and set the buffer bottom variable equal */
   /*  to the number of bytes actually read in.                 */

   buffbot = hb_fsReadLarge( infile, buffer, buffsize );

   /* if a full buffer's worth was not read in, make it full.   */

   if( buffbot != buffsize && fsize > buffsize )
   {
      if( offset > 0 )
         hb_fsSeekLarge( infile, -buffsize, FS_END );
      else
         hb_fsSeekLarge( infile, buffsize, FS_SET );

      buffbot = hb_fsReadLarge( infile, buffer, buffsize );
   }

   /* return the actual file position */

   return hb_fsSeekLarge( infile, 0, FS_RELATIVE ) - buffbot;
}

/*
 * buff_align makes sure the buffer top and bottom variables point
 * to actual complete lines of text.
 *
 */

static void buff_align()
{
   HB_ISIZ i;

   bufftop  = 0;
   buffbot  = buffsize;

   if( buffoffset != 0 )         /* if the buffoffset is otherthan 0      */
   {
      i = bufftop;               /* start at the top of the file and scan */
                                 /* forward until a CR is reached.        */

      while( buffer[ i ] != CR && i < buffbot )
         i++;

      bufftop = i + 2;
   }

   /* if the buffer offset is not a complete */
   /* buffer's length away from the file end */

   if( buffoffset + buffbot != fsize )
   {
      /*
         if the file position of the last byte
          of the buffer would end up past the
          end of the file, then the buffer does
          contain a complete buffer full and the
          buffer end pointer needs to be set to
          the last character of the file.
       */

      if( buffoffset + buffbot > fsize )
         buffbot = ( HB_ISIZ ) ( fsize - buffoffset );

      i = buffbot;               /* point the end of the buffer to a valid */
                                 /* complete text line.                    */

      while( buffer[ i ] != CR && i > bufftop )
         i--;

      buffbot = i + 2;
   }
}

/*
 * win_align takes the value for wintop and then figures out where
 * winbot would be.  if winbot would extend past the end of the
 * buffer, then the top of the window is adjusted to ensure that a full
 * screen of text will appear.  This simplifies the cursor routines.
 *
 */

static void win_align()
{
   int i;

   winbot   = wintop;            /* find out if there is enough text for */
   i        = 0;                 /* full window.                         */

   while( winbot < buffbot && i < height )
   {
      if( buffer[ winbot ] == CR )
         i++;
      winbot++;
   }

   if( i < height )             /* if there is not a full window,       */
   {
      /* then retrofit winbot to the end of a line */
      while( buffer[ winbot ] != LF && winbot > bufftop )
         winbot--;

      wintop   = winbot;
      i        = 0;                         /* and setup wintop */

      while( wintop > bufftop && i <= height )
      {
         if( buffer[ wintop ] == LF )
            i++;
         wintop--;
      }

      if( wintop != bufftop )
         wintop += 2;
   }
}

/*
 * this routine displays the actual text in the window.  This is done
 * by taking each line and placing it in a string.  the screen line
 * is then taken from the appropriate group of characters in the string.
 * this allows a window to page left-right across the buffer without
 * having to use any complex algorithm to calc the needed chars.
 *
 */

static void disp_update( int offset )
{
   int      line, col, pos, i;
   char *   vmem;

   bRefresh = HB_FALSE;
   line     = 0;

   while( line < height )
   {
      /*
         calculate the initial position, this save execution
         time because each column is considered as a offset
         from the line start
       */

      pos = ( line * ( width + 1 ) * 2 );

      /* copy string to temp buffer */

      for( i = 0; buffer[ offset ] != CR && offset <= winbot; offset++ )
      {
         if( i <= maxlin )
         {
            if( buffer[ offset ] == '\t' )            /* check for a tab   */
            {
               lbuff[ i++ ] = ' ';                    /* pad with spaces   */
               while( i % TABSET && i <= maxlin )     /* until tab stop    */
                  lbuff[ i++ ] = ' ';                 /* is reached or EOL */
            }
            else
               lbuff[ i++ ] = buffer[ offset ];

         }
      }

      for(; i <= maxlin; i++ )          /* fill out with spaces */
         lbuff[ i ] = ' ';

      /* place the proper characters onto the screen */

      for( i = wincol, col = 0; col <= width; col++ )
      {
         vmem  = vseg + pos + ( col * 2 );

         *vmem = lbuff[ i++ ];
      }

      line     += 1;
      offset   += 2;
   }
   hb_gtRest( sline, scol, eline, ecol, vseg );
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void winup()
{
   int         k;
   HB_FOFFSET  i, j;

   bRefresh = HB_TRUE;
   k        = wintop - 3;

   while( buffer[ k ] != CR && k > bufftop )
      k--;

   if( k >= bufftop )
   {
      if( buffer[ k ] == CR )
         k += 2;

      wintop   = k;
      k        = winbot - 3;

      while( buffer[ k ] != CR )
         k--;

      winbot = k + 2;
   }
   else
   if( bufftop + buffoffset > 0 && fsize > buffsize )
   {
      i  = buffoffset + wintop;
      j  = buffoffset - ( buffsize / 2 );

      if( j < 0 )
         j = 0;

      buffoffset  = getblock( j );
      wintop      = ( int ) ( i - buffoffset );

      buff_align();
      win_align();
   }
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void windown()
{
   int         k;
   HB_FOFFSET  i, j;

   bRefresh = HB_TRUE;
   k        = winbot;

   while( buffer[ k ] != CR && k <= buffbot )
      k++;
   k += 2;

   if( k <= buffbot )
   {
      winbot   = k;
      k        = wintop;

      while( buffer[ k ] != CR )
         k++;
      wintop = k + 2;
   }
   else
   if( ( buffbot + buffoffset ) < fsize && fsize > buffsize )
   {
      i  = buffoffset + wintop;
      j  = i;

      if( j > fsize )
         j = fsize - buffsize;

      buffoffset = getblock( j );

      if( i < buffoffset )
         wintop = 0;
      else
         wintop = ( int ) ( i - buffoffset );

      buff_align();
      win_align();
   }
}

/* move the cursor one line down */

static void linedown()
{
   if( winrow < eline )          /* if cursor not at last line */
      winrow += 1;
   else                          /* otherwise adjust the window top variable */
      windown();
}

/* move the cursor one line up */

static void lineup()
{
   if( winrow > sline )
      winrow -= 1;
   else
      winup();
}

/* go to the top of the file */

static void filetop()
{
   if( buffoffset != 0 )
   {
      buffoffset = getblock( 0 );

      buff_align();
   }

   bRefresh = HB_TRUE;
   wintop   = ( int ) buffoffset;
   winrow   = sline;
   wincol   = 0;

   win_align();
}

/* goto the bottom of the file */

static void filebot()
{
   if( ( buffbot + buffoffset ) < fsize && fsize > buffsize )
   {
      buffoffset = getblock( fsize + 1 );

      buff_align();
   }

   bRefresh = HB_TRUE;
   wintop   = ( int ) buffbot - 3;
   winrow   = eline;
   wincol   = 0;

   win_align();
}

HB_FUNC( _FT_DFINIT )
{
   int      rval;
   HB_ISIZ  j;
   HB_ISIZ  i;
   HB_SIZE  ulSize;

   rval     = 0;

   sline    = hb_parni( 2 );              /* top row of window   */
   scol     = hb_parni( 3 );              /* left col            */
   eline    = hb_parni( 4 );              /* bottom row          */
   ecol     = hb_parni( 5 );              /* right col           */

   width    = ecol - scol;                /* calc width of window  */
   height   = eline - sline + 1;          /* calc height of window */

   hb_gtRectSize( sline, scol, eline, ecol, &ulSize );
   vseg     = ( char * ) hb_xalloc( ulSize );
   if( vseg != NULL )
      hb_gtSave( sline, scol, eline, ecol, vseg );

   maxlin         = hb_parni( 12 );
   buffsize       = hb_parns( 13 );                      /* yes - load value */

   buffer         = ( char * ) hb_xalloc( buffsize );    /* allocate memory  */
   lbuff          = ( char * ) hb_xalloc( maxlin + 1 );  /*  for buffers     */

   bIsAllocated   = ! ( buffer == NULL || lbuff == NULL || vseg == NULL );
   /* memory allocated? */
   if( ! bIsAllocated )
   {
      rval = 8;                     /* return error code 8 (memory) */
      if( buffer != NULL )
         hb_xfree( buffer );
      if( lbuff != NULL )
         hb_xfree( lbuff );
      if( vseg != NULL )
         hb_xfree( vseg );
   }
   else                                               /* get parameters               */
   {
      infile   = hb_numToHandle( hb_parnint( 1 ) );   /* file handle               */
      j        = hb_parni( 6 );                       /* starting line value       */
      norm     = hb_parni( 7 );                       /* normal color attribute    */
      hlight   = hb_parni( 8 );                       /* highlight color attribute */

      if( HB_ISARRAY( 9 ) )
      {
         keytype  = K_LIST;
         kcount   = hb_parinfa( 9, 0 );
         if( kcount > 24 )
            kcount = 24;
         for( i = 1; i <= kcount; i++ )
            keylist[ i - 1 ] = hb_parvni( 9, i );  /* get exit key list */
      }
      else
      {
         keytype  = K_STRING;
         kcount   = hb_parclen( 9 );
         if( kcount > 24 )
            kcount = 24;
         hb_strncpy( kstr, hb_parcx( 9 ), kcount - 1 );    /* get exit key string */
      }

      bBrowse     = hb_parl( 10 );        /* get browse flag   */

      colinc      = hb_parni( 11 );       /* column skip value */

      bufftop     = 0;                    /* init buffer top pointer      */
      buffbot     = buffsize;             /* init buffer bottom pointer   */
      buffoffset  = 0;                    /* curr line offset into buffer */
      winrow      = sline;                /* init window row              */
      wincol      = 0;                    /* init window col              */
      wintop      = 0;                    /* init window top pointer      */
      winbot      = 0;                    /* init window bottom pointer   */

      /* get file size */

      fsize = hb_fsSeek( infile, 0, FS_END ) - 1;

      /* get the first block */

      hb_fsSeek( infile, 0, FS_SET );

      /* if block less than buffsize */

      if( fsize < buffbot )
         buffbot = ( int ) fsize;           /* then set buffer bottom */

      /* set the current lines buffer offset pointer */

      buffoffset = getblock( bufftop );

      /* align buffer and window pointer to valid values */

      buff_align();
      win_align();

      /* point line pointer to line passed by caller */

      for( i = 1; i < j; i++ )
         linedown();

      hb_gtRest( sline, scol, eline, ecol, vseg );

   }

   hb_retni( rval );
}

HB_FUNC( _FT_DFCLOS )
{
   if( bIsAllocated )
   {
      if( buffer != NULL )
         hb_xfree( buffer );                /* free up allocated buffer memory */
      if( lbuff != NULL )
         hb_xfree( lbuff );
      if( vseg != NULL )
         hb_xfree( vseg );
   }
}

HB_FUNC( FT_DISPFILE )
{
   int      i;
   char     rval[ 2 ];
   HB_BOOL  bDone;

   int      ch;

   /* make sure buffers were allocated and file was opened */
   if( bIsAllocated && infile > 0 )
   {
      bDone    = HB_FALSE;
      bRefresh = HB_TRUE;

      /* draw inside of window with normal color attribute */

      for( i = 0; i < height; i++ )
         chattr( 0, i, width, norm );

      hb_gtRest( sline, scol, eline, ecol, vseg );

      /* main processing loop -- terminated by user key press */

      do
      {
         if( bRefresh )                     /* redraw window contents? */
            disp_update( wintop );

         hb_gtRest( sline, scol, eline, ecol, vseg );

         /* if not browse, highlight the current line */

         if( ! bBrowse )
            chattr( 0, winrow - sline, width, hlight );

         hb_gtRest( sline, scol, eline, ecol, vseg );

         hb_gtSetPos( winrow, scol );

         ch = hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

         /* if not browse, then un-highlight current line */

         if( ! bBrowse )
            chattr( 0, winrow - sline, width, norm );

         hb_gtRest( sline, scol, eline, ecol, vseg );

         /* figure out what the user wants to do */

         switch( ch )
         {
            case K_DOWN:  if( bBrowse )                     /* if browse flag */
                  winrow = eline;                           /* is set, force  */
                                                            /* active line to */
               linedown();                                  /* be last line   */
               break;

            case K_UP:  if( bBrowse )                       /* if browse flag */
                  winrow = sline;                           /* is set, force  */
                                                            /* active line to */
               lineup();                                    /* be first line  */
               break;

            case K_LEFT:  wincol -= colinc;                 /* move cursor    */
               bRefresh          = HB_TRUE;                 /* to the left    */

               if( wincol < 0 )
                  wincol = 0;

               break;

            case K_RIGHT: wincol += colinc;                 /* move cursor  */
               bRefresh          = HB_TRUE;                 /* to the right */

               if( wincol > ( maxlin - width ) )
                  wincol = maxlin - width;

               break;

            case K_HOME:  wincol = 0;                       /* move cursor  */
               bRefresh          = HB_TRUE;                 /* to first col */

               break;

            /* move cursor to last col */

            case K_END:  wincol  = maxlin - width;
               bRefresh          = HB_TRUE;

               break;

            case K_CTRL_LEFT: wincol   -= 16;               /* move cursor    */
               bRefresh                = HB_TRUE;           /* 16 col to left */

               if( wincol < 0 )
                  wincol = 0;

               break;

            case K_CTRL_RIGHT: wincol  += 16;               /* move cursor     */
               bRefresh                = HB_TRUE;           /* 16 col to right */

               if( wincol > ( maxlin - width ) )
                  wincol = maxlin - width;

               break;

            case K_PGUP: for( i = 0; i < height; i++ )      /* move window */
                  winup();                                  /* up one page */

               break;

            case K_PGDN: for( i = 0; i < height; i++ )      /* move window */
                  windown();                                /* down 1 page */

               break;

            case K_CTRL_PGUP: filetop();                    /* move cursor to */
               break;                                       /* to top of file */

            case K_CTRL_PGDN: filebot();                    /* move cursor to */
               break;                                       /* to bot of file */

            case K_ENTER: bDone  = HB_TRUE;                 /* carriage return */
               break;                                       /* terminates      */

            case K_ESC: bDone    = HB_TRUE;                 /* escape key */
               break;                                       /* terminates */

            /* scan key list and see if key pressed is there */

            default: if( keytype == K_STRING )
               {
                  for( i = 0; i <= kcount; i++ )
                     if( ch > 0 && ch < 256 )
                        if( ( int ) kstr[ i ] == ch )
                           bDone = HB_TRUE;
                  break;                                   /* if so terminate */
               }
               else
               {
                  for( i = 0; i < kcount; i++ )
                     if( keylist[ i ] == ch )
                        bDone = HB_TRUE;
                  break;
               }
         }
      }
      while( ! bDone );
   }
   else
      ch = 0;

   /* store the key pressed as a character to be returned */

   /* return key value to caller */

   if( keytype == K_STRING )
   {
      rval[ 0 ] = ( char ) ch;
      rval[ 1 ] = '\0';
      hb_retc( rval );
   }
   else
      hb_retni( ch );
}
