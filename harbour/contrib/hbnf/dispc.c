/*
 * $Id$
 */

/*
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

/* TOFIX: support for reading files with any encoding
          and translating it to unicode for the GT */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbstack.h"

#define K_STRING     0
#define K_LIST       ( ! K_STRING )

#define CR           ( ( char ) 13 )
#define LF           ( ( char ) 10 )
#define FEOF         ( ( char ) 26 )

#define BUFFERSIZE   4096        /* maximum size of the file buffer */
#define MAXLINE      255         /* default maximum size of a line  */

#define TABSET       8

typedef struct
{
   HB_FOFFSET buffoffset;        /* offset into buffer of current line  */
   HB_FOFFSET fsize;             /* file size in bytes                  */
   HB_ISIZ bufftop, buffbot;     /* first and last character in buffer  */
   int wintop, winbot;           /* first and last character in window  */
   int winrow, wincol;           /* row and column of window highlight  */
   int sline, eline;             /* start and end line of window        */
   int scol, ecol;               /* start and end col of window         */
   int height, width;            /* height and width of window          */
   HB_FHANDLE infile;            /* input file handle                   */
   int maxlin;                   /* line size                           */
   HB_ISIZ buffsize;             /* buffer size                         */
   int hlight;                   /* highlight attribute                 */
   int norm;                     /* normal attribute                    */
   HB_ISIZ kcount;               /* number of keys in terminate key list*/
   int colinc;                   /* col increment amount                */
   HB_BOOL bBrowse;              /* browse flag                         */
   HB_BOOL bRefresh;             /* HB_TRUE means refresh screen        */
   int keylist[ 24 ];            /* terminate key list                  */
   int keytype;                  /* K_STRING or K_LIST                  */

   HB_BOOL bIsAllocated;         /* if buffers were allocated           */
   char * buffer;                /* file buffer pointer                 */
   char * lbuff;                 /* line buffer pointer                 */
   HB_UCHAR * vseg;              /* video segment variable              */
   int iCellSize;                /* size of one buffer cell             */
} FT_DISPC, * PFT_DISPC;

static HB_TSD_NEW( s_dispc, sizeof( FT_DISPC ), NULL, NULL );

/* prototypes */

static void          chattr( PFT_DISPC dispc, int x, int y, int len, int attr );
static HB_FOFFSET    getblock( PFT_DISPC dispc, HB_FOFFSET offset );
static void          buff_align( PFT_DISPC dispc );
static void          win_align( PFT_DISPC dispc );
static void          disp_update( PFT_DISPC dispc, int offset );
static void          windown( PFT_DISPC dispc );
static void          winup( PFT_DISPC dispc );
static void          linedown( PFT_DISPC dispc );
static void          lineup( PFT_DISPC dispc );
static void          filetop( PFT_DISPC dispc );
static void          filebot( PFT_DISPC dispc );

/*
 * chattr() replace the color attribute with a new one starting at
 * location x, y and going for length len.
 *
 */

static void chattr( PFT_DISPC dispc, int x, int y, int len, int attr )
{
   int i;

   /* calc the screen memory coord */
   HB_UCHAR * vmem;

   vmem = dispc->vseg + ( y * ( dispc->width + 1 ) * dispc->iCellSize ) + ( x * dispc->iCellSize ) + 1;

   if( dispc->iCellSize == 4 )
      vmem++;

   for( i = 0; i <= len; i++, vmem += dispc->iCellSize ) /* write the new attribute value */
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

static HB_FOFFSET getblock( PFT_DISPC dispc, HB_FOFFSET offset )
{
   /*
       set the file pointer to the proper offset
       and if an error occured then check to see
       if a positive offset was requested, if so
       then set the pointer to the offset from
       the end of the file, otherwise set it from
       the beginning of the file.
    */

   hb_fsSeekLarge( dispc->infile, offset, FS_SET );

   /* read in the file and set the buffer bottom variable equal */
   /*  to the number of bytes actually read in.                 */

   dispc->buffbot = hb_fsReadLarge( dispc->infile, dispc->buffer, dispc->buffsize );

   /* if a full buffer's worth was not read in, make it full.   */

   if( dispc->buffbot != dispc->buffsize && dispc->fsize > dispc->buffsize )
   {
      if( offset > 0 )
         hb_fsSeekLarge( dispc->infile, -dispc->buffsize, FS_END );
      else
         hb_fsSeekLarge( dispc->infile, dispc->buffsize, FS_SET );

      dispc->buffbot = hb_fsReadLarge( dispc->infile, dispc->buffer, dispc->buffsize );
   }

   /* return the actual file position */

   return hb_fsSeekLarge( dispc->infile, 0, FS_RELATIVE ) - dispc->buffbot;
}

/*
 * buff_align makes sure the buffer top and bottom variables point
 * to actual complete lines of text.
 *
 */

static void buff_align( PFT_DISPC dispc )
{
   HB_ISIZ i;

   dispc->bufftop = 0;
   dispc->buffbot = dispc->buffsize;

   if( dispc->buffoffset != 0 )           /* if the buffoffset is otherthan 0      */
   {
      i = dispc->bufftop;                 /* start at the top of the file and scan */
      /* forward until a CR is reached.        */

      while( dispc->buffer[ i ] != CR && i < dispc->buffbot )
         i++;

      dispc->bufftop = i + 2;
   }

   /* if the buffer offset is not a complete */
   /* buffer's length away from the file end */

   if( dispc->buffoffset + dispc->buffbot != dispc->fsize )
   {
      /*
         if the file position of the last byte
          of the buffer would end up past the
          end of the file, then the buffer does
          contain a complete buffer full and the
          buffer end pointer needs to be set to
          the last character of the file.
       */

      if( dispc->buffoffset + dispc->buffbot > dispc->fsize )
         dispc->buffbot = ( HB_ISIZ ) ( dispc->fsize - dispc->buffoffset );

      i = dispc->buffbot;               /* point the end of the buffer to a valid */
      /* complete text line.                    */

      while( dispc->buffer[ i ] != CR && i > dispc->bufftop )
         i--;

      dispc->buffbot = i + 2;
   }
}

/*
 * win_align takes the value for wintop and then figures out where
 * winbot would be.  if winbot would extend past the end of the
 * buffer, then the top of the window is adjusted to ensure that a full
 * screen of text will appear.  This simplifies the cursor routines.
 *
 */

static void win_align( PFT_DISPC dispc )
{
   int i;

   dispc->winbot  = dispc->wintop;  /* find out if there is enough text for */
   i              = 0;              /* full window.                         */

   while( dispc->winbot < dispc->buffbot && i < dispc->height )
   {
      if( dispc->buffer[ dispc->winbot ] == CR )
         i++;
      dispc->winbot++;
   }

   if( i < dispc->height )             /* if there is not a full window,       */
   {
      /* then retrofit winbot to the end of a line */
      while( dispc->buffer[ dispc->winbot ] != LF && dispc->winbot > dispc->bufftop )
         dispc->winbot--;

      dispc->wintop  = dispc->winbot;
      i              = 0;                   /* and setup dispc->wintop */

      while( dispc->wintop > dispc->bufftop && i <= dispc->height )
      {
         if( dispc->buffer[ dispc->wintop ] == LF )
            i++;
         dispc->wintop--;
      }

      if( dispc->wintop != dispc->bufftop )
         dispc->wintop += 2;
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

static void disp_update( PFT_DISPC dispc, int offset )
{
   int line, col, pos, i;

   dispc->bRefresh   = HB_FALSE;
   line              = 0;

   while( line < dispc->height )
   {
      /*
         calculate the initial position, this save execution
         time because each column is considered as a offset
         from the line start
       */

      pos = ( line * ( dispc->width + 1 ) * dispc->iCellSize );

      /* copy string to temp buffer */

      for( i = 0; dispc->buffer[ offset ] != CR && offset <= dispc->winbot; offset++ )
      {
         if( i <= dispc->maxlin )
         {
            if( dispc->buffer[ offset ] == '\t' )              /* check for a tab   */
            {
               dispc->lbuff[ i++ ] = ' ';                      /* pad with spaces   */
               while( i % TABSET && i <= dispc->maxlin )       /* until tab stop    */
                  dispc->lbuff[ i++ ] = ' ';                   /* is reached or EOL */
            }
            else
               dispc->lbuff[ i++ ] = dispc->buffer[ offset ];

         }
      }

      for(; i <= dispc->maxlin; i++ )          /* fill out with spaces */
         dispc->lbuff[ i ] = ' ';

      /* place the proper characters onto the screen */

      for( i = dispc->wincol, col = 0; col <= dispc->width; col++ )
      {
         HB_UCHAR * vmem = dispc->vseg + pos + ( col * dispc->iCellSize );

         *vmem = dispc->lbuff[ i++ ];
      }

      line     += 1;
      offset   += 2;
   }
   hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void winup( PFT_DISPC dispc )
{
   int         k;
   HB_FOFFSET  i, j;

   dispc->bRefresh   = HB_TRUE;
   k                 = dispc->wintop - 3;

   while( dispc->buffer[ k ] != CR && k > dispc->bufftop )
      k--;

   if( k >= dispc->bufftop )
   {
      if( dispc->buffer[ k ] == CR )
         k += 2;

      dispc->wintop  = k;
      k              = dispc->winbot - 3;

      while( dispc->buffer[ k ] != CR )
         k--;

      dispc->winbot = k + 2;
   }
   else if( dispc->bufftop + dispc->buffoffset > 0 && dispc->fsize > dispc->buffsize )
   {
      i  = dispc->buffoffset + dispc->wintop;
      j  = dispc->buffoffset - ( dispc->buffsize / 2 );

      if( j < 0 )
         j = 0;

      dispc->buffoffset = getblock( dispc, j );
      dispc->wintop     = ( int ) ( i - dispc->buffoffset );

      buff_align( dispc );
      win_align( dispc );
   }
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void windown( PFT_DISPC dispc )
{
   int         k;
   HB_FOFFSET  i, j;

   dispc->bRefresh   = HB_TRUE;
   k                 = dispc->winbot;

   while( dispc->buffer[ k ] != CR && k <= dispc->buffbot )
      k++;
   k += 2;

   if( k <= dispc->buffbot )
   {
      dispc->winbot  = k;
      k              = dispc->wintop;

      while( dispc->buffer[ k ] != CR )
         k++;
      dispc->wintop = k + 2;
   }
   else if( ( dispc->buffbot + dispc->buffoffset ) < dispc->fsize && dispc->fsize > dispc->buffsize )
   {
      i  = dispc->buffoffset + dispc->wintop;
      j  = i;

      if( j > dispc->fsize )
         j = dispc->fsize - dispc->buffsize;

      dispc->buffoffset = getblock( dispc, j );

      if( i < dispc->buffoffset )
         dispc->wintop = 0;
      else
         dispc->wintop = ( int ) ( i - dispc->buffoffset );

      buff_align( dispc );
      win_align( dispc );
   }
}

/* move the cursor one line down */

static void linedown( PFT_DISPC dispc )
{
   if( dispc->winrow < dispc->eline )     /* if cursor not at last line */
      ++dispc->winrow;
   else                                   /* otherwise adjust the window top variable */
      windown( dispc );
}

/* move the cursor one line up */

static void lineup( PFT_DISPC dispc )
{
   if( dispc->winrow > dispc->sline )
      --dispc->winrow;
   else
      winup( dispc );
}

/* go to the top of the file */

static void filetop( PFT_DISPC dispc )
{
   if( dispc->buffoffset != 0 )
   {
      dispc->buffoffset = getblock( dispc, 0 );

      buff_align( dispc );
   }

   dispc->bRefresh   = HB_TRUE;
   dispc->wintop     = ( int ) dispc->buffoffset;
   dispc->winrow     = dispc->sline;
   dispc->wincol     = 0;

   win_align( dispc );
}

/* goto the bottom of the file */

static void filebot( PFT_DISPC dispc )
{
   if( ( dispc->buffbot + dispc->buffoffset ) < dispc->fsize && dispc->fsize > dispc->buffsize )
   {
      dispc->buffoffset = getblock( dispc, dispc->fsize + 1 );

      buff_align( dispc );
   }

   dispc->bRefresh   = HB_TRUE;
   dispc->wintop     = ( int ) dispc->buffbot - 3;
   dispc->winrow     = dispc->eline;
   dispc->wincol     = 0;

   win_align( dispc );
}

HB_FUNC( _FT_DFINIT )
{
   PFT_DISPC   dispc = ( PFT_DISPC ) hb_stackGetTSD( &s_dispc );

   int         rval;
   HB_ISIZ     j;
   HB_ISIZ     i;
   HB_SIZE     nSize;

   rval           = 0;

   dispc->sline   = hb_parni( 2 );                    /* top row of window   */
   dispc->scol    = hb_parni( 3 );                    /* left col            */
   dispc->eline   = hb_parni( 4 );                    /* bottom row          */
   dispc->ecol    = hb_parni( 5 );                    /* right col           */

   dispc->width   = dispc->ecol - dispc->scol;        /* calc width of window  */
   dispc->height  = dispc->eline - dispc->sline + 1;  /* calc height of window */

   hb_gtRectSize( 0, 0, 0, 0, &nSize );

   dispc->iCellSize = ( int ) nSize;

   hb_gtRectSize( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, &nSize );
   dispc->vseg    = ( HB_UCHAR * ) hb_xalloc( nSize );
   if( dispc->vseg != NULL )
      hb_gtSave( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );

   dispc->maxlin        = hb_parni( 12 );
   dispc->buffsize      = hb_parns( 13 );                               /* yes - load value */

   dispc->buffer        = ( char * ) hb_xalloc( dispc->buffsize );      /* allocate memory  */
   dispc->lbuff         = ( char * ) hb_xalloc( dispc->maxlin + 1 );    /*  for buffers     */

   dispc->bIsAllocated  = ! ( dispc->buffer == NULL || dispc->lbuff == NULL || dispc->vseg == NULL );
   /* memory allocated? */
   if( ! dispc->bIsAllocated )
   {
      rval = 8;                     /* return error code 8 (memory) */
      if( dispc->buffer != NULL )
         hb_xfree( dispc->buffer );
      if( dispc->lbuff != NULL )
         hb_xfree( dispc->lbuff );
      if( dispc->vseg != NULL )
         hb_xfree( dispc->vseg );
   }
   else                                                     /* get parameters               */
   {
      dispc->infile  = hb_numToHandle( hb_parnint( 1 ) );   /* file handle               */
      j              = hb_parni( 6 );                       /* starting line value       */
      dispc->norm    = hb_parni( 7 );                       /* normal color attribute    */
      dispc->hlight  = hb_parni( 8 );                       /* highlight color attribute */

      if( HB_ISARRAY( 9 ) )
      {
         dispc->keytype = K_LIST;
         dispc->kcount  = hb_parinfa( 9, 0 );
         if( dispc->kcount > 24 )
            dispc->kcount = 24;
         for( i = 1; i <= dispc->kcount; i++ )
            dispc->keylist[ i - 1 ] = hb_parvni( 9, i );  /* get exit key list */
      }
      else
      {
         const char * pszKeys = hb_parcx( 9 );
         dispc->keytype = K_STRING;
         dispc->kcount  = hb_parclen( 9 );
         if( dispc->kcount > 24 )
            dispc->kcount = 24;
         for( i = 1; i <= dispc->kcount; i++ )
            dispc->keylist[ i - 1 ] = pszKeys[ i - 1 ];  /* get exit key list */
      }

      dispc->bBrowse    = hb_parl( 10 );           /* get browse flag   */

      dispc->colinc     = hb_parni( 11 );          /* column skip value */

      dispc->bufftop    = 0;                       /* init buffer top pointer      */
      dispc->buffbot    = dispc->buffsize;         /* init buffer bottom pointer   */
      dispc->buffoffset = 0;                       /* curr line offset into buffer */
      dispc->winrow     = dispc->sline;            /* init window row              */
      dispc->wincol     = 0;                       /* init window col              */
      dispc->wintop     = 0;                       /* init window top pointer      */
      dispc->winbot     = 0;                       /* init window bottom pointer   */

      /* get file size */

      dispc->fsize = hb_fsSeek( dispc->infile, 0, FS_END ) - 1;

      /* get the first block */

      hb_fsSeek( dispc->infile, 0, FS_SET );

      /* if block less than buffsize */

      if( dispc->fsize < dispc->buffbot )
         dispc->buffbot = ( int ) dispc->fsize;           /* then set buffer bottom */

      /* set the current lines buffer offset pointer */

      dispc->buffoffset = getblock( dispc, dispc->bufftop );

      /* align buffer and window pointer to valid values */

      buff_align( dispc );
      win_align( dispc );

      /* point line pointer to line passed by caller */

      for( i = 1; i < j; i++ )
         linedown( dispc );

      hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );
   }

   hb_retni( rval );
}

HB_FUNC( _FT_DFCLOS )
{
   PFT_DISPC dispc = ( PFT_DISPC ) hb_stackGetTSD( &s_dispc );

   if( dispc->bIsAllocated )
   {
      if( dispc->buffer != NULL )
         hb_xfree( dispc->buffer );                /* free up allocated buffer memory */
      if( dispc->lbuff != NULL )
         hb_xfree( dispc->lbuff );
      if( dispc->vseg != NULL )
         hb_xfree( dispc->vseg );
   }
}

HB_FUNC( FT_DISPFILE )
{
   PFT_DISPC   dispc = ( PFT_DISPC ) hb_stackGetTSD( &s_dispc );

   int         i;
   char        rval[ 2 ];
   HB_BOOL     bDone;

   int         ch;

   /* make sure buffers were allocated and file was opened */
   if( dispc->bIsAllocated && dispc->infile > 0 )
   {
      bDone             = HB_FALSE;
      dispc->bRefresh   = HB_TRUE;

      /* draw inside of window with normal color attribute */

      for( i = 0; i < dispc->height; i++ )
         chattr( dispc, 0, i, dispc->width, dispc->norm );

      hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );

      /* main processing loop -- terminated by user key press */

      do
      {
         if( dispc->bRefresh )                     /* redraw window contents? */
            disp_update( dispc, dispc->wintop );

         hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );

         /* if not browse, highlight the current line */

         if( ! dispc->bBrowse )
            chattr( dispc, 0, dispc->winrow - dispc->sline, dispc->width, dispc->hlight );

         hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );

         hb_gtSetPos( dispc->winrow, dispc->scol );

         ch = hb_inkey( HB_TRUE, 0.0, INKEY_ALL );

         /* if not browse, then un-highlight current line */

         if( ! dispc->bBrowse )
            chattr( dispc, 0, dispc->winrow - dispc->sline, dispc->width, dispc->norm );

         hb_gtRest( dispc->sline, dispc->scol, dispc->eline, dispc->ecol, dispc->vseg );

         /* figure out what the user wants to do */

         switch( ch )
         {
            case K_DOWN:

               if( dispc->bBrowse )                         /* if browse flag */
                  dispc->winrow = dispc->eline;             /* is set, force  */

               /* active line to */
               linedown( dispc );                           /* be last line   */
               break;

            case K_UP:

               if( dispc->bBrowse )                         /* if browse flag */
                  dispc->winrow = dispc->sline;             /* is set, force  */

               /* active line to */
               lineup( dispc );                             /* be first line  */
               break;

            case K_LEFT:

               dispc->wincol -= dispc->colinc;              /* move cursor    */
               dispc->bRefresh = HB_TRUE;                   /* to the left    */

               if( dispc->wincol < 0 )
                  dispc->wincol = 0;

               break;

            case K_RIGHT:

               dispc->wincol += dispc->colinc;              /* move cursor  */
               dispc->bRefresh = HB_TRUE;                   /* to the right */

               if( dispc->wincol > ( dispc->maxlin - dispc->width ) )
                  dispc->wincol = dispc->maxlin - dispc->width;

               break;

            case K_HOME:

               dispc->wincol = 0;                           /* move cursor  */
               dispc->bRefresh = HB_TRUE;                   /* to first col */

               break;

            /* move cursor to last col */

            case K_END:

               dispc->wincol = dispc->maxlin - dispc->width;
               dispc->bRefresh = HB_TRUE;

               break;

            case K_CTRL_LEFT:

               dispc->wincol -= 16;                         /* move cursor    */
               dispc->bRefresh = HB_TRUE;                   /* 16 col to left */

               if( dispc->wincol < 0 )
                  dispc->wincol = 0;

               break;

            case K_CTRL_RIGHT:

               dispc->wincol += 16;                         /* move cursor     */
               dispc->bRefresh = HB_TRUE;                   /* 16 col to right */

               if( dispc->wincol > ( dispc->maxlin - dispc->width ) )
                  dispc->wincol = dispc->maxlin - dispc->width;

               break;

            case K_PGUP:

               for( i = 0; i < dispc->height; i++ )         /* move window */
                  winup( dispc );                           /* up one page */

               break;

            case K_PGDN:

               for( i = 0; i < dispc->height; i++ )         /* move window */
                  windown( dispc );                         /* down 1 page */

               break;

            case K_CTRL_PGUP:

               filetop( dispc );                            /* move cursor to */
               break;                                       /* to top of file */

            case K_CTRL_PGDN:

               filebot( dispc );                            /* move cursor to */
               break;                                       /* to bot of file */

            case K_ENTER:

               bDone = HB_TRUE;                             /* carriage return */
               break;                                       /* terminates      */

            case K_ESC:

               bDone = HB_TRUE;                             /* escape key */
               break;                                       /* terminates */

            /* scan key list and see if key pressed is there */

            default:

               for( i = 0; i < dispc->kcount; i++ )
               {
                  if( dispc->keylist[ i ] == ch )
                     bDone = HB_TRUE;
               }
         }
      }
      while( ! bDone );
   }
   else
      ch = 0;

   /* store the key pressed as a character to be returned */

   /* return key value to caller */

   if( dispc->keytype == K_STRING )
   {
      rval[ 0 ]   = ( char ) ch;
      rval[ 1 ]   = '\0';
      hb_retc( rval );
   }
   else
      hb_retni( ch );
}
