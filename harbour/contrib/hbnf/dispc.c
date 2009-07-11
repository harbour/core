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

#define OFF 0
#define ON  1
#define NO  0
#define YES 1
#define OK  0
#define K_STRING   0
#define K_LIST     (!K_STRING)

#define CR   ((char) 13)
#define LF   ((char) 10)
#define FEOF ((char) 26)

#define READONLY  0             /* open file modes */
#define WRITEONLY 1
#define READWRITE 2

#define BUFFERSIZE 4096         /* maximum size of the file buffer */
#define MAXLINE    255          /* default maximum size of a line  */

#define TABSET  8

long buffoffset;            /* offset into buffer of current line  */
long fsize;                 /* file size in bytes                  */
int  bufftop, buffbot;      /* first and last character in buffer  */
int  wintop, winbot;        /* first and last character in window  */
int  winrow, wincol;        /* row and column of window highlight  */
int  sline, eline;          /* start and end line of window        */
int  scol, ecol;            /* start and end col of window         */
int  height, width;         /* height and width of window          */
int  infile;                /* input file handle                   */
int  maxlin;                /* line size                           */
int  buffsize;              /* buffer size                         */
int  hlight;                /* highlight attribute                 */
int  norm;                  /* normal attribute                    */
int  kcount;                /* number of keys in terminate key list*/
int  colinc;                /* col increment amount                */
int  brows;                 /* browse flag                         */
char refresh;               /* YES means refresh screen            */
char kstr[25];              /* terminate key string                */
int  keylist[24];           /* terminate key list                  */
int  keytype;               /* K_STRING or K_LIST                  */

int isallocated;            /* if buffers were allocated           */
char *buffer;               /* file buffer pointer                 */
char *lbuff;                /* line buffer pointer                 */
char *vseg;                 /* video segment variable              */

    /* prototypes */


static int           keyin(void);
static void          chattr(int x, int y, int len, int attr);
static long          getblock(long offset);
static void          buff_align(void);
static void          win_align(void);
static void          disp_update(int offset);
static void          windown(void);
static void          winup(void);
static void          linedown(void);
static void          lineup(void);
static void          filetop(void);
static void          filebot(void);
static void          strcpyn(char *dest, const char *source, int len);



/*
 * chattr() replace the color attribute with a new one starting at
 * location x, y and going for length len.
 *
 */

static void chattr(int x, int y, int len, int attr)
{
    int i;
    char *vmem;

    vmem = vseg + (y * (width + 1) * 2) + (x * 2) + 1;
                                            /* calc the screen memory coord */

    for (i = 0; i <= len; i++, vmem += 2)   /* write the new attribute value */
        *vmem = (char) attr;
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

static long getblock(long offset)
{
      /*
          set the file pointer to the proper offset
          and if an error occured then check to see
          if a positive offset was requested, if so
          then set the pointer to the offset from
          the end of the file, otherwise set it from
          the beginning of the file.
      */

    hb_fsSeek( infile, offset, FS_SET );

        /* read in the file and set the buffer bottom variable equal */
        /*  to the number of bytes actually read in.                 */

    buffbot = hb_fsReadLarge( infile, buffer, buffsize );

        /* if a full buffer's worth was not read in, make it full.   */

    if (( buffbot != buffsize ) && ( fsize > buffsize ))
    {
        if ( offset > 0 )
            hb_fsSeek( infile, (long) -buffsize, FS_END );
        else
            hb_fsSeek( infile, (long) buffsize, FS_SET );

        buffbot = hb_fsReadLarge( infile, buffer, buffsize );
    }

        /* return the actual file position */

    return hb_fsSeek( infile, 0L, FS_RELATIVE ) - buffbot;
}






/*
 * buff_align makes sure the buffer top and bottom variables point
 * to actual complete lines of text.
 *
 */

static void buff_align()
{
    int i;

    bufftop = 0;
    buffbot = buffsize;

    if ( buffoffset != 0L )     /* if the buffoffset is otherthan 0      */
    {
        i = bufftop;            /* start at the top of the file and scan */
                                /* forward until a CR is reached.        */

        while (( buffer[i] != CR ) && ( i < buffbot ))
            i++;

        bufftop = i + 2;
    }

        /* if the buffer offset is not a complete */
        /* buffer's length away from the file end */

    if ( buffoffset + ((long) buffbot) != fsize )
    {
          /*
             if the file position of the last byte
              of the buffer would end up past the
              end of the file, then the buffer does
              contain a complete buffer full and the
              buffer end pointer needs to be set to
              the last character of the file.
          */

        if ( buffoffset + ((long) buffbot) > fsize )
            buffbot = (int) (fsize - buffoffset);

        i = buffbot;            /* point the end of the buffer to a valid */
                                /* complete text line.                    */

        while (( buffer[i] != CR ) && ( i > bufftop ))
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

    winbot = wintop;            /* find out if there is enough text for */
    i      = 0;                 /* full window.                         */

    while (( winbot < buffbot ) && ( i < height ))
    {
        if ( buffer[winbot] == CR )
            i++;
        winbot++;
    }

    if ( i < height )           /* if there is not a full window,       */
    {
             /* then retrofit winbot to the end of a line */
         while ( buffer[winbot] != LF && winbot > bufftop)
            winbot--;

        wintop = winbot;
        i      = 0;                         /* and setup wintop */

        while (( wintop > bufftop ) && ( i <= height ))
        {
            if ( buffer[wintop] == LF )
                i++;
            wintop--;
        }

        if ( wintop != bufftop )
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

static void disp_update(int offset)
{
    int line, col, pos, i;
    char *vmem;


    refresh  = NO;
    line     = 0;

    while ( line < height )
    {
          /*
             calculate the initial position, this save execution
             time because each column is considered as a offset
             from the line start
          */

        pos = (line * (width + 1) * 2);

            /* copy string to temp buffer */

        for (i = 0; buffer[offset] != CR && offset <= winbot; offset++)
        {
           if ( i <= maxlin )
             {
               if (buffer[offset] == '\t')          /* check for a tab   */
                 {
                  lbuff[i++] = ' ';                 /* pad with spaces   */
                  while (i % TABSET && i <= maxlin) /* until tab stop    */
                    lbuff[i++] = ' ';               /* is reached or EOL */
                 }
               else lbuff[i++] = buffer[offset];

             }
        }

        for (; i <= maxlin; i++)        /* fill out with spaces */
            lbuff[i] = ' ';

            /* place the proper characters onto the screen */

        for (i = wincol, col = 0; col <= width; col++)
        {
            vmem = vseg + pos + (col * 2);

            *vmem = lbuff[i++];
        }

        line   += 1;
        offset += 2;
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
    int  k;
    long i, j;

    refresh = YES;
    k       = wintop - 3;

    while (( buffer[k] != CR ) && ( k > bufftop ))
        k--;

    if ( k >= bufftop )
    {
      if (buffer[k] == CR) k += 2;

        wintop = k;
        k      = winbot - 3;

        while ( buffer[k] != CR )
            k--;

        winbot = k + 2;
    }
    else
        if ( ((long) bufftop) + buffoffset > 0 && fsize > buffsize )
        {
            i = buffoffset + wintop;
            j = buffoffset - ((long) (buffsize / 2));

            if ( j < 0 )
                j = 0;

            buffoffset = getblock(j);
            wintop     = ((int) (i - buffoffset));

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
    int  k;
    long i, j;

    refresh = YES;
    k       = winbot;

    while (( buffer[k] != CR ) && ( k <= buffbot ))
        k++;
    k += 2;

    if ( k <= buffbot )
    {
        winbot = k;
        k      = wintop;

        while ( buffer[k] != CR )
            k++;
        wintop = k + 2;
    }
    else
        if ( (((long) buffbot) + buffoffset) < fsize && fsize > buffsize)
        {
            i = buffoffset + wintop;
            j = i;

            if ( j > fsize )
                j = fsize - ((long) buffsize);

            buffoffset = getblock(j);

            if ( i < buffoffset )
                wintop = 0;
            else
                wintop = ((int) (i - buffoffset));

            buff_align();
            win_align();
        }
}





/* move the cursor one line down */

static void linedown()
{
    if ( winrow < eline )       /* if cursor not at last line */
        winrow += 1;
    else                        /* otherwise adjust the window top variable */
        windown();
}





/* move the cursor one line up */

static void lineup()
{
    if ( winrow > sline )
        winrow -= 1;
    else
        winup();
}





/* go to the top of the file */

static void filetop()
{
    if ( buffoffset != 0 )
    {
        buffoffset = getblock(0L);

        buff_align();
    }

    refresh = YES;
    wintop  = (int) buffoffset;
    winrow  = sline;
    wincol  = 0;

    win_align();
}





/* goto the bottom of the file */

static void filebot()
{
    if ( (((long) buffbot) + buffoffset) < fsize && fsize > buffsize )
    {
        buffoffset = getblock(fsize + 1);

        buff_align();
    }

    refresh = YES;
    wintop  = buffbot - 3;
    winrow  = eline;
    wincol  = 0;

    win_align();
}


HB_FUNC( _FT_DFINIT )
{
    int rval, i, j;
    ULONG ulSize;

    rval = 0;

    sline  = hb_parni(2);                 /* top row of window   */
    scol   = hb_parni(3);                 /* left col            */
    eline  = hb_parni(4);                 /* bottom row          */
    ecol   = hb_parni(5);                 /* right col           */

    width  = ecol - scol;                 /* calc width of window  */
    height = eline - sline + 1;           /* calc height of window */

    hb_gtRectSize( sline, scol, eline, ecol, &ulSize );
    vseg = (char * ) hb_xalloc( ulSize );
    if (vseg != NULL)
       hb_gtSave( sline, scol, eline, ecol, vseg );

    maxlin   = hb_parni(12);
    buffsize = hb_parni(13);                  /* yes - load value */

    buffer = (char *) hb_xalloc(buffsize);    /* allocate memory  */
    lbuff  = (char *) hb_xalloc(maxlin + 1);  /*  for buffers     */


    isallocated = !(buffer == NULL || lbuff == NULL || vseg == NULL);
                                              /* memory allocated? */
    if (!isallocated)
    {
        rval = 8;                   /* return error code 8 (memory) */
        if (buffer != NULL) hb_xfree(buffer);
        if (lbuff != NULL)  hb_xfree(lbuff);
        if (vseg != NULL)   hb_xfree(vseg);
    }
    else                            /* get parameters               */
    {
        infile = hb_parni(1);                 /* file handle               */
        j      = hb_parni(6);                 /* starting line value       */
        norm   = hb_parni(7);                 /* normal color attribute    */
        hlight = hb_parni(8);                 /* highlight color attribute */

        if (hb_parinfo(9) & HB_IT_ARRAY)       /* if array */
        {
           keytype = K_LIST;
           kcount  = hb_parinfa( 9, 0 );
           if (kcount > 24)
              kcount = 24;
           for (i = 1; i <= kcount; i++)
              keylist[i - 1] = hb_parvni( 9, i ); /* get exit key list */
        }
        else
        {
           keytype = K_STRING;
           kcount  = hb_parclen( 9 );
           if (kcount > 24)
              kcount = 24;
           strcpyn(kstr, hb_parcx(9), kcount);    /* get exit key string */
        }

        brows = hb_parl(10);                  /* get browse flag   */

        colinc = hb_parni(11);                /* column skip value */



        bufftop    = 0;                   /* init buffer top pointer      */
        buffbot    = buffsize;            /* init buffer bottom pointer   */
        buffoffset = 0;                   /* curr line offset into buffer */
        winrow     = sline;               /* init window row              */
        wincol     = 0;                   /* init window col              */
        wintop     = 0;                   /* init window top pointer      */
        winbot     = 0;                   /* init window bottom pointer   */



            /* get file size */

        fsize = hb_fsSeek( infile, 0L, FS_END ) - 1;

            /* get the first block */

        hb_fsSeek( infile, 0L, FS_SET );

            /* if block less than buffsize */

        if ( fsize < ((long) buffbot) )
            buffbot = (int) fsize;          /* then set buffer bottom */

            /* set the current lines buffer offset pointer */

        buffoffset = getblock((long) bufftop);

            /* align buffer and window pointer to valid values */

        buff_align();
        win_align();

            /* point line pointer to line passed by caller */

        for (i = 1; i < j; i++)
            linedown();

        hb_gtRest( sline, scol, eline, ecol, vseg );

    }

    hb_retni(rval);
}

HB_FUNC ( _FT_DFCLOS )
{
  if (isallocated)
    {
      if (buffer != NULL) hb_xfree(buffer); /* free up allocated buffer memory */
      if (lbuff != NULL)  hb_xfree(lbuff);
      if (vseg != NULL)   hb_xfree(vseg);
    }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DISPFILE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Browse a text file
 *  $SYNTAX$
 *     FT_DISPFILE() -> cExitkey
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     The ASCII keystroke that terminated FT_DISPFILE()
 *  $DESCRIPTION$
 *     This routine displays a text file within a defined window using as
 *     little memory as possible.  The text file to display has to be
 *     present or an error value of 0 is returned (as a character.)
 *
 *     Assumptions: The routine assumes that all lines are terminated
 *                  with a CR/LF sequence (0x0d and 0x0a).
 *
 *     Note:        Make sure you allocate a buffer large enough to hold
 *                  enough data for the number of lines that you have
 *                  in the window.  Use the following formula as a
 *                  guideline - buffer size = (# of line) + 1 * RMargin
 *                  this is the smallest you should make the buffer and
 *                  for normal use I recommend 4096 bytes.
 *
 *     Cursor Keys: Up, Down    - moves the highlight line
 *                  Left, Right - moves the window over nColSkip col's
 *                  Home        - moves the window to the far left
 *                  End         - moves the window to the nRMargin column
 *                  PgUp, PgDn  - moves the highlight one page
 *                  Ctrl-PgUp   - moves the highlight to the file top
 *                  Ctrl-PgDn   - moves the highlight to the file bottom
 *                  Ctrl-Right  - moves the window 16 col's to the right
 *                  Ctrl-Left   - moves the window 16 col's to the left
 *
 *                  Esc, Return - terminates the function
 *
 *                  All other keys are ignored unless they are specified
 *                  within cExitKeys parameter.  This list will tell the
 *                  routine what keys terminate the function.  Special
 *                  keys must be passed by a unique value and that value
 *                  can be found by looking in the keys.h file.
 *  $EXAMPLES$
 *     @ 4,9 TO 11,71
 *
 *     FT_DFSETUP("test.txt", 5, 10, 10, 70, 1, 7, 15,;
 *                 "AaBb" + Chr(143), .T., 5, 132, 4096)
 *
 *     cKey = FT_DISPFILE()
 *
 *     FT_DFCLOSE()
 *
 *     @ 20,0 SAY "Key that terminated FT_DISPFILE() was: " + '[' + cKey + ']'
 *  $SEEALSO$
 *     FT_DFSETUP() FT_DFCLOSE()
 *  $END$
 */

HB_FUNC( FT_DISPFILE )
{
    int  i, done;
    char rval[2];

    int ch;


    /* make sure buffers were allocated and file was opened */
    if (isallocated && infile > 0)
      {
        done    = NO;
        refresh = YES;

        /* draw inside of window with normal color attribute */

        for (i = 0; i < height; i++)
            chattr(0, i, width, norm);

        hb_gtRest( sline, scol, eline, ecol, vseg );

            /* main processing loop -- terminated by user key press */

        do
        {
            if ( refresh == YES )           /* redraw window contents? */
                disp_update(wintop);

                hb_gtRest( sline, scol, eline, ecol, vseg );

                /* if not browse, highlight the current line */

            if ( brows == NO )
                chattr(0, winrow - sline, width, hlight);

            hb_gtRest( sline, scol, eline, ecol, vseg );

            hb_gtSetPos( winrow, scol );

            ch = keyin();                   /* get user key press */

                /* if not browse, then un-highlight current line */

            if ( brows == NO )
                chattr(0, winrow - sline, width, norm);

            hb_gtRest( sline, scol, eline, ecol, vseg );

                /* figure out what the user wants to do */

            switch (ch)
            {
               case K_DOWN :  if ( brows == YES )          /* if browse flag */
                                  winrow = eline;          /* is set, force  */
                                                           /* active line to */
                              linedown();                  /* be last line   */
                              break;

               case K_UP   :  if ( brows == YES )          /* if browse flag */
                                  winrow = sline;          /* is set, force  */
                                                           /* active line to */
                              lineup();                    /* be first line  */
                              break;

               case K_LEFT :  wincol -= colinc;            /* move cursor    */
                              refresh = YES;               /* to the left    */

                              if ( wincol < 0 )
                                  wincol = 0;

                              break;

               case K_RIGHT : wincol += colinc;            /* move cursor  */
                              refresh = YES;               /* to the right */

                              if ( wincol > (maxlin - width) )
                                  wincol = maxlin - width;

                              break;

               case K_HOME :  wincol  = 0;                 /* move cursor  */
                              refresh = YES;               /* to first col */

                              break;

                    /* move cursor to last col */

               case K_END  :  wincol  = maxlin - width;
                              refresh = YES;

                              break;

               case K_CTRL_LEFT  : wincol -= 16;           /* move cursor    */
                              refresh = YES;               /* 16 col to left */

                              if ( wincol < 0 )
                                  wincol = 0;

                              break;

               case K_CTRL_RIGHT  : wincol += 16;          /* move cursor     */
                              refresh = YES;               /* 16 col to right */

                              if ( wincol > (maxlin - width) )
                                  wincol = maxlin - width;

                              break;

               case K_PGUP  : for (i = 0; i < height; i++)  /* move window */
                                  winup();                  /* up one page */

                              break;

               case K_PGDN  : for (i = 0; i < height; i++)  /* move window */
                                  windown();                /* down 1 page */

                              break;

               case K_CTRL_PGUP : filetop();                /* move cursor to */
                              break;                        /* to top of file */

               case K_CTRL_PGDN : filebot();                /* move cursor to */
                              break;                        /* to bot of file */

               case K_ENTER : done = YES;                   /* carriage return */
                              break;                        /* terminates      */

               case K_ESC  : done = YES;                    /* escape key */
                              break;                        /* terminates */

                    /* scan key list and see if key pressed is there */

                default    : if (keytype == K_STRING)
                             {
                               for (i = 0; i <= kcount; i++)
                                   if ((ch > 0) && (ch < 256))
                                      if ( (int) kstr[i] == ch )
                                         done = YES;
                               break;                      /* if so terminate */
                             }
                             else
                             {
                               for (i = 0; i < kcount; i++)
                                  if ( keylist[i] == ch )
                                     done = YES;
                               break;
                             }
            }
        } while ( done == NO );
      }
    else
      ch = 0;


    /* store the key pressed as a character to be returned */

        /* return key value to caller */

    if (keytype == K_STRING)
    {
       rval[0] = (char) ch;
       rval[1] = '\0';
       hb_retc( rval );
    }
    else
       hb_retni( ch );
}



/*
 *  keyin() gets the next key typed and does any translation needed.
 *  Some keys are converted to a common name - like the up arrow is
 *  converted to the UP value which also is the Ctrl-E value.  This
 *  allows the Wordstar-like control keys to be used.  Only extended
 *  keys are translated - the values of the defines were chosen to
 *  match up with the non-extended key codes.
 *
 */

static int keyin()
{
    return hb_inkey( TRUE, 0.0, INKEY_ALL );
}


static void strcpyn( char *dest, const char *source, int len )
{
   int i;

   for (i = 0; i < len; i++)
      dest[i] = source[i];

   dest[len+1] = 0x00;
}
