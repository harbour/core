/*
 * $Id$
*/
#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* If mc51 is defined then the code created will used Extend and GT API
   If mc51 is not defined then the standalone editor will be created
 */
#define  mc51

#ifdef mc51
  #include <extend.api>
  #include <fm.api>
  #include <gt.api>
  #include <filesys.api>
  #include <vm.api>
#else
  #include "rvideo.h"
  #include <dos.h>

  #define CLIPPER  void pascal
  #define FALSE    0
  #define TRUE     1
#endif

#define CLIPPER_ACTION(action)  void pascal action( void )


#define Eof        '\x0'
#define SOFT        141
#define HARD       '\r'
#define END        'í'

#define LOWER      0
#define UPPER      1
#define SPOKO      2

#define UP         1
#define DOWN       0
#define YES        1
#define NO         0

#define REWRITE    1
#define NO_WRITE   0

#define  MAX_LINE_LEN   254

#ifndef mc51

  int xk,yk;
  int xmax,ymax;
  int il_kol;
  int i, insert;
  int array;

  char ramka[9] = "ÚÄ·º¼ÍÔ³";
  char *str;
  char name[30];
  int  adres;
  char *adr;

#endif



typedef struct
{
   int		top;		/* topmost row of editor's window */
   int		left;		/* leftmost column of the editor's window */
   int		bottom;		/* bottom row position */
   int		right;		/* rightmost column */
   int		line_length;	/* maximal line length */
   long int line_number;   /* the number of lines stored in text buffer */
   long int	current_line;	/* the offset in memory buffer where the current line starts (where the cursor is positioned) */
   long int	first_line;	/* offset of the first line (usually 0) */
   long int 	last_line;	/* the offset in memory buffer of the last line */
   int		cursor_row;	/* current cursor row in the window */
   int		cursor_col;	/* current cursor column in the window */
   long int	first_display;	/* the offset of first visible (displayed) line */
   long int	last_display;	/* the offset of last visible line */
   int		first_col;	/* first visible column */
   int		stable;		/* is the editor stabilized? */
   int		current_stabil; /* currently displayed row (during stabilisation) */
   int		stabil;		/* number of rows to stabilize */
   int		space;		/* should spaces at the end of line be removed */
   char		escape;		/* ASCII code of color escaspe character (the next character after this will be used as color index */
   long int	next_stabil;	/* the offset in memory buffer of next line to display */
   int		dir;		/* the direction of line stabilization */
   int		tab_size;	/* the number of spaces the replaces TAB character */
   long int	active;		/* the line number where the cursor is positioned */
   int		IsConfigured;
   long int	next_line;	/* the offset of next line to return by ED_GetNextLine() */
   long int	text_length;	/* the size (in bytes) of edited text */
   long int	bufor_size;	/* the size of allocated memory buffer */
   char		*begin;		/* the memory buffer */

} EDITOR ;




static EDITOR *EStack[] ={ NULL, NULL, NULL, NULL, NULL };
/* table of created editors */
static EDITOR *ETab[] ={ NULL, NULL, NULL, NULL, NULL,
                         NULL, NULL, NULL, NULL, NULL, NULL };
static EDITOR *ED;	/* currently serviced editor */





#ifdef mc51

  void        _ncopyuc( char*, char*, int );
  void        _ncopylc( char*, char*, int );
  static unsigned int Clear( EDITOR*, long int, unsigned int * );
  static void BackSpace( int );
  static void NextWord( void );
  static void Return( int );
  static void GoTo( int );
  static int  format_line( EDITOR*, int, unsigned int );
  static void MoveText ( EDITOR*, long int, long int, long int );
  static unsigned int GetLineLength( EDITOR*, long int, int* );

#else
#pragma aux MS_C "_*" \
        parm caller [] \
        value struct float struct routine [ax]\
        modify [ax bx cx dx es];

  static int  format_line( EDITOR*, int, unsigned int );
  static void GoTo( int );
  static unsigned int Clear( EDITOR*, long int, unsigned int * );
  static void BackSpace( int );
  static void NextWord( void );
  static void Return( int );
  static void MoveText ( EDITOR*, long int, long int, long int );
  static unsigned int GetLineLength( EDITOR*, long int, int* );

  #pragma aux (MS_C) DISPXYA;
  #pragma aux (MS_C) REPXYA;
  #pragma aux (MS_C) READKEY;
  #pragma aux (MS_C) SETXY;
  #pragma aux (MS_C) VIDEMODE;
  #pragma aux (MS_C) DRAWBOX;

  static int nColor=15;

void _gtWriteAt( int nTop, int nLeft, char *cText, int nLen )
{
  char cBuff[232];

  strncpy( cBuff, cText, nLen );
  cBuff[nLen] ='\x0';

  DISPXYA( nLeft, nTop, nColor, cBuff );
}

void _gtRepChar( int nTop, int nLeft, int nChar, int nRep )
{
  REPXYA( nLeft, nTop, nColor, nRep, (char)nChar );
}

void _gtColorSelect( int nClr )
{
  nColor =nClr+1;
}

#endif

/*
 *
 **
 *
*/


/* Find the beginning of previous line starting from given offset
 */
static long int Prev( EDITOR *E, long int adres )
{
   long int i;

   if( adres > 0 )
   {
      for( i = adres; i >= 0; i-- )
      {
         if( E->begin[ (unsigned int) i ] == '\n' )
         {
            if( i < adres-2 )
               return( i + 1 );
         }
      }
      return( 0 );
   }

   return( -1 );
}

/* Find the beginning of next line starting from given offset
 */
static long int Next( EDITOR *E, long int adres )
{
   char *tmp;

   tmp =strchr( E->begin + (unsigned int) adres, '\n' );

   if( tmp && tmp[1] )
      return( (long int)(++tmp - E->begin ) );
   else
      return( -1 );
}

/* Initializes EDITOR structure
 */
static void New(EDITOR *E, int tab, int ll, long int BuforSize)
{

  E->line_length    = ll;
  E->first_line     = 0;
  E->last_line      = 0;
  E->current_line   = 0;
  E->first_display  = 0;
  E->last_display   = 0;
  E->cursor_row     = 0;
  E->cursor_col     = 0;
  E->first_col      = 0;
  E->stabil         = 0;
  E->current_stabil = 0;
  E->stable         = FALSE;
  E->tab_size       = tab;
  E->active         = 1;
  E->line_number    = 0;
  E->IsConfigured   = 0;
  E->text_length    = 0;
  E->bufor_size     = BuforSize;

  E->begin [ 0 ]    = '\r';
  E->begin [ 1 ]    = '\n';
  E->begin [ 2 ]    = '\x0';

}



/*
 ** Creates new editor and returns index into internal editors table
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_NEW )
#else
int HB_ED_NEW( int ll, int tab, long int BuforSize )
#endif
{
   EDITOR *E;
   int nEdit=0;

#ifdef mc51
   int      ll, tab;
   long int BuforSize;
#endif

   /* Find the free slot fpr new editor
    */
   while ( (nEdit < 10 ) && (ETab[nEdit] != NULL) )
      nEdit++;
   if( nEdit == 10 )
   #ifdef mc51
      _retni(-1);    /* no more free slots */
   #else
      return(-1);
   #endif

   #ifdef mc51
      ll  = _parni(1);
      if( ll > MAX_LINE_LEN )
         ll = MAX_LINE_LEN;
      tab = _parni(2);
      E   = (EDITOR *)_xalloc( sizeof(EDITOR) );
   #else
      E = calloc(sizeof(EDITOR),1);
   #endif

   ETab[nEdit] = E;

   if( E )
   {
   #ifdef mc51
      if ( _parinfo ( 0 ) < 3 )
         BuforSize = 32767;
      else
         BuforSize = (long int )_parni ( 3 ) + 10;

      if( _parinfo( 0 ) < 4 )
         E->escape =0;
      else
         E->escape =(char)_parni(4);
   #else
         E->escape =0;
   #endif

   #ifdef mc51
      E->begin = (char *)_xalloc( (unsigned int) BuforSize+100 );
   #else
      E->begin = calloc( (unsigned int) BuforSize+100, 1 );
   #endif
      memset( E->begin, '\x0', (unsigned int) BuforSize );

      New(E, tab, ll, BuforSize);

   #ifdef mc51
      _retni ( nEdit );
   #else
      return ( nEdit );
   #endif
   }
   else
   {
      #ifdef mc51
         _retni ( -1 );    /* failure */
      #else
         return ( -1 );
      #endif
   }
}

/* Replaces TAB with spaces and removes spaces from the end of line
 */
static void FormatText ( EDITOR *E )
{
   long int  j, dl;
   char      *wsk;
   int       i;
   unsigned int nLen, nEsc;

   dl              = E->current_line;
   E->current_line = E->last_line;

   /* TODO: remove this TAB replacement because it is time consuming
    * operation if a very large file is edited
    */
   wsk =E->begin +E->last_line;
   while( (wsk = strchr( wsk, '\t' )) )
   {
      j = wsk - E->begin;

      MoveText( E, j, j + E->tab_size-1,
                     (long int)( E->bufor_size - j - E->tab_size +1 ) );

      for( i = 0; i < E->tab_size; i++, wsk++ )
         *wsk = ' ';
   }

   /* TODO: optimize this line formating - format line only if
    * it will be displayed
    */
   while( E->current_line >= 0 )
   {
      E->last_line    =E->current_line;
      E->line_number++;

      nLen =Clear( E, E->current_line, &nEsc );

      if( !format_line( E, HARD, nLen ) )
         E->current_line = Next( E, E->current_line );
   }

   E->current_line = dl;
   E->first_col = 0;
}

/* Resets the editor state after pasting new content of text buffer
 */
static void NewText( EDITOR *E )
{
   unsigned int i, dl;

   /* text in buffer have to end with CR/LF
    */
   dl =(unsigned int)E->text_length;
   if( E->begin[ dl-1 ] != '\n' )
   {
      E->begin[ dl     ] = '\r';
      E->begin[ dl + 1 ] = '\n';
      E->begin[ dl + 2 ] = '\x0';
      E->text_length +=2;
   }

   FormatText( E );

   E->cursor_col     = 0;
   E->cursor_row     = 0;
   E->stable         = FALSE;
   E->current_stabil = 0;
   E->first_display  = E->last_display = 0;
   E->next_stabil    = 0;
   E->dir            = DOWN;
   E->stabil         = E->bottom - E->top + 1;

   for( i = 0; i < E->stabil; i++ )
      E->last_display =Next( E, E->last_display );
}

/* Appends passed text to the existing text buffer
 */
static void AddText( int nEdit, char *adres )
{
   EDITOR    *E;
   long int  dl, dlold;

   E = ETab [ nEdit ];

   dl    = strlen( adres );
   dlold = E->text_length;
   if( dlold == 2 )
      dlold =0;   /* if current text buffer contains CRLF only then discard it */

   /* TODO: add reallocation of text buffer
    */
   if( (dl+dlold) <= (E->bufor_size - 10) )
   {
      /* there is enough room in text buffer
       */
      strcpy( E->begin+dlold, adres );
      E->text_length +=dl;
   }
   else
   {
      strncpy( E->begin+dlold, adres, (int)(E->bufor_size -10 -dlold) );
      E->text_length =E->bufor_size - 10;
   }

   NewText( E );     /* reformat text */
}

/*
 ** Appends passed text at the end of existing one
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_ADDTEXT )
#else
CLIPPER HB_ED_ADDTEXT ( int nEdit, char *adres )
#endif
{
  #ifdef mc51
    char *adres;
    int nEdit;

    nEdit = _parni(1);
    adres = _parc(2);
  #endif

  AddText( nEdit, adres );
}

/* Moves text from one location into another
 */
static void MoveText ( EDITOR * E, long int source, long int dest,
                       long int ilb )
{
   long int diff;

   diff = dest - source;
   /* memmove supports overlapped buffers */
   memmove( E->begin + (unsigned int)dest, E->begin + (unsigned int)source,
            (unsigned int) ilb );

   if( E->last_display > E->current_line )
      E->last_display += diff;

   if ( E->current_line < E->last_line )
      E->last_line += diff;

   E->text_length +=diff;

   if( E->text_length > ( E->bufor_size - 8 ) )
   {
      E->text_length =E->bufor_size - 8;
      E->begin[ E->text_length ]    = '\x0';
      E->begin[ E->text_length-1 ]  = '\n';
      E->begin[ E->text_length-2 ]  = '\r';
   }
}

/* Skips to the beginning of given line
 */
static long int GoToLine( EDITOR *E, int linia )
{
   char *p;
   long int  i;

   i =0;
   p =E->begin;
   while( (++i <= linia) && (p=strchr( p, '\n' )) )
      p +=2;

   if( i > linia )
      return( p - E->begin -1 );
   else
      return( E->text_length );  /* no such line number - go to the end */
}

/* Counts the number of printable characters in given line
 */
static unsigned int GetLineLength( EDITOR *E, long int off, int *wsk )
{
   unsigned int i, j;
   char *p, *tmp;

   tmp =E->begin + (unsigned int)off;
   p   =strchr( tmp, '\n' );  /* find EOL starting from given position */

   if( p )
   {
      off =( p - tmp );
      i =(unsigned int)off -1;
   }
   else
      i =strlen( tmp );

   *wsk = 0;   /* number of characters used in color escape codes */
   if( E->escape )
   {
      for( j=0; j < i; j++ )
         if( (char)tmp[j] == E->escape )
         {
            (*wsk) +=2;
            j++;
         }
   }

   return( i - *wsk );  /* number of all chars minus number of escape chars */
}

/* Inserts text into existing text buffer starting from given line number
 */
static long int InsText ( EDITOR *E, char *adres, long int line )
{
   long int  dl, off, il, dl1;
   int       addCRLF, cc;

   addCRLF = 0;
   dl  = strlen ( adres );    /* length of text to insert */
   dl1 = E->text_length;      /* length of text that is currently in the buffer */

   /* TODO: add reallocation  of text buffer
    */
   if( dl1 < (E->bufor_size - 10) )
   {
      /* there is some free space in text buffer
       */
      /* Find the offset of given line */
      if( line > 0 )
         off = GoToLine( E, (unsigned int) line ); /* Find the offset of given line */
      else
         off = 0;

      if( (long int)(dl + dl1) < (E->bufor_size-10) )
      {
         /* there is enough free room in text buffer
          */
         if ( (adres[(unsigned int) dl-1] != '\n') && (adres[(unsigned int) dl-2] != '\r') )
         {
            /* There is no CRLF at the end of inserted text -
             * we have to add CRLF to separate it from existing text
             */
            addCRLF = 1;
            dl += 2;
         }
         MoveText( E, off, off + dl, E->bufor_size - (off - 1) - dl );
         strncpy( E->begin + (unsigned int) off, adres , (unsigned int) dl );
      }
      else
      {
         /* not enough free space
          * text at the end of existing text buffer will be lost
          */
         dl = E->bufor_size - 10 - dl1;
         if( adres[ (unsigned int)dl-1 ] == '\r' )
            adres[ (unsigned int)dl-1 ] = ' ';

         if( (adres[(unsigned int) dl-1] != '\n') && (adres[(unsigned int) dl-2] != '\r') )
         {
            addCRLF = 1;
            dl += 2;
         }
         MoveText( E, off, off + dl, E->bufor_size - (off - 1) - dl );
         strncpy ( E->begin + (unsigned int) off, adres , (unsigned int) dl );
      }

      if( addCRLF )
      {
         E->begin [(unsigned int) (off + dl - 2)] = '\r';
         E->begin [(unsigned int) (off + dl - 1)] = '\n';
         E->text_length += 2;
      }

      if ( ( off + dl ) == E->text_length )
         E->begin [ (unsigned int) E->text_length ] = '\x0';
      E->text_length = strlen ( E->begin );

      il = E->line_number;
      cc = E->cursor_col;

      FormatText ( E );

      E->cursor_col = cc;

      if ( off <= E->current_line )
      {
         E->current_line += E->text_length - dl1;
         E->active       += E->line_number - il;
      }
   }

   return ( dl );
}

/*
 ** Inserts passed text into text buffer
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_INSTEXT )
#else
unsigned int HB_ED_INSTEXT ( int nEdit, char *adres, long int linia )
#endif
{
   long int dl;
   EDITOR *E;

   #ifdef mc51
      char *adres;
      int nEdit;
      long int linia;

      nEdit = _parni(1);
      adres = _parc(2);
      linia = (long int) _parni(3);
   #endif

   E =ETab[nEdit];
   dl = InsText( E, adres, linia );
   E->last_line = Prev ( E, (long int) strlen ( E->begin ) );

   #ifdef mc51
      _retni ( (unsigned int) dl );
   #else
      return ( (unsigned int) dl );
   #endif
}

/*
 **
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_PUSH )
{
   int i;

   i =0;
   while( i < 5 && EStack[i] )
      i++;

   EStack[ i ] =ED;
}

CLIPPER_ACTION( HB_ED_POP )
{
   int i;

   i =0;
   while( i < 5 && EStack[i] )
      i++;

   if( i )
   {
      ED =EStack[ i-1 ];
      EStack[ i-1 ] =NULL;
   }
}
#endif

/*
 * Selects the editor as active - all next ED_*() calls will be send
 * to this editor.
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_CONFIG )
#else
CLIPPER HB_ED_CONFIG(int nEdit, int top, int left, int bottom, int right,
                       int nRow, int nCol)
#endif
{
   int  szer, wys;
   int      nszer, nwys;
   int      diff;
   long int tmp;
   long int j;

#ifdef mc51
   int top, left, bottom, right;
   int nRow, nCol;
   int nEdit, i;

   nEdit  = _parni(1);
   top    = _parni(2);
   left   = _parni(3);
   bottom = _parni(4);
   right  = _parni(5);
   nRow   = _parni(6);
   nCol   = _parni(7);
#endif

   ED = ETab[nEdit];  /* select the editor to work on */

   szer = ED->right - ED->left + 1;
   wys  = ED->bottom - ED->top + 1;

   ED->top        = top;
   ED->left       = left;
   ED->bottom     = bottom;
   ED->right      = right;

   ED->last_display = ED->first_display;
   ED->stabil       = ED->bottom - ED->top + 1;

   if( ED->IsConfigured )
   {
      /* In event driven world the position and size of the editor window can
      * change between activations - recalculate some required values
      */
      ED->first_display  = ED->current_line;

      /* find the first line to display - try to keep visible the current line
       * and display this line in the same row in the window (if possible)
       */
      for ( i = 0; i < ED->cursor_row; i++ )
      {
         j = Prev( ED, ED->first_display );
         if ( j >= 0 )
         ED->first_display = j;
         else
         ED->cursor_row--;
         if ( ED->cursor_row < 0 )
         ED->cursor_row = 0;
      }

      /* find the last line for display */
      for( i = 0; i < ED->bottom - ED->top; i++ )
      {
         j = Next ( ED, ED->last_display );
         if ( j >= 0 )
         ED->last_display  = j;
      }
   }
   else
   {
      ED->first_display  = ED->first_line;

      /* find the last line for display */
      nwys =ED->bottom - ED->top;
      for( i = 0; i < nwys; i++ )
      {
         j = Next( ED, ED->last_display );
         if ( j >= 0 )
         ED->last_display  = j;
      }
      /* check if this line is empty */
      if( strlen(ED->begin+(unsigned int)ED->last_display) == 0 )
         ED->last_display = Prev( ED, ED->last_display );

      /* set initial cursor position in the window */
      ED->cursor_row = nRow;
      ED->cursor_col = nCol;
   }

   if( ED->IsConfigured )
   {
      nszer = ED->right - ED->left + 1;
      nwys  = ED->bottom - ED->top + 1;

      diff = abs(szer - nszer);
      if(szer < nszer)
      {
         /* current width of the window is greater then during previous activation
          * adjust the first visible column
          */
         if(ED->first_col > diff)
         {
            ED->first_col -= diff;
            ED->cursor_col+= diff;
         }
         else
         {
            ED->cursor_col+= ED->first_col;
            ED->first_col = 0;
         }
      }
      if(szer > nszer)
      { /* current width of the window is smaller then during previous activation
         */
         if(ED->cursor_col > (nszer - 1))
         {
            ED->first_col += ED->cursor_col - nszer + 1;
            ED->cursor_col = nszer - 1;
         }
      }

      diff = abs(nwys - wys);
      if(wys > nwys)
      {
         /* current height of the window is smaller then during previous activation
         */
         if( ED->cursor_row < nwys )
         {
            /* the old cursor row position is smaller then the window height
            */
            tmp  = ED->last_display;
            for ( i = 0; i < diff; i++ )
            {
               j = Prev ( ED, tmp );
               if ( j >= 0 )
                  tmp = j;

            }
            ED->last_display = tmp;
         }
         else
         {
            /* old cursor row position is greater then current window height
             *  display the line where the cursor is placed as the last visible
             *  line in the window
             */
            ED->last_display  = ED->current_line;
            tmp  = ED->last_display;

            for(i = 0; i < nwys-1; i++)
            {
               j = Prev ( ED, tmp );
               if ( j >= 0 )
                  tmp = j;
            }
            ED->first_display = tmp;
            ED->cursor_row = nwys - 1;
         }
      }
   }
   else
   {
      ED->current_line   = ED->first_line;
      ED->active         = 1;
   }

   ED->IsConfigured   = 1;
   ED->stable         = FALSE;
   ED->current_stabil = 0;
   ED->next_stabil    = ED->first_display;
   ED->dir            = DOWN;
}

/*
 ** Returns current text buffer
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_GETTEXT )
#else
char *HB_ED_GETTEXT(int nEdit, int Case, int mietka )
#endif
{
   long int  dl;
   char          *bufor, *help;
   EDITOR        *E;

#ifdef mc51
   int Case, mietka;
   int nEdit;

   nEdit  = _parni(1);
   Case   = _parni(2);
   mietka = _parni(3);
#else
   int p;
#endif

      E = ETab[nEdit];

      dl    = strlen ( E->begin ) +3;

   #ifdef mc51
      bufor = (char *)_xalloc((unsigned int) dl);
   #else
      bufor = calloc((unsigned int) dl,1);

      strcpy(name,"c:\\out.txt");
      p     = open(name,O_CREAT+O_WRONLY+O_BINARY);
   #endif

      strcpy ( bufor, E->begin );

      switch (Case)
      {
   /* IT USES INTERNAL CLIPPER FUNCTIONS
      #ifdef mc51
         case 0: _ncopyuc(bufor, bufor, (unsigned int) dl);
                  break;
         case 1: _ncopylc(bufor, bufor, (unsigned int) dl);
                  break;
      #else
   */
         case 0: bufor = strlwr(bufor);
                  break;
         case 1: bufor = strupr(bufor);
   /*     #endif */

         default:;
      }

      help = bufor;
      if(mietka != SOFT)
      {
         while(help !=NULL)
         {
            help = strstr(bufor,"\n");   /* CHR(141)+CHR(10) */
            if( help )
               help[0] = '\r';
         }
      }

#ifdef mc51
   _retc  ( bufor );
   _xfree ( bufor );
#else
   write(p, bufor, (unsigned int) dl);
   return(bufor);
#endif
}

/*
 ** Returns given line of text and positions caret at the beginning of next line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_GETLINE )
#else
char *HB_ED_GETLINE(int nEdit, long int linia )
#endif
{
   long int        l, j;
   long int        tmp;
   char            *bufor;
   EDITOR          *E;
   int             rdl, i, dl;

#ifdef mc51
   long int linia;
   int          nEdit;

   nEdit  = _parni(1);
   linia  = (long int) _parni(2);
#endif

   E = ETab[nEdit];

   l = 1;
   tmp = E->first_line;
   for(i = 1; i < (unsigned int) linia; i++)
   {
      j = Next ( E, tmp );
      if ( j >= 0 )
      {
         tmp = j;
         l++;
      }
   }
   if( l == linia )
   {
      dl = GetLineLength ( E, tmp, &rdl );
      dl++;
      #ifdef mc51
         bufor = (char *)_xalloc( dl );
      #else
         bufor = calloc( dl,1 );
      #endif

      strncpy ( bufor, E->begin + (unsigned int) tmp, dl-1 );
      bufor[ (dl-1) ] = '\x0';
   }

   E->next_line = Next ( E, tmp );

#ifdef mc51
   _retc  ( bufor );
   _xfree ( bufor );
#else
   return( bufor );
#endif
}

/*
 ** Returns current line pointed by caret position and advances it to the beginning of next line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_GETNEXT )
#else
char *HB_ED_GETNEXT(int nEdit)
#endif
{
   char             *bufor;
   EDITOR           *E;
   int              rdl, dl;

#ifdef mc51
   int nEdit;

   nEdit  = _parni(1);
#endif

   E = ETab[nEdit];
   if ( E->next_line > 0 )
   {
      dl = GetLineLength ( E, E->next_line, &rdl );
      dl++;
      #ifdef mc51
         bufor = (char *)_xalloc ( dl );
      #else
         bufor = calloc  ( dl, 1 );
      #endif

      strncpy ( bufor, E->begin + (unsigned int) E->next_line, dl-1 );
      bufor [ (unsigned int) (dl-1) ] = '\x0';
      E->next_line = Next ( E, E->next_line );

      #ifdef mc51
         _retc  ( bufor );
         _xfree ( bufor );
      #else

         DISPXYA( 1, 24, 5, bufor );
         return( bufor );
      #endif
   }
   else
   {
      #ifdef mc51
         _ret();
      #else
         DISPXYA( 1, 24, 5, "KONIEC" );
         return( NULL );
      #endif
   }
}

/* Resets text buffer
 */
static void KillText ( EDITOR *E )
{
   memset( E->begin, '\x0', (unsigned int) E->bufor_size );
   E->first_line = E->last_line = 0;
}

/*
 ** Stores new text into editor
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_SETTEXT )
#else
CLIPPER HB_ED_SETTEXT(int nEdit, char *adres)
#endif
{
   EDITOR *E;

#ifdef mc51
   char *adres;
   int  nEdit;

   nEdit = _parni (1);
   adres = _parc  (2);
#endif

   E = ETab[nEdit];

   KillText( E );

   New( E, E->tab_size, E->line_length, E->bufor_size );

   AddText ( nEdit, adres );
}

/*
 ** Reads a text from the file
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_READTEXT )
{
   unsigned int nEdit, nFile, nSize, lSuccess=FALSE;
   long int nSeek, nRead;
   EDITOR *E;
   BOOL lConv;

   nEdit =_parni( 1 );
   E =ETab[ nEdit ];

   KillText( E );
   New( E, E->tab_size, E->line_length, E->bufor_size );

   nFile =_parni( 2 );
   nSeek =_parnl( 3 );
   nSize =_parni( 4 );
   lConv =_parl( 5 );

   nRead =_fsSeek( nFile, nSeek, FS_SET );
   if( nRead == nSeek )
   {
      if( nSize > (unsigned int)(E->bufor_size-10) )
         nSize =(unsigned int)E->bufor_size-10;

      nSize =_fsRead( nFile, (unsigned char *)E->begin, nSize );
      E->begin[ nSize ] ='\x0';

      E->text_length =nSize;

/*
 * Characters with ASCII code 27 and 26 cannot be stored inside
 * a file - for this reason the replacement characters were used 181 and 198
 * and stored in a file on disk
 *
   if( lConv )
   {
      unsigned char *cPtr;

      cPtr =(unsigned char *) E->begin;
      while( nSize-- )
      {
         if( *cPtr == 181 )
            *cPtr =27;
         else if( *cPtr == 198 )
            *cPtr =26;

         cPtr++;
      }
   }
*/
      NewText( E );

      lSuccess =TRUE;
      E->stable =FALSE;
   }

   _retl( lSuccess );
}
#endif

/*
 ** Releases memory occupied by the editor
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_KILL )
#else
CLIPPER HB_ED_KILL(int nEdit)
#endif
{
   EDITOR *E;

#ifdef mc51
   int nEdit;

   nEdit = _parni(1);
#endif

   E = ETab[nEdit];
   KillText( E );

   #ifdef mc51
      _xfree ( E->begin );
   #else
      free ( E->begin );
   #endif

   #ifdef mc51
      _xfree(E);
   #else
      free(E);
   #endif

   ETab[nEdit] = NULL;
}

/* Sorry - I don't remember why it is here
 */
CLIPPER_ACTION( HB_ED_UNLOCK )
{
}

/*
 * Stabilize the editor
 * Redisplays all requested lines
 * It is simmilar to TBrowse:forceStable()
 * Incremental stabilisation was too slow
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_STABILIZE )
#else
int HB_ED_STABILI( void )
#endif
{
   unsigned int nRow=0, nEscLen, nLen, width, i,j, e;
   int nLeft, nTop;
   char *EscPtr;
   char *cPtr;
   char  adres[ MAX_LINE_LEN + 2 ];

   while( --ED->stabil >= 0 )
   {
      /* there are some lines of text to display
      */
      width =ED->right - ED->left + 1;

      if( ED->next_stabil >= 0 )
      {
         cPtr =ED->begin +ED->next_stabil;
         for( nEscLen=nLen=0; *cPtr && *cPtr != '\n'; cPtr++ )
         {
            /* copy the line into temporary buffer - count characters used
            * as color escape codes
            */
            adres[nLen++] =*cPtr;
            if( ED->escape && (char)*cPtr == ED->escape )
               nEscLen +=2;
         }
         j =nLen -nEscLen;    /* length of printable text */
         adres[nLen] = '\x0';

         if( ED->first_col >= j )
            adres[ nEscLen=nLen=0 ] ='\x0'; /* first visible column is greater then line length */

         else if( ED->first_col )
         {
            /* text is scrolled right - we need to find the first
             * color escape code that will be visible
             * text ~2 in bold ~1 in normal
             *          ^-first visible column
             */
            e =0;
            if( ED->escape )
            {
               for( i=0; i < (ED->first_col+e) ; i++ )
                  if( (char)adres[i] == ED->escape )
                  {
                     adres[0] =adres[i];
                     adres[1] =adres[++i];
                     e +=2;
                  };
            }

            if( e )
            {
               nLen -=(i-2);
               if( (char)adres[i-1] == ED->escape )
                  i++, nLen--;
               strncpy( adres+2, adres+i, nLen-2 );
               nEscLen -=(e-2);
            }
            else
            {
               nLen -=ED->first_col;
               strncpy( adres, adres+ED->first_col, nLen );
            }
            adres[ nLen ] = '\x0';
         }

         if( nLen )
            if( adres[ nLen-1 ] & '\xd' ) /* soft or hard carriage */
               adres[ --nLen ] = '\x0';

         /* find next line for displaying */
         switch( ED->dir )
         {
         case DOWN : ED->next_stabil =Next( ED, ED->next_stabil );
                     nRow =ED->current_stabil++;
                     break;

         case UP   : ED->next_stabil =Prev( ED, ED->next_stabil );
                     nRow =ED->current_stabil--;
                     break;

         default   : ;
         }

         _gtColorSelect( 0 );    /* select default color */

         nTop  =ED->top +nRow;
         if( nLen )
         {
            if( ED->escape && ( EscPtr = strchr( adres, ED->escape ) ) )
            {
               i  =(unsigned int)(EscPtr - adres);
               nLeft =ED->left +i;

               if( i )
                  _gtWriteAt( nTop, ED->left, (unsigned char *)adres, (width < i)?width:i );

               for( ; i < nLen && nLeft <= ED->right; i++ )
                  if( (char)adres[i] == ED->escape )
                     _gtColorSelect( (adres[++i] & '\x0F')-1 );
                  else
                     _gtWriteAt( nTop, nLeft++, (unsigned char *)adres+i, 1 );
            }
            else
               _gtWriteAt( nTop, ED->left, (unsigned char *)adres, ((width < nLen)?width:nLen ) );
         }

         /* fill the rest of the row with spaces */
         if( (nLen-nEscLen) < width )
            _gtRepChar( nTop, ED->left+nLen-nEscLen, ' ', width-nLen+nEscLen );
      }
      else
      {
         /* no more lines of text to display - fill the window with spaces
         */
         switch( ED->dir )
         {
         case DOWN : nRow = ED->current_stabil++;
                     break;
         case UP   : nRow = ED->current_stabil--;
                     break;
         default   : ;
         }

         _gtColorSelect( 0 );
         _gtRepChar( ED->top+nRow, ED->left, ' ', width );
      }

      _gtColorSelect( 0 );
   }

   ED->stable = TRUE;

#ifdef mc51
   _retni( nRow );
#else
   return( nRow );
#endif
}

#ifndef mc51
   void IInsert(void)
   {
      if(insert)
         insert = FALSE;
      else
         insert = TRUE;
   }
#endif

#ifndef mc51
   void init_window(void)
   {
   HB_ED_STABILI();
   }
#endif

/* Removes trailing spaces from the end of line
 */
static unsigned int Clear( EDITOR *E, long int e, unsigned int *nEsc )
{
   unsigned int nLen, i;

   nLen =GetLineLength( E, e, (int*)nEsc );
   i =(unsigned int)e + nLen + *nEsc;

   if( i )
      while( E->begin[ i-1 ] == ' ' )
      {
         if( E->cursor_col > 0 )
            E->cursor_col--;

         else if( E->first_col > 0 )
            E->first_col--;

         MoveText( E, (long int)i, (long int)i-1, E->text_length -i +3 );
         nLen--;
      }

   return( nLen );
}

/* Moves the cursor to the next line of text
 */
static void Down( EDITOR *E )
{
   long int j;
   unsigned int nEsc;

   j = Next( E, E->current_line );  /* find the offset of next line */
   if ( E->begin [ (unsigned int) j ] == '\x0' )
      j = -1;     /* no more lines */
   if ( j < 0 )
   {
      E->stable = TRUE;
   }
   else
   {
      E->active++;
      Clear( E, E->current_line, &nEsc );

      j = Next( E, E->current_line );
      if ( j >= 0 )
      {
         E->current_line = j;
      }
      if( ((++E->cursor_row)+E->top) > E->bottom )
      {
         /* attempt to move to the line that was not visible yet
          */
         #ifdef mc51
            E->stabil      = 1;  /* only one line needs to be redisplayed */
         #else
            E->stabil      = E->bottom - E->top + 1;
         #endif

         E->cursor_row     = E->bottom - E->top;
         E->first_display  = Next( E, E->first_display );
         E->last_display   = j;

         E->stable         = FALSE;
         E->next_stabil    = E->last_display;
         E->dir            = UP;
         E->current_stabil = E->cursor_row;
      }
      else
      {
         /* the new line is already visible */
         if( E->line_number <= (E->bottom - E->top + 1) )
            E->last_display = E->last_line; /* the total number of lines is smaller then rows to display */
      }
   }
   if( E->current_line > E->last_display )
      E->last_display = E->current_line;
   if( E->current_line > E->last_line )
      E->last_line = E->current_line;
}




/*
 ** Moves cursor to the next line of text
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_DOWN )
#else
CLIPPER HB_ED_DOWN(void)
#endif
{
   Down( ED );

#ifdef mc51
   _retl(ED->stable);
#endif
}

/* Moves the cursor to the previous line of text
 */
static void Up( void )
{
   int j, i;
   long int jj, tmp;
   unsigned int nEsc;

   /* find the previous line */
   jj = Prev ( ED, ED->current_line );
   if ( jj < 0 )
   {
      ED->stable = TRUE;
   }
   else
   {
      ED->active--;
      Clear( ED, ED->current_line, &nEsc );
      ED->current_line = jj;
      if( ((--ED->cursor_row)+ED->top) < ED->top )
      {
         /* the new line was not displayed yet */
      #ifdef mc51
         ED->stabil = 1;   /* only one line needs redisplay - rest of lines will be scrolled */
      #else
         ED->stabil = ED->bottom - ED->top + 1;
      #endif

         j              = 0;
         ED->cursor_row = 0;

         /* count the number of lines that will be visible in the window.
          * If the number of lines is smaller then the window height then
          * the offset of last displayed line will be not changed
          */
         ED->first_display  = Prev ( ED, ED->first_display );
         tmp                = ED->first_display;
         for(i = 0; i < ED->bottom - ED->top + 1; i++)
         {
            jj = Next ( ED, tmp );
            if ( jj >= 0 )
            {
               tmp = jj;
               j++;
            }
         }
         if( j == (ED->bottom - ED->top + 1) )
            ED->last_display   = Prev ( ED, ED->last_display );

         ED->stable         = FALSE;
         ED->next_stabil    = ED->current_line;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
   }
}

/*
 ** Moves the cursor to the previous line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_UP )
#else
CLIPPER HB_ED_UP(void)
#endif
{
  Up();

#ifdef mc51
   _retl(ED->stable);
#endif
}

/*
 ** Moves the cursor to the next page of text
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_PGDOWN )
#else
CLIPPER HB_ED_PGDOWN(void)
#endif
{
   int i;
   long int j;

   j = Next( ED, ED->last_display );
   if( ED->begin[ (unsigned int) j ] == '\x0' )
   {  /* no more lines */
      ED->stable = TRUE;
      /* advance the cursor as much as possible (to the last line) */
      for( i=0; i < ED->bottom - ED->top +1; i++ )
         Down( ED );
      return;
   }

   /* the last possible line will be displayed at the very bottom of the window */
   /* find the last line to display */
   for(i = 0; i < ED->bottom - ED->top; i++)
   {
      j = Next ( ED, ED->last_display );
      if ( j >= 0 )
      {
         if ( ED->begin [ (unsigned int)j ] != '\x0' )
         {
            ED->active++;
            ED->last_display = j;
         }
      }
      else
         break;   /* no more lines */
   }

   if ( ED->begin[ (unsigned int)ED->last_display ] == '\x0' )
      ED->last_display = Prev ( ED, ED->last_display );
   ED->first_display = ED->last_display;

   /* find the first displayed line now */
   for(i = 0; i < ED->bottom - ED->top; i++)
   {
      j = Prev ( ED, ED->first_display );
      if ( j >= 0 )
         ED->first_display = j;
      else
         ED->first_display = 0;
   }

   /* find the offset of the line where the currsor will be displayed */
   ED->current_line = ED->last_display;
   for(i = 0; i < ED->bottom - ED->top - ED->cursor_row; i++)
   {
      j = Prev ( ED, ED->current_line );
      if ( j >= 0 )
         ED->current_line = j;
      else
         ED->current_line = 0;
   }

   ED->stable         = FALSE;
   ED->next_stabil    = ED->first_display;
   ED->stabil         = ED->bottom - ED->top + 1;
   ED->dir            = DOWN;
   ED->current_stabil = 0;

}


/*
 ** Moves the cursor to the previous page of text
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_PGUP )
#else
CLIPPER HB_ED_PGUP(void)
#endif
{
   int i, bt;
   long int j;

   bt = ED->bottom - ED->top;

   j = Prev ( ED, ED->first_display );
   if( j < 0 )
   {  /* no more lines to move */
      ED->stable = TRUE;
      /* advannce the cursor to the topmost line */
      for(i=0; i < bt + 1; i++)
        Up();
      return;
   }

   /* find the offset of the first visible line */
   for(i = 0; i < bt; i++)
   {
      j = Prev ( ED, ED->first_display );
      if ( j >= 0 )
      {
         ED->active--;
         ED->first_display = j;
      }
      else
         break;   /* no more line */
   }
   /* now the last visible line */
   ED->last_display = ED->first_display;
   for(i = 0; i < bt; i++)
   {
      j = Next ( ED, ED->last_display );
      if ( j >= 0 )
        ED->last_display  = j;
   }

   /* update the offset of line where the cursor will be displayed
    * keep the cursor in the same row if possible
    */
   ED->current_line = ED->last_display;
   for(i = 0; i < bt - ED->cursor_row; i++)
   {
     j = Prev ( ED, ED->current_line );
     if ( j >= 0 )
       ED->current_line = j;
     else
       ED->current_line = 0;
   }

   ED->stable         = FALSE;
   ED->next_stabil    = ED->last_display;
   ED->stabil         = bt + 1;
   ED->dir            = UP;
   ED->current_stabil = bt;
}

/*
 ** Move the cursor to the beginning of the text
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_TOP )
#else
CLIPPER HB_ED_TOP(void)
#endif
{
   long int j;
   int      i;
   unsigned int nEsc;

   Clear( ED, ED->current_line, &nEsc );
   ED->current_line   = ED->first_line;
   ED->cursor_row     = 0;
   ED->first_display  = ED->last_display = ED->first_line;

   /* find the last visible line */
   for(i = 0; i < ED->bottom - ED->top; i++)
   {
      j = Next ( ED, ED->last_display );
      if ( j >= 0 )
         ED->last_display = j;
   }

   ED->cursor_row     = ED->current_stabil = 0;
   ED->current_line   = ED->next_stabil = ED->first_line;
   ED->active         = 1;
   ED->stable         = FALSE;
   ED->stabil         = ED->bottom - ED->top + 1;
   ED->dir            = DOWN;
   ED->first_col      = ED->cursor_col = 0;
}

/*
 ** Move the cursor to the last line of text
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_BOTTOM )
#else
CLIPPER HB_ED_BOTTOM(void)
#endif
{
   int      i, j;
   long int jj;
   unsigned int nEsc;

   j = 0;
   Clear( ED, ED->current_line, &nEsc );
   ED->current_line   = ED->last_line;
   ED->cursor_row     = 0;
   ED->first_display  = ED->last_line;

   /* find the first visible line */
   /* We have to count from the bottom to make it work in case the number
    * of lines is smaller then the height of the window
    */
   ED->last_display   = ED->first_display;
   for(i = 0; i < ED->bottom - ED->top; i++)
   {
      jj = Prev ( ED, ED->first_display );
      if ( jj >= 0 )
      {
         ED->first_display = jj;
         j++;
      }
   }

   ED->current_line   = ED->last_display;
   ED->active         = ED->line_number;
   ED->stable         = FALSE;
   ED->next_stabil    = ED->first_display;
   ED->dir            = DOWN;
   ED->current_stabil = 0;
   ED->first_col      = ED->cursor_col = 0;
   ED->cursor_row     = j;
   ED->stabil         = ED->bottom - ED->top + 1;
}

/* Go to the specified line number
 */
static void GoTo(int line)
{
   int          i;
   long int j;
   unsigned int nEsc;

   Clear( ED, ED->current_line, &nEsc );

   /* find specified line */
   ED->current_line = ED->first_line;
   for(i = 0; i < line-1; i++)
   {
      j = Next( ED, ED->current_line );
      if ( j >= 0 )
         ED->current_line = j;
   }
   ED->cursor_row     = 0;
   ED->first_display  = ED->current_line;

   /* find the offset of the last visible line */
   ED->last_display   = ED->first_display;
   for(i = 0; i < ED->bottom - ED->top; i++)
   {
      j = Next ( ED, ED->last_display );
      if( j >= 0 )
         ED->last_display = j;
   }

   ED->active          = line;
   ED->stable          = FALSE;
   ED->next_stabil     = ED->current_line;
   ED->stabil          = ED->bottom - ED->top + 1;
   ED->dir             = DOWN;
   ED->current_stabil  = ED->cursor_row;
   ED->first_col       = 0;
}

/*
 ** Move the cursor to the given line using line number
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_GOTO )
#else
CLIPPER HB_ED_GOTO(long int line)
#endif
{
#ifdef mc51
   long int line;

   line = (long int)_parni(1);
#endif

   GoTo((unsigned int) line);
}

/* Move the cursor to the previous character
 */
static void Left(void)
{
   if( ED->cursor_col > 0 )
      ED->cursor_col--;    /* inside the window - decrement current column number */
   else
   {
      if( ED->first_col > 0 )
      {
         /* the text is scrolled right - scroll it back to the left by one column */
         ED->first_col--;
         ED->stable         = FALSE;
         ED->next_stabil    = ED->first_display;
         ED->stabil         = ED->bottom - ED->top + 1;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
      /* else no wrap allowed */
   }
}

/*
 ** Move the cursor to the previous character
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_LEFT )
#else
CLIPPER HB_ED_LEFT(void)
#endif
{
   Left();
}


/* Move the cursor to the next character
 */
static void Right( EDITOR *E )
{
   if( E->cursor_col < (E->right - E->left) )
   {  /* inside the window */
      if( (E->first_col + E->cursor_col) < E->line_length )
         E->cursor_col++;
      /* else no wrap allowed */
   }
   else
   {
      /* scroll text to the right no more then the maximal line length */
      if( (++E->first_col + E->cursor_col) > E->line_length )
         E->first_col--;

      E->stable         = FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
}

/*
 ** Move the cursor to the next character
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_RIGHT )
#else
CLIPPER HB_ED_RIGHT(void)
#endif
{
   Right( ED );
}

/* Move the cursor to the beginning of line
 */
static void Home( EDITOR *E )
{
   if( E->first_col > 0 )
   {
      /* the line was scrolled to the right */
      E->cursor_col     = 0;
      E->first_col      = 0;
      E->stable         = FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;

   }
   else
   {
      E->cursor_col = 0;
      E->first_col  = 0;
   }
}


/*
 ** Move the cursor to the beginning of the line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_HOME )
#else
CLIPPER HB_ED_HOME(void)
#endif
{
   Home( ED );
}

/* Move the cursor to the end of line
 */
static void End( EDITOR *E )
{
   unsigned int ll, nEsc;

   ll =Clear( E, E->current_line, &nEsc );
   if( ll < E->first_col )
   {
      /* the line length is smaller then the number of characters scrolled -
       * adjust the first visible column to make the end of line visible
       */
      E->first_col      = ll;
      E->cursor_col     = 0;
      E->stable         = FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
   else
      if( ( ll - E->first_col ) > (E->right - E->left) )
      {
         /* scroll text to the right  */
         E->cursor_col     = E->right - E->left;
         E->first_col      = (int ) ( ll - (E->right - E->left) );
         E->stable         = FALSE;
         E->next_stabil    = E->first_display;
         E->stabil         = E->bottom - E->top + 1;
         E->dir            = DOWN;
         E->current_stabil = 0;
      }
      else
         E->cursor_col = ll - E->first_col;
}


/*
 ** Move the cursor the the end of line (after the last non-space character)
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_END )
#else
CLIPPER HB_ED_END(void)
#endif
{
   End( ED );
}


/* Format the current paragraph
 */
static void FormatParagraph ( EDITOR *E )
{
   int       rdl, cc, cr, cor;
   long int  dl, source, CrLine;
   char      pom[ MAX_LINE_LEN * 2 ], *tmp;
   unsigned int nEsc;

   cc  = E->cursor_col;
   cr  = E->cursor_row;
   cor = 0;

   rdl       = format_line( E, SOFT, 0 );
   E->stabil = 1;    /* at least one line will be redisplayed */

/*  if ( rdl )
*/
   {
      dl = (long int)GetLineLength( E, E->current_line, &rdl );
      strncpy( pom, E->begin+(unsigned int)E->current_line, (int)(dl+rdl+10) );
      pom[ E->line_length + rdl + 1 ] = '\x0';
      tmp = strchr( pom, '\n' );
      if( tmp && ((unsigned char)*(tmp-1) == 141u) )  /* soft CR */
      {
         tmp--;
         cor++;
      }
      else
         tmp = NULL;

      CrLine = E->current_line;

      while( tmp )
      {
         source   = E->current_line + (long int)(tmp - pom - 1);
         MoveText ( E, source+2, source+1, E->bufor_size - source + 2 );
         E->begin[ (unsigned int)(source+1) ] = ' ';

         rdl = format_line( E, SOFT, 0 );
         Clear( E, E->current_line, &nEsc );

         E->current_line = Next( E, E->current_line );
         dl  = (long int)GetLineLength( E, E->current_line, &rdl );
         strncpy( pom, E->begin+(unsigned int)E->current_line, (int)(dl+rdl+10) );
         pom[ E->line_length + rdl + 1 ] = '\x0';
         tmp    = strchr( pom, '\n' );
         if( tmp && ((unsigned char)*(tmp-1) == 141u) )
         {
            tmp--;
            cor++;
         }
         else
         tmp = NULL;

      }
      E->current_line   = CrLine;
      E->stabil         = E->bottom - E->top + 1;
      E->next_stabil    = E->first_display;
      E->current_stabil = 0;
   }
/*  else
*/
   {
      E->stabil = 1;
      E->next_stabil    = E->current_line;
      E->current_stabil = E->cursor_row;
   }

   E->stable      = FALSE;
   E->dir         = DOWN;
   E->cursor_col  = cc;
   E->cursor_row  = cr;
   E->line_number-= cor;
}



/* Delete the character under the cursor
 */
static void DelChar ( EDITOR *E )
{
   int ccc, rdl;
   long int cl;

   cl  = E->current_line;
   ccc = E->cursor_col + E->first_col;
   if( ccc <= GetLineLength( E, E->current_line, &rdl ) )
   {
      if( E->escape )
         while( (char)E->begin[ (unsigned int)(E->current_line + ccc) ] == E->escape )
            ccc += 2;
      MoveText ( E, E->current_line + (long int) (ccc + 1),
                    E->current_line + (long int) ccc,
                    (E->bufor_size - ( E->current_line + (long int)(ccc + 1))));

      E->stable         = FALSE;
      E->next_stabil    = E->current_line;

      E->dir            = DOWN;
      E->current_stabil = E->cursor_row;

      FormatParagraph ( E );

      if( E->current_line == E->last_line )
         E->last_display = E->last_line;

      E->current_line = cl;

      E->stable         = FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
}

/*
 ** Delete the character at current cursor position
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_DELCHAR )
#else
CLIPPER HB_ED_DELCHAR(void)
#endif
{
   long int j;
   int      rdl;

   if ( ( (unsigned int) (ED->cursor_col + ED->first_col) ) >=
         ( (unsigned int) GetLineLength ( ED, ED->current_line, &rdl ) ) )
   {
      /* The cursor is positioned after the last non-space character
       */
      j = Next ( ED, ED->current_line );
      if ( j >= 0 )
      {
         /* there are more lines below the cursor - join the lines
          */
         Down( ED ); /* goto the next line */
         Home( ED ); /* goto the beginning of line */
         BackSpace(1);  /* delete separating CR/LF */

         ED->stable         = FALSE;
         ED->next_stabil    = ED->first_display;
         ED->stabil         = ED->bottom - ED->top + 1;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
   }
   else
   {
      /* The cursor is inside the line or at the last character
       */
      if ( (unsigned int) ( ED->cursor_col + ED->first_col ) <
            (unsigned int) ( GetLineLength ( ED, ED->current_line, &rdl ) ) )
         DelChar ( ED );   /* inside a line */
      else
      { /* at the last character */
         j = Next ( ED, ED->current_line );
         if ( j >= 0 )  /* if it is not the last line then delete character unde the cursor */
            DelChar ( ED );
      }
   }
}

/* Delete a character on the left side of the cursor
 */
static void BackSpace( int INS )
{
   char      tmp[ MAX_LINE_LEN + 2 ], tmp1[ MAX_LINE_LEN + 2 ], *w;
   long int  ww, j, ccc, kk;
   int       rdl, nLen;

   ED->stable         = FALSE;
   ED->next_stabil    = ED->current_line;
   ED->stabil         = 1;
   ED->dir            = DOWN;
   ED->current_stabil = ED->cursor_row;

   if( INS )
   {
      if( ( ccc = ED->cursor_col+ED->first_col ) >
                  GetLineLength( ED, ED->current_line, &rdl ) )
         INS = 0; /* cursor is scrolled after the last character in the line - just move the cursor left */
   }

   if( INS )
   {
      /* in destructive mode
       */
      *tmp  ='\x0';
      *tmp1 ='\x0';
      if( (ccc = ED->cursor_col+ED->first_col) > 0 )
      {
         /* inside the line */
         MoveText ( ED, ED->current_line + ccc, ED->current_line + ccc - 1,
                  (ED->bufor_size - (ED->current_line + (long int) ccc + 1)));
         Left();
      }
      else
      {
         /* at the beginning of the line */
         if( ED->current_line != ED->first_line )
         {
            /* this is not the first line */
            nLen =GetLineLength( ED, ED->current_line, &rdl );

            if( ED->current_line == ED->last_line )
               if( nLen == 0 )
                  ED->last_line = Prev( ED, ED->last_line );

            /* copy the last line into temporary buffer */
            strncpy( tmp, ED->begin + (unsigned int) ED->current_line, nLen+rdl );
            tmp[ nLen+rdl ] = '\x0';

            /* find the first space in current line (the new line will
             * be wrapped eventually at this position) */
            if( ( w = strchr ( tmp, ' ') ) )
               ww = (int)(w - tmp);
            else
               ww = nLen+rdl;

            /* go to the previous line */
            j = Prev ( ED, ED->current_line );
            kk = GetLineLength( ED, j, &rdl );
            strncpy( tmp1, ED->begin + (unsigned int) j, (unsigned int)( kk + rdl ) );
            tmp1[ (unsigned int)( kk + rdl )  ] = '\x0';
            Up();
            End( ED );

            /* the lines can be joined
             * the sum of whole length of these lines is smaller then maximal allowed
             * or there is a space where the line can be wrapped
             */
            if( (ww + kk + rdl - 1) < ( ED->line_length ) )
            {
               kk = GetLineLength ( ED, ED->current_line, &rdl );
               j  = ED->current_line + kk + rdl;
               /* remove separating CRLF characters */
               MoveText( ED, j + 2, j, ED->bufor_size - j - 2 );

               ED->line_number--;

               j = Next ( ED, ED->last_display );
               if ( j >= 0 )
                  ED->last_display = j;
               if ( ED->begin[ (unsigned int) ED->last_display + 1 ] == '\x0' )
                  ED->last_display = Prev ( ED, ED->last_display );

               /* split the new line if it is too long */
               format_line( ED, HARD, 0 );

               j = Next ( ED, ED->current_line );
               if ( j < 0 )
               {
                  ED->last_display    = Prev ( ED, ED->last_display );
                  ED->last_line       = ED->current_line;
               }
               ED->stable         = FALSE;
               ED->next_stabil    = ED->first_display;
               ED->stabil         = ED->bottom - ED->top + 1;
               ED->dir            = DOWN;
               ED->current_stabil = 0;
            }
         }
      }
      FormatParagraph ( ED );

      ED->stable         = FALSE;
      ED->next_stabil    = ED->first_display;
      ED->stabil         = ED->bottom - ED->top + 1;
      ED->dir            = DOWN;
      ED->current_stabil = 0;
   }
   else
   {
      /* non-destructive mode - move cursor only */
      ccc = ED->cursor_col+ED->first_col;
      if( ccc > 0 )
      {
         Left();
      }
      else
      {
         if( ED->current_line != ED->first_line )
         {
            Up();
            End( ED );
         }
      }
   }
}


/*
 ** Delete a character on the left side of the cursor
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_BSPACE )
#else
CLIPPER HB_ED_BSPACE(int INS)
#endif
{
#ifdef mc51
   int INS;

   INS = _parl(1);   /* get current INSERT state  */
#endif

   BackSpace(INS);
}


/* Move to the beginning of next non-empty line
 */
static CLIPPER GotoNextNonEmptyLine(void)
{
   int  rdl;

   Down( ED );
   Home( ED );

   while( GetLineLength ( ED, ED->current_line, &rdl ) == 0 )
   {
      Down( ED );
      Home( ED );
      if( Next ( ED, ED->current_line ) < 0 )
         break;
   }
}


/* Move the cursor to the next word
 */
static void NextWord(void)
{
   char      *adr;
   char      tmp[ MAX_LINE_LEN + 2 ];
   int       ccc;
   unsigned int nEsc, nLen;

   ccc = ED->cursor_col + ED->first_col;
   nLen =Clear( ED, ED->current_line, &nEsc );

   if( nLen < ccc )
      GotoNextNonEmptyLine();
   else
   {
      *tmp ='\x0';
      strncpy( tmp, ED->begin + (unsigned int) ED->current_line + ccc,
                     nLen - ED->cursor_col - ED->first_col );
      tmp[ nLen - ED->cursor_col - ED->first_col ] = '\x0';
      if( (adr = strchr ( tmp, ' ' ) ) == NULL )
      {
         GotoNextNonEmptyLine();
         if( !ED->stable )
         {
            ED->next_stabil    = ED->first_display;
            ED->stabil         = ED->bottom - ED->top + 1;
            ED->dir            = DOWN;
            ED->current_stabil = 0;
         }
      }
      else
      {
         ED->cursor_col   = (int)( adr - tmp + 1 +ED->cursor_col + ED->first_col);

         if(ED->cursor_col > (ED->right - ED->left))
         {
            ED->first_col     += ED->cursor_col - (ED->right - ED->left);
            ED->cursor_col     = ED->right - ED->left;
            ED->stable         = FALSE;
            ED->next_stabil    = ED->first_display;
            ED->stabil         = ED->bottom - ED->top + 1;
            ED->dir            = DOWN;
            ED->current_stabil = 0;
         }
      }
   }

   if( ED->begin[ (unsigned int) (ED->current_line +
                     ED->cursor_col + ED->first_col) ] == ' ')
      NextWord();
}


/*
 ** Move the cursor to the next word
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_NWORD )
#else
CLIPPER HB_ED_NWORD(void)
#endif
{
   NextWord();
}

/* Move the cursor to the previous word
 */
static void PreviousWord(void)
{
   int       pom, i, rdl;
   unsigned int nLen, nEsc;

   pom = -1;
   nLen =Clear ( ED, ED->current_line, &nEsc );
   if( nLen < ( ED->cursor_col + ED->first_col ) )
      End( ED );

   if( ( ED->first_col + ED->cursor_col ) == 0 )
   {
      Up();
      End( ED );
   }

   for ( i = ED->first_col + ED->cursor_col - 2; i >= 0; i-- )
   {
      if ( ED->begin [ (unsigned int) (ED->current_line + i) ] == ' ')
      {
         pom = i;
         break;
      }
      else
         pom = -1;
   }

   if(pom < 0)
   {
      Home( ED );
      while( GetLineLength( ED, ED->current_line, &rdl ) == 0 )
      {
         Up();
         Home( ED );
         if( Prev( ED, ED->current_line ) < 0 )
            break;
      }
      if(!ED->stable)
      {
         ED->next_stabil    = ED->first_display;
         ED->stabil         = ED->bottom - ED->top + 1;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
   }
   else
   {
      ED->cursor_col = pom + 1;
      if( ED->first_col > 0 )
      {
         ED->cursor_col-= ED->first_col;
      }
      if( ED->cursor_col < 0 )
      {
         ED->first_col      = ED->cursor_col - (ED->right - ED->left);
         ED->stable         = FALSE;
         ED->next_stabil    = ED->first_display;
         ED->stabil         = ED->bottom - ED->top + 1;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
   }

   if ( ED->begin [ (unsigned int) (ED->current_line + ED->cursor_col +
                          ED->first_col) ] == ' ')
      PreviousWord();
}


/*
 ** Move the cursor to the previous word
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_PWORD )
#else
CLIPPER HB_ED_PWORD(void)
#endif
{
   PreviousWord();
}


/* Format given line - returns it the line has changed
 */
static int format_line( EDITOR *E, int Karetka, unsigned int LineDl )
{
   char      pom[ MAX_LINE_LEN * 2 ], *p;
   int       podz, jj, status, i;
   long int  j;
   int       rdl = 0;

   if( !LineDl )
      LineDl = GetLineLength( E, E->current_line, &rdl );

   status = 0; /* the line is not splitted yet */
   if( LineDl > E->line_length )
   {
      /* the line is longer then maximal allowed length  -
       * wrap the line
       */
      status = 1; /* the line will be splitted */

      /* copy maximum allowed bytes form the line into temporary buffer */
      strncpy( pom, E->begin+(unsigned int)E->current_line,
               (int)( E->line_length + 10 + rdl ) );
      pom[ (unsigned int)(E->line_length + rdl) ] = '\x0';

      /* find the last space where the line can be splitted */
      p = strrchr( pom, ' ' );
      if( p )
      {
         /* use this position to split the line */
         podz = (int)( p - pom + 1 );
         jj   = 1 - podz + E->cursor_col + E->first_col;
      }
      else
      {
         /* there is no space in the line - split it at the maximal line length */
         podz = E->line_length;
         jj   = 1;
      }

      j = (long int) (E->current_line + podz );
      MoveText ( E, j, j + 2, E->bufor_size - j - 2 );

      /* replace with separators */
      E->begin [ (unsigned int) j + 0 ] = (char)Karetka;
      E->begin [ (unsigned int) j + 1 ] = '\n';
      E->line_number++;

      if( ( E->cursor_col + E->first_col ) >= podz )
      {
         Home( E );
         Down( E );
         for( i = 0; i < jj; i++ )
         Right( E );
      }
      else
         Right( E );
   }

   return ( status );
}

/* Appends the character at the end of line
 */
static int AppendChar( EDITOR *E, int znak, int podz )
{
   int       diff, rdl, status;
   long int  iPos, nLen;
   long int  cl;
   int       ccol, fcol;
   char *cNew;

   nLen = GetLineLength ( E, E->current_line, &rdl );
   iPos = E->current_line + nLen ;
   diff = E->cursor_col + E->first_col - (int) nLen;

   /* the cursor is positioned 'diff' columns after the last character
    * in the line - fill the gap with spaces before appending the character
    */
   MoveText ( E, iPos, iPos + diff + 1, E->bufor_size - 1 - iPos - diff );
   memset( E->begin + (unsigned int) (E->current_line + nLen), ' ', diff );

   /* move the CRLF characters after the appended character */
   cNew  =E->begin +iPos +diff;
   *cNew++ = (char) znak;
   if( *cNew != '\r' )
      *cNew = (char) podz; /* insert requested soft/hard carriage */
   *++cNew = '\n';

   /* the last line always have to end with the hard carriage return */
   E->begin [ (unsigned int) E->text_length - 2 ] = '\r';

   status = format_line( E, SOFT, 0 );

   cl     = E->current_line;
   ccol   = E->cursor_col;
   fcol   = E->first_col;

   FormatParagraph( E );

   E->current_line = cl;
   E->cursor_col   = ccol;
   E->first_col    = fcol;

   return ( status );
}


/* Checks if there is enough free room in the text buffer
 */
static int Check_length( int iRequested )
{
   if ( ( ED->text_length + iRequested ) <= ( ED->bufor_size - 8 ) )
      return( 1 );
   else
      return( 0 );
}


/* Adjusts the offset of last line
 */
static void SetLastLine( EDITOR *E )
{
   if ( E->current_line > E->last_line )
      E->last_line = E->current_line ;
}


/* Insert or replace the new character into the text buffer
 */
static void PutChar( int INS, int znak )
{
   long int  i, jj, cc;
   int       rdl;
   long int  cl;
   int       ccol, fcol;

   jj = 0;
   cc = ED->cursor_col + ED->first_col;   /* currnt position in the line */
   if( INS )
   {
      /* INSERT is ON */
      if ( Check_length( 1 ) )
      {
         /* TODO: add reallocation of text buffer
          */
         if( (unsigned int) cc < GetLineLength ( ED, ED->current_line, &rdl ) )
         {
            /* the character will be inserted within the line - the cursor
             * is placed inside the line
             */
            i = ED->current_line + cc;
            MoveText( ED, i, i + 1, ED->bufor_size - ED->current_line - cc - 1);
            ED->begin [ (unsigned int) i ] = (char) znak;

            jj = format_line( ED, SOFT, 0 );

            cl     = ED->current_line;
            ccol   = ED->cursor_col;
            fcol   = ED->first_col;

            FormatParagraph( ED );

            ED->current_line = cl;
            ED->cursor_col   = ccol;
            ED->first_col    = fcol;

            if ( jj )
               SetLastLine( ED );
         }
         else  /* the cursor is located after the last character in the line */
            jj = AppendChar(ED, znak, SOFT);

         if( !jj )
            Right ( ED );
         else
            SetLastLine( ED );
      }
   }
   else
   {
      if((long int) cc < (GetLineLength(ED, ED->current_line, &rdl)))
      {
         ED->begin [ (unsigned int)(ED->current_line + cc) ] = (char)znak;
         jj = 0;
         Right ( ED );
      }
      else
         if ( Check_length( 1 ) )
         {
            jj = AppendChar(ED, znak, SOFT);
            if( !jj )
               Right ( ED );
            else
               SetLastLine( ED );
         }
   }

   if( !jj )
      if( ( ED->cursor_col + ED->first_col ) > ( ED->right - ED->left ) )
         jj = 1;

   if(jj)
   {
      ED->stabil = ED->bottom - ED->top + 1;
      ED->next_stabil    = ED->first_display;
      ED->current_stabil = 0;
   }
   else
   {
      ED->stabil = 1;
      ED->next_stabil    = ED->current_line;
      ED->current_stabil = ED->cursor_row;
   }

   ED->stable = FALSE;
   ED->dir    = DOWN;
}


/*
 ** Insert or replace the character into the text buffer
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_PUTCHAR )
#else
CLIPPER HB_ED_PUTCHAR(int INS, int znak)
#endif
{
#ifdef mc51
   int INS, znak;

   znak = _parni(1);    /* character to paste */
   INS  = _parl(2);     /* current INSERT state */
#endif

   PutChar( INS, znak );
}


/*
static void Tab ( int INS )
{
  if(INS)
    for(i=0; i<ED->tab_size; i++)
      PutChar(TRUE, 32);
  else
    for(i=0; i<ED->tab_size; i++)
      Right();
}
*/

/*
 **
*/

/*
#ifdef mc51
CLIPPER_ACTION( HB_ED_TAB )
#else
CLIPPER HB_ED_TAB(int INS)
#endif
{
#ifdef mc51
  int INS = _parl(1);
#endif

  Tab ( INS );
}
*/



/*
 ** Delete the current line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_DELLINE )
#else
CLIPPER HB_ED_DELLINE(void)
#endif
{
   long int  tmp, j;

   if( ED->active < ED->line_number )
   {
      j = Next ( ED, ED->last_display );
      if ( j >= 0 )
         ED->last_display   = j;

      tmp = Next ( ED, ED->current_line );
      if( tmp < 0 )
         tmp = 0;

      ED->stabil = ED->bottom - ED->top + 1 - ED->cursor_row;
      ED->dir    = DOWN;

      MoveText( ED, tmp, ED->current_line, ED->bufor_size - ED->current_line - 2 );

      if(ED->line_number > 0)
         ED->line_number--;

      ED->next_stabil    = ED->current_line;
      ED->current_stabil = ED->cursor_row;
      ED->stable         = FALSE;

   }
   else
   {
      ED->begin [ (unsigned int) ED->current_line + 0 ] = '\r';
      ED->begin [ (unsigned int) ED->current_line + 1 ] = '\n';
      ED->begin [ (unsigned int) ED->current_line + 2 ] = '\x0';
      memset( ED->begin + (unsigned int) ED->current_line + 2, '\x0',
               (unsigned int) (ED->bufor_size - strlen (ED->begin )) );

      ED->last_display   = ED->last_line;
      ED->stabil         = 1;
      ED->dir            = DOWN;
      ED->next_stabil    = ED->current_line;
      ED->current_stabil = ED->cursor_row;
      ED->stable         = FALSE;
   }
   if( ED->text_length == 0 )
   {
      ED->begin [ 0 ] = '\r';
      ED->begin [ 1 ] = '\n';
      ED->begin [ 2 ] = '\x0';
   }
}

/*
 ** Delete the word on the right side of the cursor
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_DELWORD )
#else
CLIPPER HB_ED_DELWORD(void)
#endif
{
   long int pos1, pos2, j;
   int cc, fc, cr, rdl;
   long int fd, ld;
   long int l;

   j = ED->current_line + ED->cursor_col + ED->first_col;
   if ( ED->begin [ (unsigned int) j ] != ' ' )
   {
      if((unsigned int) ( ED->cursor_col + ED->first_col ) <
         (unsigned int) ( GetLineLength ( ED, ED->current_line, &rdl ) ))
      {
         cc = ED->cursor_col;
         cr = ED->cursor_row;
         fc = ED->first_col;
         fd = ED->first_display;
         ld = ED->last_display;
         l  = ED->current_line;
         NextWord();
         pos2 = ED->cursor_col + ED->first_col;
         PreviousWord();
         pos1 = ED->cursor_col + ED->first_col;

         ED->current_line  = l;
         ED->cursor_col    = cc;
         ED->cursor_row    = cr;
         ED->first_col     = fc;
         ED->first_display = fd;
         ED->last_display  = ld;

         if(pos2 == 0)
         pos2 = GetLineLength ( ED, ED->current_line, &rdl );

         MoveText ( ED, ED->current_line + pos2, ED->current_line + pos1,
                  ED->bufor_size - ED->current_line - pos2 );
         FormatParagraph ( ED );
         ED->stable         = FALSE;
         ED->next_stabil    = ED->first_display;
         ED->stabil         = ED->bottom - ED->top + 1;
         ED->dir            = DOWN;
         ED->current_stabil = 0;
      }
      else
      {
         if( ( GetLineLength( ED, ED->current_line, &rdl ) ) == 0 )
         {
            HB_ED_DELLINE( );
            Home( ED );
         }
      }
   }
}


/* Insert the CRLF characters
 */
static void Return(int INS)
{
   long int ii, j;
   unsigned int nEsc, nLen;

   if( Check_length( 2 ) )
   {
      if( INS )
      {
         /* only if INSERT state is ON
          */
         nLen =Clear( ED, ED->current_line, &nEsc );

         ED->line_number++;

         j = Next( ED, ED->current_line );
         if ( j < 0 )
         {
            ED->last_line =ED->text_length;
            ED->text_length +=2;

            ED->begin[ ED->text_length -2 ] = '\r';
            ED->begin[ ED->text_length -1 ] = '\n';
            ED->begin[ ED->text_length    ] = '\x0';
         }
         else
         {
            if( (ED->first_col + ED->cursor_col) > nLen+1 )
               End( ED );
            ii = ED->current_line + (long int) ED->first_col +
                                    (long int) ED->cursor_col;
            MoveText ( ED, ii, ii + 2, ED->bufor_size - ii - 2 );

            ED->begin [ (unsigned int) ii + 0 ] = '\r';
            ED->begin [ (unsigned int) ii + 1 ] = '\n';
            if ( ED->last_line == ED->current_line )
               ED->last_line = Next ( ED, ED->current_line );
         }

         if( ED->cursor_row < (ED->bottom - ED->top) )
         {
            j = Prev ( ED, ED->last_display );
            if ( j > 0 )
               ED->last_display = j;

            ED->next_stabil    = ED->current_line;
            ED->stabil         = ED->bottom - ED->top + 1 - ED->cursor_row;
            ED->current_stabil = ED->cursor_row;
         }
         else
         {
            ED->next_stabil    = ED->first_display;
            ED->stabil         = ED->bottom - ED->top + 1;
            ED->current_stabil = 0;
         }
      }
   }
   else
   {
      Clear( ED, ED->current_line, &nEsc );
      j = Next ( ED, ED->current_line );
      if ( j > ED->last_line )
      {
         if ( Check_length( 2 ) )
         {
            ED->line_number++;
            ED->last_line       = j;
            ED->begin [ (unsigned int) j + 0 ] = '\r';
            ED->begin [ (unsigned int) j + 1 ] = '\n';
            ED->begin [ (unsigned int) j + 2 ] = '\x0';
         }
      }
   }
   if( Check_length( 0 ) )
   {
      ED->stable         = FALSE;
      ED->dir            = DOWN;
      Down( ED );
      Home( ED );
   }

   if( !ED->stable )
   {
      ED->next_stabil    = ED->first_display;
      ED->stabil         = ED->bottom - ED->top + 1;
      ED->current_stabil = 0;
      ED->dir            = DOWN;
   }
}


/*
 ** Insert the CRLF characters
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_RETURN )
#else
CLIPPER HB_ED_RETURN(int INS)
#endif
{
#ifdef mc51
   int INS = _parl(1);
#endif

   Return(INS);
}


/*
 ** Returns the current cursor row inside the editor's window
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_WINROW )
#else
int HB_ED_WINROW(void)
#endif
{
#ifdef mc51
   _retni( ED->cursor_row );
#else
   return( ED->cursor_row );
#endif
}

/*
 ** Returns the line number where the cursor is positioned
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_ROW )
#else
int HB_ED_ROW(void)
#endif
{
#ifdef mc51
   _retni( (unsigned int) ED->active );
#else
   return( (unsigned int) ED->active );
#endif
}


/*
 ** Return the current cursor column inside the editor's window
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_WINCOL )
#else
int HB_ED_WINCOL(void)
#endif
{
#ifdef mc51
   _retni( ED->cursor_col );
#else
   return( ED->cursor_col );
#endif
}

/*
 ** Returns the current cursor position inside the line
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_COL )
#else
int HB_ED_COL(void)
#endif
{
#ifdef mc51
   _retni( ED->cursor_col + ED->first_col + 1 );
#else
   return( ED->cursor_col + ED->first_col + 1 );
#endif
}


/*
 ** Returns the total number of lines
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_MAXLINE )
#else
long int HB_ED_MAXLINE(void)
#endif
{
#ifdef mc51
   _retni((unsigned int) ED->line_number);
#else
   return((unsigned int) ED->line_number);
#endif
}

/*
 ** Counts the total number of lines in passed editor
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_LCOUNT )
#else
long int HB_ED_LCOUNT(int nEdit)
#endif
{
   EDITOR *E;

#ifdef mc51
   E = ETab[ _parni(1) ];

   _retni( (unsigned int) E->line_number );
#else
   E = ETab[ nEdit ];

   return( (unsigned int) E->line_number );
#endif
}


/*
 ** Returns if the editor is correctly displayed
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_STABLE )
#else
int HB_ED_STABLE(void)
#endif
{
#ifdef mc51
   _retl( ED->stable );
#else
   return( ED->stable );
#endif
}


/*
 ** Returns the number of bytes stored in the text buffer
*/
#ifdef mc51
CLIPPER_ACTION( HB_ED_LENGTH )
#else
long int HB_ED_LENGTH(void)
#endif
{
#ifdef mc51
   _retni((unsigned int) ED->text_length);
#else
   return(ED->text_length);
#endif
}


/*
 *
 **  This is a standalone version of editor for testing
 * it requires home made screen library
 *
*/

#ifndef mc51

  CLIPPER Ed(EDITOR *E)
  {
  int          l;
  unsigned int i;
  char         *adres, pom[80];
  char         *str1, strall[ MAX_LINE_LEN + 2 ], str11[100];

    str1 = str11;
    while((i = READKEY(12)) != 283)
    {
    switch(i)
    {
      case UPARR  : HB_ED_UP();
                    break;
      case DNARR  : HB_ED_DOWN();
                    break;
      case 0x51E0u: HB_ED_PGDOWN();
                    break;
      case 0x49E0u: HB_ED_PGUP();
                    break;
      case 0x84E0u: HB_ED_TOP();
                    break;
      case 0x76E0u: HB_ED_BOTTOM();
                    break;
      case EEE    : HB_ED_GOTO( 1L );
                    break;
      case LTARR  : HB_ED_LEFT();
                    break;
      case RTARR  : HB_ED_RIGHT();
                    break;
      case 0x47E0u: HB_ED_HOME();
                    break;
      case 0x4FE0u: HB_ED_END();
                    break;
      case DEL    : HB_ED_DELCHAR();
                    break;
      case 0x0E08u: HB_ED_BSPACE(insert);
                    break;
      case 0x74E0u: HB_ED_NWORD();
                    break;
      case 0x73E0u: HB_ED_PWORD();
                    break;
      case 0x1519u: HB_ED_DELLINE();
                    break;
      case 0x1414u: HB_ED_DELWORD();
                    break;
      case ENTER  : HB_ED_RETURN(insert);
                    break;
      case 33333u : HB_ED_ROW();
                    break;
      case 44444u : HB_ED_COL();
                    break;
      case 55555u : HB_ED_STABLE();
                    break;
      case INSERT : IInsert();
                    break;
      default     : i &= 0xff;
                    HB_ED_PUTCHAR(insert, i);
                    break;
    }
    l= HB_ED_STABILI();

      strcpy( strall, "col ");
      str1   = itoa( E->cursor_col+E->first_col, str1, 10 );
      strcat( strall, str1 );
      strcat( strall, "  row " );
      str1   = itoa( E->cursor_row, str1, 10 );
      strcat( strall, str1 );
      strcat( strall, "    lines " );
      str1   = itoa( (int)E->line_number, str1, 10 );
      strcat( strall, str1 );
      strcat( strall, "     " );

      DISPXYA( 1, 1, 5, strall );

      if(insert)
      {
        str1 = itoa( (int)E->active, str1, 10 );
        strcpy( strall, "  INS ON   " );
        strcat( strall, str1 );
        DISPXYA( 30, 1, 5, strall );
      }
      else
      {
        str1 = itoa( (int)E->active, str1, 10 );
        strcpy( strall, "  INS OFF   " );
        strcat( strall, str1 );
        DISPXYA( 30, 1, 5, strall );
      }
      str1 = itoa( (int)strlen( E->begin ) , str1, 10 );
      strcpy( strall, "  Length=" );
      strcat( strall, str1 );
      strcat( strall, "     " );
      DISPXYA( 60, 1, 5, strall );



      strncpy( pom, E->begin, 70 );
      pom[70] = ' ';
      pom[71] = '\x0';
      DISPXYA( 1, 21, 5, pom );

      str1 = ltoa( E->line_number , str1, 10 );
      strcpy( strall, "  li  " );
      strcat( strall, str1 );
      strcat( strall, "     " );
      DISPXYA( 48, 1, 5, strall );

      SETXY(E->cursor_col+E->left, E->cursor_row+E->top);
    }
  }


  void main()
  {
  int xg, yg, xd, yd, nE, p;
  char *aaa;
  int mode, page, row, col;



    VIDEMODE( &mode, &page, &row, &col );

    DRAWBOX( 00, 00, 79, 24, 7, 7, "ÚÄ¿³±³ÔÍ¾" );

    xg = 0;
    yg = 1;
    xd = 79;
    yd = 23;

    nE = HB_ED_NEW( 96, 8, 4096l );
    ED = ETab[ nE ];

    strcpy( name, "test.xxx" );
    p = open ( name, O_RDONLY+O_BINARY );
    adr = calloc( 7000, 1 );
    read ( p, adr, 7000 );

    close ( p );

    HB_ED_SETTEXT( nE, adr );

//    HB_ED_INSTEXT( nE, adr, 0l );

    DRAWBOX( xg, yg, xd, yd, 14, 14, "ÚÄ¿³±³ÔÍ¾" );
    HB_ED_CONFIG( nE, yg+1, xg+1, yd-1, xd-1, 0, 0 );
    HB_ED_STABILI();

    Ed( ED );

    HB_ED_KILL(nE);

  }

#endif
