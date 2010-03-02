/*
 * $Id$
*/

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"

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

#define MAX_LINE_LEN   254


typedef struct
{
   int      top;              /* topmost row of editor's window */
   int      left;             /* leftmost column of the editor's window */
   int      bottom;           /* bottom row position */
   int      right;            /* rightmost column */
   int      line_length;      /* maximal line length */
   long int line_number;      /* the number of lines stored in text buffer */
   long int current_line;     /* the offset in memory buffer where the current line starts (where the cursor is positioned) */
   long int first_line;       /* offset of the first line (usually 0) */
   long int last_line;        /* the offset in memory buffer of the last line */
   int      cursor_row;       /* current cursor row in the window */
   int      cursor_col;       /* current cursor column in the window */
   long int first_display;    /* the offset of first visible (displayed) line */
   long int last_display;     /* the offset of last visible line */
   int      first_col;        /* first visible column */
   int      stable;           /* is the editor stabilized? */
   int      current_stabil;   /* currently displayed row (during stabilisation) */
   int      stabil;           /* number of rows to stabilize */
   int      space;            /* should spaces at the end of line be removed */
   char     escape;           /* ASCII code of color escaspe character (the next character after this will be used as color index */
   long int next_stabil;      /* the offset in memory buffer of next line to display */
   int      dir;              /* the direction of line stabilization */
   int      tab_size;         /* the number of spaces the replaces TAB character */
   long int active;           /* the line number where the cursor is positioned */
   int      IsConfigured;
   long int next_line;        /* the offset of next line to return by ED_GetNextLine() */
   long int text_length;      /* the size (in bytes) of edited text */
   long int buffer_size;      /* the size of allocated memory buffer */
   char *   begin;           /* the memory buffer */

} EDITOR;




static EDITOR * s_EStack[] = { NULL, NULL, NULL, NULL, NULL };
/* table of created editors */
static EDITOR * s_ETab[]   = { NULL, NULL, NULL, NULL, NULL,
                               NULL, NULL, NULL, NULL, NULL, NULL };
static EDITOR * s_ED;     /* currently serviced editor */





static unsigned int Clear( EDITOR * ED, long int, unsigned int * );
static void         BackSpace( int );
static void         NextWord( void );
static void         Return( int );
static void         GoTo( int );
static int          format_line( EDITOR * ED, int, unsigned int );
static void         MoveText( EDITOR * ED, long int, long int, long int );
static unsigned int GetLineLength( EDITOR * ED, long int, int * );

/*
 *
 **
 *
*/


/* Find the beginning of previous line starting from given offset
 */
static long int Prev( EDITOR * E, long int adres )
{
   long int i;

   if( adres > 0 )
   {
      for( i = adres; i >= 0; i-- )
      {
         if( E->begin[ ( unsigned int ) i ] == '\n' )
         {
            if( i < adres - 2 )
               return i + 1;
         }
      }
      return 0;
   }

   return -1;
}

/* Find the beginning of next line starting from given offset
 */
static long int Next( EDITOR * E, long int adres )
{
   char * tmp;

   tmp = strchr( E->begin + ( unsigned int ) adres, '\n' );

   if( tmp && tmp[ 1 ] )
      return ( long int ) ( ++tmp - E->begin );
   else
      return -1;
}

/* Initializes EDITOR structure
 */
static void New( EDITOR * E, int tab, int ll, long int bufferSize )
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
  E->stable         = HB_FALSE;
  E->tab_size       = tab;
  E->active         = 1;
  E->line_number    = 0;
  E->IsConfigured   = 0;
  E->text_length    = 0;
  E->buffer_size    = bufferSize;

  E->begin[ 0 ]    = '\r';
  E->begin[ 1 ]    = '\n';
  E->begin[ 2 ]    = '\x0';

}



/* Creates new editor and returns index into internal editors table */
HB_FUNC( ED_NEW )
{
   EDITOR * E;
   int nEdit=0;

   int      ll, tab;
   long int bufferSize;

   /* Find the free slot fpr new editor
    */
   while( ( nEdit < 10 ) && ( s_ETab[ nEdit ] != NULL ) )
      nEdit++;
   if( nEdit == 10 )
     hb_retni( -1 );    /* no more free slots */

   ll  = hb_parni( 1 );
   if( ll > MAX_LINE_LEN )
      ll = MAX_LINE_LEN;
   tab = hb_parni( 2 );
   E   = ( EDITOR * ) hb_xgrab( sizeof( EDITOR ) );

   s_ETab[ nEdit ] = E;

   if( E )
   {
      if( hb_parinfo( 0 ) < 3 )
         bufferSize = 32767;
      else
         bufferSize = ( long int ) hb_parni( 3 ) + 10;

      if( hb_parinfo( 0 ) < 4 )
         E->escape =0;
      else
         E->escape =( char ) hb_parni( 4 );

      E->begin = ( char * ) hb_xgrab( ( unsigned int ) bufferSize + 100 );
      memset( E->begin, '\x0', ( unsigned int ) bufferSize );

      New( E, tab, ll, bufferSize );

      hb_retni( nEdit );
   }
   else
      hb_retni( -1 );    /* failure */
}

/* Replaces TAB with spaces and removes spaces from the end of line
 */
static void FormatText ( EDITOR * E )
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
   wsk = E->begin + E->last_line;
   while( ( wsk = strchr( wsk, '\t' ) ) != 0 )
   {
      j = wsk - E->begin;

      MoveText( E, j, j + E->tab_size - 1,
                     ( long int ) ( E->buffer_size - j - E->tab_size + 1 ) );

      for( i = 0; i < E->tab_size; i++, wsk++ )
         *wsk = ' ';
   }

   /* TODO: optimize this line formating - format line only if
    * it will be displayed
    */
   while( E->current_line >= 0 )
   {
      E->last_line    = E->current_line;
      E->line_number++;

      nLen = Clear( E, E->current_line, &nEsc );

      if( ! format_line( E, HARD, nLen ) )
         E->current_line = Next( E, E->current_line );
   }

   E->current_line = dl;
   E->first_col = 0;
}

/* Resets the editor state after pasting new content of text buffer
 */
static void NewText( EDITOR * E )
{
   unsigned int dl;
   int i;

   /* text in buffer have to end with CR/LF
    */
   dl = ( unsigned int ) E->text_length;
   if( E->begin[ dl - 1 ] != '\n' )
   {
      E->begin[ dl     ] = '\r';
      E->begin[ dl + 1 ] = '\n';
      E->begin[ dl + 2 ] = '\x0';
      E->text_length += 2;
   }

   FormatText( E );

   E->cursor_col     = 0;
   E->cursor_row     = 0;
   E->stable         = HB_FALSE;
   E->current_stabil = 0;
   E->first_display  = E->last_display = 0;
   E->next_stabil    = 0;
   E->dir            = DOWN;
   E->stabil         = E->bottom - E->top + 1;

   for( i = 0; i < E->stabil; i++ )
      E->last_display = Next( E, E->last_display );
}

/* Appends passed text to the existing text buffer
 */
static void AddText( int nEdit, const char * adres )
{
   EDITOR * E;
   long int dl, dlold;

   E = s_ETab[ nEdit ];

   dl    = strlen( adres );
   dlold = E->text_length;
   if( dlold == 2 )
      dlold =0;   /* if current text buffer contains CRLF only then discard it */

   /* TODO: add reallocation of text buffer
    */
   if( ( dl + dlold ) <= ( E->buffer_size - 10 ) )
   {
      /* there is enough room in text buffer
       */
      strcpy( E->begin + dlold, adres );
      E->text_length += dl;
   }
   else
   {
      strncpy( E->begin + dlold, adres, ( int ) ( E->buffer_size - 10 - dlold ) );
      E->text_length = E->buffer_size - 10;
   }

   NewText( E );     /* reformat text */
}

/* Appends passed text at the end of existing one */
HB_FUNC( ED_ADDTEXT )
{
   const char * adres;
   int nEdit;

   nEdit = hb_parni( 1 );
   adres = hb_parc( 2 );

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
   memmove( E->begin + ( unsigned int ) dest, E->begin + ( unsigned int ) source,
            ( unsigned int ) ilb );

   if( E->last_display > E->current_line )
      E->last_display += diff;

   if( E->current_line < E->last_line )
      E->last_line += diff;

   E->text_length += diff;

   if( E->text_length > ( E->buffer_size - 8 ) )
   {
      E->text_length = E->buffer_size - 8;
      E->begin[ E->text_length ]      = '\x0';
      E->begin[ E->text_length - 1 ]  = '\n';
      E->begin[ E->text_length - 2 ]  = '\r';
   }
}

/* Skips to the beginning of given line
 */
static long int GoToLine( EDITOR * E, int linia )
{
   char * p;
   long int  i;

   i = 0;
   p = E->begin;
   while( ( ++i <= linia ) && ( p = strchr( p, '\n' ) ) != 0 )
      p += 2;

   if( i > linia )
      return p - E->begin - 1;
   else
      return E->text_length;  /* no such line number - go to the end */
}

/* Counts the number of printable characters in given line
 */
static unsigned int GetLineLength( EDITOR * E, long int off, int *wsk )
{
   unsigned int i, j;
   char * p;
   char * tmp;

   tmp = E->begin + ( unsigned int ) off;
   p   = strchr( tmp, '\n' );  /* find EOL starting from given position */

   if( p )
   {
      off = ( p - tmp );
      i = ( unsigned int ) off - 1;
   }
   else
      i = strlen( tmp );

   *wsk = 0;   /* number of characters used in color escape codes */
   if( E->escape )
   {
      for( j = 0; j < i; j++ )
      {
         if( ( char ) tmp[ j ] == E->escape )
         {
            ( *wsk ) += 2;
            j++;
         }
      }
   }

   return i - *wsk;  /* number of all chars minus number of escape chars */
}

/* Inserts text into existing text buffer starting from given line number
 */
static long int InsText( EDITOR * E, char * adres, long int line )
{
   long int  dl, off, il, dl1;
   int       addCRLF, cc;

   addCRLF = 0;
   dl  = strlen( adres );    /* length of text to insert */
   dl1 = E->text_length;      /* length of text that is currently in the buffer */

   /* TODO: add reallocation  of text buffer
    */
   if( dl1 < (E->buffer_size - 10) )
   {
      /* there is some free space in text buffer
       */
      /* Find the offset of given line */
      if( line > 0 )
         off = GoToLine( E, ( unsigned int ) line ); /* Find the offset of given line */
      else
         off = 0;

      if( ( long int )( dl + dl1 ) < ( E->buffer_size - 10 ) )
      {
         /* there is enough free room in text buffer
          */
         if( (adres[ ( unsigned int ) dl - 1 ] != '\n' ) && ( adres[ ( unsigned int ) dl - 2 ] != '\r' ) )
         {
            /* There is no CRLF at the end of inserted text -
             * we have to add CRLF to separate it from existing text
             */
            addCRLF = 1;
            dl += 2;
         }
         MoveText( E, off, off + dl, E->buffer_size - ( off - 1 ) - dl );
         strncpy( E->begin + ( unsigned int ) off, adres , ( unsigned int ) dl );
      }
      else
      {
         /* not enough free space
          * text at the end of existing text buffer will be lost
          */
         dl = E->buffer_size - 10 - dl1;
         if( adres[ ( unsigned int ) dl - 1 ] == '\r' )
            adres[ ( unsigned int ) dl - 1 ] = ' ';

         if( ( adres[ ( unsigned int ) dl - 1 ] != '\n' ) && ( adres[ ( unsigned int ) dl - 2 ] != '\r' ) )
         {
            addCRLF = 1;
            dl += 2;
         }
         MoveText( E, off, off + dl, E->buffer_size - ( off - 1 ) - dl );
         strncpy( E->begin + ( unsigned int ) off, adres, ( unsigned int ) dl );
      }

      if( addCRLF )
      {
         E->begin[ ( unsigned int ) ( off + dl - 2 ) ] = '\r';
         E->begin[ ( unsigned int ) ( off + dl - 1 ) ] = '\n';
         E->text_length += 2;
      }

      if( ( off + dl ) == E->text_length )
         E->begin[ ( unsigned int ) E->text_length ] = '\x0';
      E->text_length = strlen( E->begin );

      il = E->line_number;
      cc = E->cursor_col;

      FormatText ( E );

      E->cursor_col = cc;

      if( off <= E->current_line )
      {
         E->current_line += E->text_length - dl1;
         E->active       += E->line_number - il;
      }
   }

   return dl;
}

/* Inserts passed text into text buffer */
HB_FUNC( ED_INSTEXT )
{
   long int dl;
   EDITOR * E;

   char * adres;
   int nEdit;
   long int linia;

   nEdit = hb_parni( 1 );
   adres = hb_strdup( hb_parc( 2 ) );
   linia = ( long int ) hb_parni( 3 );

   E = s_ETab[ nEdit ];
   dl = InsText( E, adres, linia );
   E->last_line = Prev( E, ( long int ) strlen( E->begin ) );

   hb_retni( ( unsigned int ) dl );

   hb_xfree( adres );
}

/*
 **
*/
HB_FUNC( ED_PUSH )
{
   int i;

   i = 0;
   while( i < 5 && s_EStack[ i ] )
      ++i;

   s_EStack[ i ] = s_ED;
}

HB_FUNC( ED_POP )
{
   int i;

   i = 0;
   while( i < 5 && s_EStack[ i ] )
      ++i;

   if( i )
   {
      s_ED = s_EStack[ i - 1 ];
      s_EStack[ i - 1 ] = NULL;
   }
}

/*
 * Selects the editor as active - all next ED_*() calls will be send
 * to this editor.
*/
HB_FUNC( ED_CONFIG )
{
   int  szer, wys;
   int      nszer, nwys;
   int      diff;
   long int tmp;
   long int j;

   int top, left, bottom, right;
   int nRow, nCol;
   int nEdit, i;

   nEdit  = hb_parni( 1 );
   top    = hb_parni( 2 );
   left   = hb_parni( 3 );
   bottom = hb_parni( 4 );
   right  = hb_parni( 5 );
   nRow   = hb_parni( 6 );
   nCol   = hb_parni( 7 );

   s_ED = s_ETab[ nEdit ];  /* select the editor to work on */

   szer = s_ED->right - s_ED->left + 1;
   wys  = s_ED->bottom - s_ED->top + 1;

   s_ED->top        = top;
   s_ED->left       = left;
   s_ED->bottom     = bottom;
   s_ED->right      = right;

   s_ED->last_display = s_ED->first_display;
   s_ED->stabil       = s_ED->bottom - s_ED->top + 1;

   if( s_ED->IsConfigured )
   {
      /* In event driven world the position and size of the editor window can
      * change between activations - recalculate some required values
      */
      s_ED->first_display  = s_ED->current_line;

      /* find the first line to display - try to keep visible the current line
       * and display this line in the same row in the window (if possible)
       */
      for( i = 0; i < s_ED->cursor_row; i++ )
      {
         j = Prev( s_ED, s_ED->first_display );
         if( j >= 0 )
            s_ED->first_display = j;
         else
            s_ED->cursor_row--;
         if( s_ED->cursor_row < 0 )
            s_ED->cursor_row = 0;
      }

      /* find the last line for display */
      for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
      {
         j = Next( s_ED, s_ED->last_display );
         if( j >= 0 )
            s_ED->last_display  = j;
      }
   }
   else
   {
      s_ED->first_display  = s_ED->first_line;

      /* find the last line for display */
      nwys = s_ED->bottom - s_ED->top;
      for( i = 0; i < nwys; i++ )
      {
         j = Next( s_ED, s_ED->last_display );
         if( j >= 0 )
            s_ED->last_display  = j;
      }
      /* check if this line is empty */
      if( strlen( s_ED->begin + ( unsigned int ) s_ED->last_display ) == 0 )
         s_ED->last_display = Prev( s_ED, s_ED->last_display );

      /* set initial cursor position in the window */
      s_ED->cursor_row = nRow;
      s_ED->cursor_col = nCol;
   }

   if( s_ED->IsConfigured )
   {
      nszer = s_ED->right - s_ED->left + 1;
      nwys  = s_ED->bottom - s_ED->top + 1;

      diff = abs( szer - nszer );
      if( szer < nszer )
      {
         /* current width of the window is greater then during previous activation
          * adjust the first visible column
          */
         if( s_ED->first_col > diff )
         {
            s_ED->first_col -= diff;
            s_ED->cursor_col += diff;
         }
         else
         {
            s_ED->cursor_col += s_ED->first_col;
            s_ED->first_col = 0;
         }
      }
      if( szer > nszer )
      { /* current width of the window is smaller then during previous activation
         */
         if( s_ED->cursor_col > ( nszer - 1 ) )
         {
            s_ED->first_col += s_ED->cursor_col - nszer + 1;
            s_ED->cursor_col = nszer - 1;
         }
      }

      diff = abs( nwys - wys );
      if( wys > nwys )
      {
         /* current height of the window is smaller then during previous activation
         */
         if( s_ED->cursor_row < nwys )
         {
            /* the old cursor row position is smaller then the window height
            */
            tmp = s_ED->last_display;
            for( i = 0; i < diff; i++ )
            {
               j = Prev( s_ED, tmp );
               if( j >= 0 )
                  tmp = j;

            }
            s_ED->last_display = tmp;
         }
         else
         {
            /* old cursor row position is greater then current window height
             *  display the line where the cursor is placed as the last visible
             *  line in the window
             */
            s_ED->last_display = s_ED->current_line;
            tmp = s_ED->last_display;

            for( i = 0; i < nwys - 1; i++ )
            {
               j = Prev( s_ED, tmp );
               if( j >= 0 )
                  tmp = j;
            }
            s_ED->first_display = tmp;
            s_ED->cursor_row = nwys - 1;
         }
      }
   }
   else
   {
      s_ED->current_line   = s_ED->first_line;
      s_ED->active         = 1;
   }

   s_ED->IsConfigured   = 1;
   s_ED->stable         = HB_FALSE;
   s_ED->current_stabil = 0;
   s_ED->next_stabil    = s_ED->first_display;
   s_ED->dir            = DOWN;
}

/* Returns current text buffer */
HB_FUNC( ED_GETTEXT )
{
   long int  dl;
   char * buffer, * help;
   EDITOR * E;

   int Case, mietka;
   int nEdit;

   nEdit  = hb_parni( 1 );
   Case   = hb_parni( 2 );
   mietka = hb_parni( 3 );

      E = s_ETab[ nEdit ];

      dl    = strlen( E->begin ) + 3;

      buffer = ( char * ) hb_xgrab( ( unsigned int ) dl );

      strcpy( buffer, E->begin );

      switch( Case )
      {
         case 0:
            buffer = strlwr( buffer );
            break;
         case 1:
            buffer = strupr( buffer );
            break;

         default:;
      }

      help = buffer;
      if( mietka != SOFT )
      {
         while( help !=NULL )
         {
            help = strstr( buffer, "\n" );   /* CHR(141)+CHR(10) */
            if( help )
               help[ 0 ] = '\r';
         }
      }

   hb_retc( buffer );
   hb_xfree( buffer );
}

/* Returns given line of text and positions caret at the beginning of next line */
HB_FUNC( ED_GETLINE )
{
   long int        l, j;
   long int        tmp;
   char *          buffer;
   EDITOR *        E;
   int             rdl, dl;
   long int        i;

   long int linia;
   int      nEdit;

   nEdit  = hb_parni( 1 );
   linia  = ( long int ) hb_parni( 2 );

   E = s_ETab[ nEdit ];

   l = 1;
   tmp = E->first_line;
   for( i = 1; i < linia; i++ )
   {
      j = Next( E, tmp );
      if( j >= 0 )
      {
         tmp = j;
         l++;
      }
   }

   if( l == linia )
   {
      dl = GetLineLength( E, tmp, &rdl );
      dl++;
      buffer = ( char * ) hb_xgrab( dl );

      strncpy( buffer, E->begin + ( unsigned int ) tmp, dl - 1 );
      buffer[ dl - 1 ] = '\x0';

      hb_retc_buffer( buffer );
   }
   else
      hb_retc_null();

   E->next_line = Next( E, tmp );
}

/* Returns current line pointed by caret position and advances it to the beginning of next line */
HB_FUNC( ED_GETNEXT )
{
   char * buffer;
   EDITOR * E;
   int rdl, dl;

   int nEdit;

   nEdit  = hb_parni( 1 );

   E = s_ETab[ nEdit ];
   if( E->next_line > 0 )
   {
      dl = GetLineLength( E, E->next_line, &rdl );
      dl++;
      buffer = ( char * ) hb_xgrab( dl );

      strncpy( buffer, E->begin + ( unsigned int ) E->next_line, dl-1 );
      buffer[ ( unsigned int ) ( dl-1 ) ] = '\x0';
      E->next_line = Next( E, E->next_line );

      hb_retc( buffer );
      hb_xfree( buffer );
   }
   else
      hb_ret();
}

/* Resets text buffer
 */
static void KillText( EDITOR * E )
{
   memset( E->begin, '\x0', ( unsigned int ) E->buffer_size );
   E->first_line = E->last_line = 0;
}

/* Stores new text into editor */
HB_FUNC( ED_SETTEXT )
{
   EDITOR * E;

   const char * adres;
   int nEdit;

   nEdit = hb_parni( 1 );
   adres = hb_parc( 2 );

   E = s_ETab[ nEdit ];

   KillText( E );

   New( E, E->tab_size, E->line_length, E->buffer_size );

   AddText( nEdit, adres );
}

/* Reads a text from the file */
HB_FUNC( ED_READTEXT )
{
   unsigned int nEdit, nFile, nSize, lSuccess = HB_FALSE;
   long int nSeek, nRead;
   EDITOR * E;
/* BOOL lConv; */

   nEdit = hb_parni( 1 );
   E = s_ETab[ nEdit ];

   KillText( E );
   New( E, E->tab_size, E->line_length, E->buffer_size );

   nFile = hb_parni( 2 );
   nSeek = hb_parnl( 3 );
   nSize = hb_parni( 4 );
/* lConv = hb_parl( 5 ); */

   nRead = hb_fsSeek( nFile, nSeek, FS_SET );
   if( nRead == nSeek )
   {
      if( nSize > ( unsigned int ) ( E->buffer_size - 10 ) )
         nSize = ( unsigned int ) E->buffer_size - 10;

      nSize = hb_fsRead( nFile, ( unsigned char * ) E->begin, nSize );
      E->begin[ nSize ] ='\x0';

      E->text_length =nSize;

/*
 * Characters with ASCII code 27 and 26 cannot be stored inside
 * a file - for this reason the replacement characters were used 181 and 198
 * and stored in a file on disk
 *
   if( lConv )
   {
      unsigned char * cPtr;

      cPtr = ( unsigned char * ) E->begin;
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

      lSuccess = HB_TRUE;
      E->stable = HB_FALSE;
   }

   hb_retl( lSuccess );
}

/* Releases memory occupied by the editor */
HB_FUNC( ED_KILL )
{
   EDITOR * E;

   int nEdit;

   nEdit = hb_parni( 1 );

   E = s_ETab[ nEdit ];
   KillText( E );

   hb_xfree ( E->begin );

   hb_xfree( E );

   s_ETab[nEdit] = NULL;
}

/* Sorry - I don't remember why it is here
 */
HB_FUNC( ED_UNLOCK )
{
}

/*
 * Stabilize the editor
 * Redisplays all requested lines
 * It is simmilar to TBrowse:forceStable()
 * Incremental stabilisation was too slow
*/
HB_FUNC( ED_STABILIZE )
{
   unsigned int nRow = 0, nEscLen, nLen, width, i, j, e;
   int nLeft, nTop;
   char * EscPtr;
   char * cPtr;
   char adres[ MAX_LINE_LEN + 2 ];

   while( --s_ED->stabil >= 0 )
   {
      /* there are some lines of text to display
      */
      width = s_ED->right - s_ED->left + 1;

      if( s_ED->next_stabil >= 0 )
      {
         cPtr = s_ED->begin + s_ED->next_stabil;
         for( nEscLen = nLen = 0; *cPtr && *cPtr != '\n'; cPtr++ )
         {
            /* copy the line into temporary buffer - count characters used
            * as color escape codes
            */
            adres[ nLen++ ] =*cPtr;
            if( s_ED->escape && ( char ) *cPtr == s_ED->escape )
               nEscLen += 2;
         }
         j = nLen - nEscLen;    /* length of printable text */
         adres[nLen] = '\x0';

         if( s_ED->first_col >= j )
            adres[ nEscLen = nLen = 0 ] = '\x0'; /* first visible column is greater then line length */

         else if( s_ED->first_col )
         {
            /* text is scrolled right - we need to find the first
             * color escape code that will be visible
             * text ~2 in bold ~1 in normal
             *          ^-first visible column
             */
            e = 0;
            if( s_ED->escape )
            {
               for( i = 0; i < ( s_ED->first_col + e ); i++ )
               {
                  if( ( char ) adres[ i ] == s_ED->escape )
                  {
                     adres[ 0 ] = adres[ i ];
                     adres[ 1 ] = adres[ ++i ];
                     e +=2;
                  }
               }
            }

            if( e )
            {
               nLen -= ( i - 2 );
               if( ( char ) adres[ i - 1 ] == s_ED->escape )
                  i++, nLen--;
               strncpy( adres + 2, adres + i, nLen - 2 );
               nEscLen -= ( e - 2 );
            }
            else
            {
               nLen -= s_ED->first_col;
               strncpy( adres, adres+s_ED->first_col, nLen );
            }
            adres[ nLen ] = '\x0';
         }

         if( nLen )
         {
            if( adres[ nLen-1 ] & '\xd' ) /* soft or hard carriage */
               adres[ --nLen ] = '\x0';
         }

         /* find next line for displaying */
         switch( s_ED->dir )
         {
         case DOWN : s_ED->next_stabil = Next( s_ED, s_ED->next_stabil );
                     nRow = s_ED->current_stabil++;
                     break;

         case UP   : s_ED->next_stabil = Prev( s_ED, s_ED->next_stabil );
                     nRow = s_ED->current_stabil--;
                     break;
         }

         hb_gtColorSelect( 0 );    /* select default color */

         nTop = s_ED->top + nRow;
         if( nLen )
         {
            if( s_ED->escape && ( EscPtr = strchr( adres, s_ED->escape ) ) != 0 )
            {
               i = ( unsigned int )( EscPtr - adres );
               nLeft = s_ED->left + i;

               if( i )
                  hb_gtWriteAt( nTop, s_ED->left, adres, ( width < i ) ? width : i );

               for( ; i < nLen && nLeft <= s_ED->right; i++ )
               {
                  if( ( char )adres[ i ] == s_ED->escape )
                     hb_gtColorSelect( ( adres[ ++i ] & '\x0F' ) - 1 );
                  else
                     hb_gtWriteAt( nTop, nLeft++, adres + i, 1 );
               }
            }
            else
               hb_gtWriteAt( nTop, s_ED->left, adres, ( ( width < nLen ) ? width : nLen ) );
         }

         /* fill the rest of the row with spaces */
         if( ( nLen - nEscLen ) < width )
            hb_gtRepChar( nTop, s_ED->left + nLen - nEscLen, ' ', width - nLen + nEscLen );
      }
      else
      {
         /* no more lines of text to display - fill the window with spaces
         */
         switch( s_ED->dir )
         {
         case DOWN:
            nRow = s_ED->current_stabil++;
            break;
         case UP:
            nRow = s_ED->current_stabil--;
            break;
         }

         hb_gtColorSelect( 0 );
         hb_gtRepChar( s_ED->top + nRow, s_ED->left, ' ', width );
      }

      hb_gtColorSelect( 0 );
   }

   s_ED->stable = HB_TRUE;

   hb_retni( nRow );
}

/* Removes trailing spaces from the end of line
 */
static unsigned int Clear( EDITOR * E, long int e, unsigned int *nEsc )
{
   unsigned int nLen, i;

   nLen = GetLineLength( E, e, ( int * ) nEsc );
   i = ( unsigned int ) e + nLen + *nEsc;

   if( i )
   {
      while( E->begin[ i - 1 ] == ' ' )
      {
         if( E->cursor_col > 0 )
            E->cursor_col--;

         else if( E->first_col > 0 )
            E->first_col--;

         MoveText( E, ( long int ) i, ( long int ) i - 1, E->text_length - i + 3 );
         nLen--;
      }
   }

   return nLen;
}

/* Moves the cursor to the next line of text
 */
static void Down( EDITOR * E )
{
   long int j;
   unsigned int nEsc;

   j = Next( E, E->current_line );  /* find the offset of next line */
   if( E->begin[ ( unsigned int ) j ] == '\x0' )
      j = -1;     /* no more lines */
   if( j < 0 )
   {
      E->stable = HB_TRUE;
   }
   else
   {
      E->active++;
      Clear( E, E->current_line, &nEsc );

      j = Next( E, E->current_line );
      if( j >= 0 )
      {
         E->current_line = j;
      }
      if( ((++E->cursor_row)+E->top) > E->bottom )
      {
         /* attempt to move to the line that was not visible yet
          */
         E->stabil      = 1;  /* only one line needs to be redisplayed */

         E->cursor_row     = E->bottom - E->top;
         E->first_display  = Next( E, E->first_display );
         E->last_display   = j;

         E->stable         = HB_FALSE;
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




/* Moves cursor to the next line of text */
HB_FUNC( ED_DOWN )
{
   Down( s_ED );

   hb_retl(s_ED->stable);
}

/* Moves the cursor to the previous line of text
 */
static void Up( void )
{
   int j, i;
   long int jj, tmp;
   unsigned int nEsc;

   /* find the previous line */
   jj = Prev( s_ED, s_ED->current_line );
   if( jj < 0 )
   {
      s_ED->stable = HB_TRUE;
   }
   else
   {
      s_ED->active--;
      Clear( s_ED, s_ED->current_line, &nEsc );
      s_ED->current_line = jj;
      if( ((--s_ED->cursor_row)+s_ED->top) < s_ED->top )
      {
         /* the new line was not displayed yet */
         s_ED->stabil = 1;   /* only one line needs redisplay - rest of lines will be scrolled */

         j              = 0;
         s_ED->cursor_row = 0;

         /* count the number of lines that will be visible in the window.
          * If the number of lines is smaller then the window height then
          * the offset of last displayed line will be not changed
          */
         s_ED->first_display  = Prev( s_ED, s_ED->first_display );
         tmp                = s_ED->first_display;
         for( i = 0; i < s_ED->bottom - s_ED->top + 1; i++ )
         {
            jj = Next( s_ED, tmp );
            if( jj >= 0 )
            {
               tmp = jj;
               j++;
            }
         }
         if( j == (s_ED->bottom - s_ED->top + 1) )
            s_ED->last_display   = Prev( s_ED, s_ED->last_display );

         s_ED->stable         = HB_FALSE;
         s_ED->next_stabil    = s_ED->current_line;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
   }
}

/* Moves the cursor to the previous line */
HB_FUNC( ED_UP )
{
  Up();

   hb_retl(s_ED->stable);
}

/* Moves the cursor to the next page of text */
HB_FUNC( ED_PGDOWN )
{
   int i;
   long int j;

   j = Next( s_ED, s_ED->last_display );
   if( s_ED->begin[ ( unsigned int ) j ] == '\x0' )
   {  /* no more lines */
      s_ED->stable = HB_TRUE;
      /* advance the cursor as much as possible (to the last line) */
      for( i = 0; i < s_ED->bottom - s_ED->top + 1; i++ )
         Down( s_ED );
      return;
   }

   /* the last possible line will be displayed at the very bottom of the window */
   /* find the last line to display */
   for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
   {
      j = Next( s_ED, s_ED->last_display );
      if( j >= 0 )
      {
         if( s_ED->begin[ ( unsigned int )j ] != '\x0' )
         {
            s_ED->active++;
            s_ED->last_display = j;
         }
      }
      else
         break;   /* no more lines */
   }

   if( s_ED->begin[ ( unsigned int )s_ED->last_display ] == '\x0' )
      s_ED->last_display = Prev( s_ED, s_ED->last_display );
   s_ED->first_display = s_ED->last_display;

   /* find the first displayed line now */
   for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
   {
      j = Prev( s_ED, s_ED->first_display );
      if( j >= 0 )
         s_ED->first_display = j;
      else
         s_ED->first_display = 0;
   }

   /* find the offset of the line where the currsor will be displayed */
   s_ED->current_line = s_ED->last_display;
   for( i = 0; i < s_ED->bottom - s_ED->top - s_ED->cursor_row; i++ )
   {
      j = Prev( s_ED, s_ED->current_line );
      if( j >= 0 )
         s_ED->current_line = j;
      else
         s_ED->current_line = 0;
   }

   s_ED->stable         = HB_FALSE;
   s_ED->next_stabil    = s_ED->first_display;
   s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
   s_ED->dir            = DOWN;
   s_ED->current_stabil = 0;

}


/* Moves the cursor to the previous page of text */
HB_FUNC( ED_PGUP )
{
   int i, bt;
   long int j;

   bt = s_ED->bottom - s_ED->top;

   j = Prev( s_ED, s_ED->first_display );
   if( j < 0 )
   {  /* no more lines to move */
      s_ED->stable = HB_TRUE;
      /* advannce the cursor to the topmost line */
      for( i = 0; i < bt + 1; i++ )
        Up();
      return;
   }

   /* find the offset of the first visible line */
   for( i = 0; i < bt; i++ )
   {
      j = Prev( s_ED, s_ED->first_display );
      if( j >= 0 )
      {
         s_ED->active--;
         s_ED->first_display = j;
      }
      else
         break;   /* no more line */
   }
   /* now the last visible line */
   s_ED->last_display = s_ED->first_display;
   for( i = 0; i < bt; i++ )
   {
      j = Next( s_ED, s_ED->last_display );
      if( j >= 0 )
         s_ED->last_display  = j;
   }

   /* update the offset of line where the cursor will be displayed
    * keep the cursor in the same row if possible
    */
   s_ED->current_line = s_ED->last_display;
   for( i = 0; i < bt - s_ED->cursor_row; i++ )
   {
      j = Prev( s_ED, s_ED->current_line );
      if( j >= 0 )
         s_ED->current_line = j;
      else
         s_ED->current_line = 0;
   }

   s_ED->stable         = HB_FALSE;
   s_ED->next_stabil    = s_ED->last_display;
   s_ED->stabil         = bt + 1;
   s_ED->dir            = UP;
   s_ED->current_stabil = bt;
}

/* Move the cursor to the beginning of the text */
HB_FUNC( ED_TOP )
{
   long int j;
   int      i;
   unsigned int nEsc;

   Clear( s_ED, s_ED->current_line, &nEsc );
   s_ED->current_line   = s_ED->first_line;
   s_ED->cursor_row     = 0;
   s_ED->first_display  = s_ED->last_display = s_ED->first_line;

   /* find the last visible line */
   for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
   {
      j = Next( s_ED, s_ED->last_display );
      if( j >= 0 )
         s_ED->last_display = j;
   }

   s_ED->cursor_row     = s_ED->current_stabil = 0;
   s_ED->current_line   = s_ED->next_stabil = s_ED->first_line;
   s_ED->active         = 1;
   s_ED->stable         = HB_FALSE;
   s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
   s_ED->dir            = DOWN;
   s_ED->first_col      = s_ED->cursor_col = 0;
}

/* Move the cursor to the last line of text */
HB_FUNC( ED_BOTTOM )
{
   int      i, j;
   long int jj;
   unsigned int nEsc;

   j = 0;
   Clear( s_ED, s_ED->current_line, &nEsc );
   s_ED->current_line   = s_ED->last_line;
   s_ED->cursor_row     = 0;
   s_ED->first_display  = s_ED->last_line;

   /* find the first visible line */
   /* We have to count from the bottom to make it work in case the number
    * of lines is smaller then the height of the window
    */
   s_ED->last_display   = s_ED->first_display;
   for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
   {
      jj = Prev( s_ED, s_ED->first_display );
      if( jj >= 0 )
      {
         s_ED->first_display = jj;
         j++;
      }
   }

   s_ED->current_line   = s_ED->last_display;
   s_ED->active         = s_ED->line_number;
   s_ED->stable         = HB_FALSE;
   s_ED->next_stabil    = s_ED->first_display;
   s_ED->dir            = DOWN;
   s_ED->current_stabil = 0;
   s_ED->first_col      = s_ED->cursor_col = 0;
   s_ED->cursor_row     = j;
   s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
}

/* Go to the specified line number
 */
static void GoTo( int line )
{
   int          i;
   long int j;
   unsigned int nEsc;

   Clear( s_ED, s_ED->current_line, &nEsc );

   /* find specified line */
   s_ED->current_line = s_ED->first_line;
   for( i = 0; i < line-1; i++ )
   {
      j = Next( s_ED, s_ED->current_line );
      if( j >= 0 )
         s_ED->current_line = j;
   }
   s_ED->cursor_row     = 0;
   s_ED->first_display  = s_ED->current_line;

   /* find the offset of the last visible line */
   s_ED->last_display   = s_ED->first_display;
   for( i = 0; i < s_ED->bottom - s_ED->top; i++ )
   {
      j = Next( s_ED, s_ED->last_display );
      if( j >= 0 )
         s_ED->last_display = j;
   }

   s_ED->active          = line;
   s_ED->stable          = HB_FALSE;
   s_ED->next_stabil     = s_ED->current_line;
   s_ED->stabil          = s_ED->bottom - s_ED->top + 1;
   s_ED->dir             = DOWN;
   s_ED->current_stabil  = s_ED->cursor_row;
   s_ED->first_col       = 0;
}

/* Move the cursor to the given line using line number */
HB_FUNC( ED_GOTO )
{
   long int line;

   line = ( long int ) hb_parni( 1 );

   GoTo( ( unsigned int ) line );
}

/* Move the cursor to the previous character
 */
static void Left(void)
{
   if( s_ED->cursor_col > 0 )
      s_ED->cursor_col--;    /* inside the window - decrement current column number */
   else
   {
      if( s_ED->first_col > 0 )
      {
         /* the text is scrolled right - scroll it back to the left by one column */
         s_ED->first_col--;
         s_ED->stable         = HB_FALSE;
         s_ED->next_stabil    = s_ED->first_display;
         s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
      /* else no wrap allowed */
   }
}

/* Move the cursor to the previous character */
HB_FUNC( ED_LEFT )
{
   Left();
}


/* Move the cursor to the next character
 */
static void Right( EDITOR * E )
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

      E->stable         = HB_FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
}

/* Move the cursor to the next character */
HB_FUNC( ED_RIGHT )
{
   Right( s_ED );
}

/* Move the cursor to the beginning of line
 */
static void Home( EDITOR * E )
{
   if( E->first_col > 0 )
   {
      /* the line was scrolled to the right */
      E->cursor_col     = 0;
      E->first_col      = 0;
      E->stable         = HB_FALSE;
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


/* Move the cursor to the beginning of the line */
HB_FUNC( ED_HOME )
{
   Home( s_ED );
}

/* Move the cursor to the end of line
 */
static void End( EDITOR * E )
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
      E->stable         = HB_FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
   else
      if( ( ll - E->first_col ) > ( E->right - E->left ) )
      {
         /* scroll text to the right  */
         E->cursor_col     = E->right - E->left;
         E->first_col      = ( int ) ( ll - ( E->right - E->left ) );
         E->stable         = HB_FALSE;
         E->next_stabil    = E->first_display;
         E->stabil         = E->bottom - E->top + 1;
         E->dir            = DOWN;
         E->current_stabil = 0;
      }
      else
         E->cursor_col = ll - E->first_col;
}


/* Move the cursor the the end of line (after the last non-space character) */
HB_FUNC( ED_END )
{
   End( s_ED );
}


/* Format the current paragraph
 */
static void FormatParagraph ( EDITOR * E )
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

/*  if( rdl )
*/
   {
      dl = ( long int ) GetLineLength( E, E->current_line, &rdl );
      strncpy( pom, E->begin+( unsigned int )E->current_line, ( int ) ( dl + rdl + 10 ) );
      pom[ E->line_length + rdl + 1 ] = '\x0';
      tmp = strchr( pom, '\n' );
      if( tmp && ( ( unsigned char ) *( tmp - 1 ) == 141u ) )  /* soft CR */
      {
         tmp--;
         cor++;
      }
      else
         tmp = NULL;

      CrLine = E->current_line;

      while( tmp )
      {
         source   = E->current_line + ( long int )( tmp - pom - 1 );
         MoveText ( E, source + 2, source + 1, E->buffer_size - source + 2 );
         E->begin[ ( unsigned int ) ( source + 1 ) ] = ' ';

         rdl = format_line( E, SOFT, 0 );
         Clear( E, E->current_line, &nEsc );

         E->current_line = Next( E, E->current_line );
         dl  = ( long int ) GetLineLength( E, E->current_line, &rdl );
         strncpy( pom, E->begin + ( unsigned int ) E->current_line, ( int )( dl + rdl + 10 ) );
         pom[ E->line_length + rdl + 1 ] = '\x0';
         tmp    = strchr( pom, '\n' );
         if( tmp && ( ( unsigned char ) * ( tmp - 1 ) == 141u ) )
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

   E->stable      = HB_FALSE;
   E->dir         = DOWN;
   E->cursor_col  = cc;
   E->cursor_row  = cr;
   E->line_number-= cor;
}



/* Delete the character under the cursor
 */
static void DelChar( EDITOR * E )
{
   int ccc, rdl;
   long int cl;

   cl  = E->current_line;
   ccc = E->cursor_col + E->first_col;
   if( ccc <= GetLineLength( E, E->current_line, &rdl ) )
   {
      if( E->escape )
         while( ( char ) E->begin[ ( unsigned int ) ( E->current_line + ccc ) ] == E->escape )
            ccc += 2;
      MoveText( E, E->current_line + ( long int ) ( ccc + 1 ),
                   E->current_line + ( long int ) ccc,
                   ( E->buffer_size - ( E->current_line + ( long int ) ( ccc + 1 ) ) ) );

      E->stable         = HB_FALSE;
      E->next_stabil    = E->current_line;

      E->dir            = DOWN;
      E->current_stabil = E->cursor_row;

      FormatParagraph( E );

      if( E->current_line == E->last_line )
         E->last_display = E->last_line;

      E->current_line = cl;

      E->stable         = HB_FALSE;
      E->next_stabil    = E->first_display;
      E->stabil         = E->bottom - E->top + 1;
      E->dir            = DOWN;
      E->current_stabil = 0;
   }
}

/* Delete the character at current cursor position */
HB_FUNC( ED_DELCHAR )
{
   long int j;
   int      rdl;

   if( ( ( unsigned int ) (s_ED->cursor_col + s_ED->first_col) ) >=
         ( ( unsigned int ) GetLineLength( s_ED, s_ED->current_line, &rdl ) ) )
   {
      /* The cursor is positioned after the last non-space character
       */
      j = Next( s_ED, s_ED->current_line );
      if( j >= 0 )
      {
         /* there are more lines below the cursor - join the lines
          */
         Down( s_ED ); /* goto the next line */
         Home( s_ED ); /* goto the beginning of line */
         BackSpace( 1 );  /* delete separating CR/LF */

         s_ED->stable         = HB_FALSE;
         s_ED->next_stabil    = s_ED->first_display;
         s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
   }
   else
   {
      /* The cursor is inside the line or at the last character
       */
      if( ( unsigned int ) ( s_ED->cursor_col + s_ED->first_col ) <
          ( unsigned int ) ( GetLineLength( s_ED, s_ED->current_line, &rdl ) ) )
         DelChar( s_ED );   /* inside a line */
      else
      { /* at the last character */
         j = Next( s_ED, s_ED->current_line );
         if( j >= 0 )  /* if it is not the last line then delete character unde the cursor */
            DelChar( s_ED );
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

   s_ED->stable         = HB_FALSE;
   s_ED->next_stabil    = s_ED->current_line;
   s_ED->stabil         = 1;
   s_ED->dir            = DOWN;
   s_ED->current_stabil = s_ED->cursor_row;

   if( INS )
   {
      if( ( ccc = s_ED->cursor_col+s_ED->first_col ) >
                  GetLineLength( s_ED, s_ED->current_line, &rdl ) )
         INS = 0; /* cursor is scrolled after the last character in the line - just move the cursor left */
   }

   if( INS )
   {
      /* in destructive mode
       */
      *tmp  ='\x0';
      *tmp1 ='\x0';
      if( ( ccc = s_ED->cursor_col + s_ED->first_col ) > 0 )
      {
         /* inside the line */
         MoveText( s_ED, s_ED->current_line + ccc, s_ED->current_line + ccc - 1,
                   ( s_ED->buffer_size - (s_ED->current_line + ( long int ) ccc + 1 ) ) );
         Left();
      }
      else
      {
         /* at the beginning of the line */
         if( s_ED->current_line != s_ED->first_line )
         {
            /* this is not the first line */
            nLen =GetLineLength( s_ED, s_ED->current_line, &rdl );

            if( s_ED->current_line == s_ED->last_line )
               if( nLen == 0 )
                  s_ED->last_line = Prev( s_ED, s_ED->last_line );

            /* copy the last line into temporary buffer */
            strncpy( tmp, s_ED->begin + ( unsigned int ) s_ED->current_line, nLen+rdl );
            tmp[ nLen+rdl ] = '\x0';

            /* find the first space in current line (the new line will
             * be wrapped eventually at this position) */
            if( ( w = strchr ( tmp, ' ') ) != 0 )
               ww = ( int ) ( w - tmp );
            else
               ww = nLen+rdl;

            /* go to the previous line */
            j = Prev( s_ED, s_ED->current_line );
            kk = GetLineLength( s_ED, j, &rdl );
            strncpy( tmp1, s_ED->begin + ( unsigned int ) j, ( unsigned int )( kk + rdl ) );
            tmp1[ ( unsigned int )( kk + rdl )  ] = '\x0';
            Up();
            End( s_ED );

            /* the lines can be joined
             * the sum of whole length of these lines is smaller then maximal allowed
             * or there is a space where the line can be wrapped
             */
            if( (ww + kk + rdl - 1) < ( s_ED->line_length ) )
            {
               kk = GetLineLength( s_ED, s_ED->current_line, &rdl );
               j  = s_ED->current_line + kk + rdl;
               /* remove separating CRLF characters */
               MoveText( s_ED, j + 2, j, s_ED->buffer_size - j - 2 );

               s_ED->line_number--;

               j = Next( s_ED, s_ED->last_display );
               if( j >= 0 )
                  s_ED->last_display = j;
               if( s_ED->begin[ ( unsigned int ) s_ED->last_display + 1 ] == '\x0' )
                  s_ED->last_display = Prev( s_ED, s_ED->last_display );

               /* split the new line if it is too long */
               format_line( s_ED, HARD, 0 );

               j = Next( s_ED, s_ED->current_line );
               if( j < 0 )
               {
                  s_ED->last_display    = Prev( s_ED, s_ED->last_display );
                  s_ED->last_line       = s_ED->current_line;
               }
               s_ED->stable         = HB_FALSE;
               s_ED->next_stabil    = s_ED->first_display;
               s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
               s_ED->dir            = DOWN;
               s_ED->current_stabil = 0;
            }
         }
      }
      FormatParagraph ( s_ED );

      s_ED->stable         = HB_FALSE;
      s_ED->next_stabil    = s_ED->first_display;
      s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
      s_ED->dir            = DOWN;
      s_ED->current_stabil = 0;
   }
   else
   {
      /* non-destructive mode - move cursor only */
      ccc = s_ED->cursor_col + s_ED->first_col;
      if( ccc > 0 )
      {
         Left();
      }
      else
      {
         if( s_ED->current_line != s_ED->first_line )
         {
            Up();
            End( s_ED );
         }
      }
   }
}


/* Delete a character on the left side of the cursor */
HB_FUNC( ED_BSPACE )
{
   int INS;

   INS = hb_parl( 1 );   /* get current INSERT state  */

   BackSpace( INS );
}


/* Move to the beginning of next non-empty line
 */
static void GotoNextNonEmptyLine( void )
{
   int  rdl;

   Down( s_ED );
   Home( s_ED );

   while( GetLineLength( s_ED, s_ED->current_line, &rdl ) == 0 )
   {
      Down( s_ED );
      Home( s_ED );
      if( Next( s_ED, s_ED->current_line ) < 0 )
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

   ccc = s_ED->cursor_col + s_ED->first_col;
   nLen =Clear( s_ED, s_ED->current_line, &nEsc );

   if( nLen < ccc )
      GotoNextNonEmptyLine();
   else
   {
      *tmp ='\x0';
      strncpy( tmp, s_ED->begin + ( unsigned int ) s_ED->current_line + ccc,
                     nLen - s_ED->cursor_col - s_ED->first_col );
      tmp[ nLen - s_ED->cursor_col - s_ED->first_col ] = '\x0';
      if( (adr = strchr ( tmp, ' ' ) ) == NULL )
      {
         GotoNextNonEmptyLine();
         if( !s_ED->stable )
         {
            s_ED->next_stabil    = s_ED->first_display;
            s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
            s_ED->dir            = DOWN;
            s_ED->current_stabil = 0;
         }
      }
      else
      {
         s_ED->cursor_col   = ( int ) ( adr - tmp + 1 + s_ED->cursor_col + s_ED->first_col);

         if( s_ED->cursor_col > ( s_ED->right - s_ED->left ) )
         {
            s_ED->first_col     += s_ED->cursor_col - ( s_ED->right - s_ED->left );
            s_ED->cursor_col     = s_ED->right - s_ED->left;
            s_ED->stable         = HB_FALSE;
            s_ED->next_stabil    = s_ED->first_display;
            s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
            s_ED->dir            = DOWN;
            s_ED->current_stabil = 0;
         }
      }
   }

   if( s_ED->begin[ ( unsigned int ) (s_ED->current_line +
                     s_ED->cursor_col + s_ED->first_col) ] == ' ')
      NextWord();
}


/* Move the cursor to the next word */
HB_FUNC( ED_NWORD )
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
   nLen =Clear ( s_ED, s_ED->current_line, &nEsc );
   if( nLen < ( s_ED->cursor_col + s_ED->first_col ) )
      End( s_ED );

   if( ( s_ED->first_col + s_ED->cursor_col ) == 0 )
   {
      Up();
      End( s_ED );
   }

   for( i = s_ED->first_col + s_ED->cursor_col - 2; i >= 0; i-- )
   {
      if( s_ED->begin[ ( unsigned int ) (s_ED->current_line + i) ] == ' ')
      {
         pom = i;
         break;
      }
      else
         pom = -1;
   }

   if(pom < 0)
   {
      Home( s_ED );
      while( GetLineLength( s_ED, s_ED->current_line, &rdl ) == 0 )
      {
         Up();
         Home( s_ED );
         if( Prev( s_ED, s_ED->current_line ) < 0 )
            break;
      }
      if(!s_ED->stable)
      {
         s_ED->next_stabil    = s_ED->first_display;
         s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
   }
   else
   {
      s_ED->cursor_col = pom + 1;
      if( s_ED->first_col > 0 )
      {
         s_ED->cursor_col-= s_ED->first_col;
      }
      if( s_ED->cursor_col < 0 )
      {
         s_ED->first_col      = s_ED->cursor_col - (s_ED->right - s_ED->left);
         s_ED->stable         = HB_FALSE;
         s_ED->next_stabil    = s_ED->first_display;
         s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
   }

   if( s_ED->begin[ ( unsigned int ) (s_ED->current_line + s_ED->cursor_col +
                          s_ED->first_col) ] == ' ')
      PreviousWord();
}


/* Move the cursor to the previous word */
HB_FUNC( ED_PWORD )
{
   PreviousWord();
}


/* Format given line - returns it the line has changed
 */
static int format_line( EDITOR * E, int Karetka, unsigned int LineDl )
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
      strncpy( pom, E->begin + ( unsigned int ) E->current_line,
               ( int ) ( E->line_length + 10 + rdl ) );
      pom[ ( unsigned int )(E->line_length + rdl) ] = '\x0';

      /* find the last space where the line can be splitted */
      p = strrchr( pom, ' ' );
      if( p )
      {
         /* use this position to split the line */
         podz = ( int ) ( p - pom + 1 );
         jj   = 1 - podz + E->cursor_col + E->first_col;
      }
      else
      {
         /* there is no space in the line - split it at the maximal line length */
         podz = E->line_length;
         jj   = 1;
      }

      j = ( long int ) ( E->current_line + podz );
      MoveText ( E, j, j + 2, E->buffer_size - j - 2 );

      /* replace with separators */
      E->begin[ ( unsigned int ) j + 0 ] = ( char ) Karetka;
      E->begin[ ( unsigned int ) j + 1 ] = '\n';
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

   return status;
}

/* Appends the character at the end of line
 */
static int AppendChar( EDITOR * E, int znak, int podz )
{
   int       diff, rdl, status;
   long int  iPos, nLen;
   long int  cl;
   int       ccol, fcol;
   char * cNew;

   nLen = GetLineLength( E, E->current_line, &rdl );
   iPos = E->current_line + nLen;
   diff = E->cursor_col + E->first_col - ( int ) nLen;

   /* the cursor is positioned 'diff' columns after the last character
    * in the line - fill the gap with spaces before appending the character
    */
   MoveText( E, iPos, iPos + diff + 1, E->buffer_size - 1 - iPos - diff );
   memset( E->begin + ( unsigned int ) ( E->current_line + nLen ), ' ', diff );

   /* move the CRLF characters after the appended character */
   cNew  = E->begin + iPos + diff;
   *cNew++ = ( char ) znak;
   if( *cNew != '\r' )
      *cNew = ( char ) podz; /* insert requested soft/hard carriage */
   *++cNew = '\n';

   /* the last line always have to end with the hard carriage return */
   E->begin[ ( unsigned int ) E->text_length - 2 ] = '\r';

   status = format_line( E, SOFT, 0 );

   cl     = E->current_line;
   ccol   = E->cursor_col;
   fcol   = E->first_col;

   FormatParagraph( E );

   E->current_line = cl;
   E->cursor_col   = ccol;
   E->first_col    = fcol;

   return status;
}


/* Checks if there is enough free room in the text buffer
 */
static int Check_length( int iRequested )
{
   if( ( s_ED->text_length + iRequested ) <= ( s_ED->buffer_size - 8 ) )
      return 1;
   else
      return 0;
}


/* Adjusts the offset of last line
 */
static void SetLastLine( EDITOR * E )
{
   if( E->current_line > E->last_line )
      E->last_line = E->current_line;
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
   cc = s_ED->cursor_col + s_ED->first_col;   /* currnt position in the line */
   if( INS )
   {
      /* INSERT is ON */
      if( Check_length( 1 ) )
      {
         /* TODO: add reallocation of text buffer
          */
         if( ( unsigned int ) cc < GetLineLength( s_ED, s_ED->current_line, &rdl ) )
         {
            /* the character will be inserted within the line - the cursor
             * is placed inside the line
             */
            i = s_ED->current_line + cc;
            MoveText( s_ED, i, i + 1, s_ED->buffer_size - s_ED->current_line - cc - 1 );
            s_ED->begin[ ( unsigned int ) i ] = ( char ) znak;

            jj = format_line( s_ED, SOFT, 0 );

            cl     = s_ED->current_line;
            ccol   = s_ED->cursor_col;
            fcol   = s_ED->first_col;

            FormatParagraph( s_ED );

            s_ED->current_line = cl;
            s_ED->cursor_col   = ccol;
            s_ED->first_col    = fcol;

            if( jj )
               SetLastLine( s_ED );
         }
         else  /* the cursor is located after the last character in the line */
            jj = AppendChar( s_ED, znak, SOFT );

         if( !jj )
            Right( s_ED );
         else
            SetLastLine( s_ED );
      }
   }
   else
   {
      if( ( long int ) cc < ( GetLineLength( s_ED, s_ED->current_line, &rdl ) ) )
      {
         s_ED->begin[ ( unsigned int ) ( s_ED->current_line + cc ) ] = ( char ) znak;
         jj = 0;
         Right( s_ED );
      }
      else
         if( Check_length( 1 ) )
         {
            jj = AppendChar( s_ED, znak, SOFT );
            if( ! jj )
               Right( s_ED );
            else
               SetLastLine( s_ED );
         }
   }

   if( ! jj )
      if( ( s_ED->cursor_col + s_ED->first_col ) > ( s_ED->right - s_ED->left ) )
         jj = 1;

   if(jj)
   {
      s_ED->stabil = s_ED->bottom - s_ED->top + 1;
      s_ED->next_stabil    = s_ED->first_display;
      s_ED->current_stabil = 0;
   }
   else
   {
      s_ED->stabil = 1;
      s_ED->next_stabil    = s_ED->current_line;
      s_ED->current_stabil = s_ED->cursor_row;
   }

   s_ED->stable = HB_FALSE;
   s_ED->dir    = DOWN;
}


/* Insert or replace the character into the text buffer */
HB_FUNC( ED_PUTCHAR )
{
   int INS, znak;

   znak = hb_parni( 1 );    /* character to paste */
   INS  = hb_parl( 2 );     /* current INSERT state */

   PutChar( INS, znak );
}


/*
static void Tab ( int INS )
{
   if( INS )
   {
      for( i = 0; i < s_ED->tab_size; i++ )
         PutChar( HB_TRUE, 32 );
   }
   else
   {
      for( i = 0; i < s_ED->tab_size; i++ )
         Right();
   }
}
*/

/*
 **
*/

/*
HB_FUNC( ED_TAB )
{
  int INS = hb_parl( 1 );

  Tab( INS );
}
*/



/* Delete the current line */
HB_FUNC( ED_DELLINE )
{
   long int tmp, j;

   if( s_ED->active < s_ED->line_number )
   {
      j = Next( s_ED, s_ED->last_display );
      if( j >= 0 )
         s_ED->last_display = j;

      tmp = Next( s_ED, s_ED->current_line );
      if( tmp < 0 )
         tmp = 0;

      s_ED->stabil = s_ED->bottom - s_ED->top + 1 - s_ED->cursor_row;
      s_ED->dir    = DOWN;

      MoveText( s_ED, tmp, s_ED->current_line, s_ED->buffer_size - s_ED->current_line - 2 );

      if( s_ED->line_number > 0 )
         s_ED->line_number--;

      s_ED->next_stabil    = s_ED->current_line;
      s_ED->current_stabil = s_ED->cursor_row;
      s_ED->stable         = HB_FALSE;

   }
   else
   {
      s_ED->begin[ ( unsigned int ) s_ED->current_line + 0 ] = '\r';
      s_ED->begin[ ( unsigned int ) s_ED->current_line + 1 ] = '\n';
      s_ED->begin[ ( unsigned int ) s_ED->current_line + 2 ] = '\x0';
      memset( s_ED->begin + ( unsigned int ) s_ED->current_line + 2, '\x0',
               ( unsigned int ) ( s_ED->buffer_size - strlen( s_ED->begin ) ) );

      s_ED->last_display   = s_ED->last_line;
      s_ED->stabil         = 1;
      s_ED->dir            = DOWN;
      s_ED->next_stabil    = s_ED->current_line;
      s_ED->current_stabil = s_ED->cursor_row;
      s_ED->stable         = HB_FALSE;
   }
   if( s_ED->text_length == 0 )
   {
      s_ED->begin[ 0 ] = '\r';
      s_ED->begin[ 1 ] = '\n';
      s_ED->begin[ 2 ] = '\x0';
   }
}

/* Delete the word on the right side of the cursor */
HB_FUNC( ED_DELWORD )
{
   long int pos1, pos2, j;
   int cc, fc, cr, rdl;
   long int fd, ld;
   long int l;

   j = s_ED->current_line + s_ED->cursor_col + s_ED->first_col;
   if( s_ED->begin[ ( unsigned int ) j ] != ' ' )
   {
      if( ( unsigned int ) ( s_ED->cursor_col + s_ED->first_col ) <
          ( unsigned int ) ( GetLineLength( s_ED, s_ED->current_line, &rdl ) ))
      {
         cc = s_ED->cursor_col;
         cr = s_ED->cursor_row;
         fc = s_ED->first_col;
         fd = s_ED->first_display;
         ld = s_ED->last_display;
         l  = s_ED->current_line;
         NextWord();
         pos2 = s_ED->cursor_col + s_ED->first_col;
         PreviousWord();
         pos1 = s_ED->cursor_col + s_ED->first_col;

         s_ED->current_line  = l;
         s_ED->cursor_col    = cc;
         s_ED->cursor_row    = cr;
         s_ED->first_col     = fc;
         s_ED->first_display = fd;
         s_ED->last_display  = ld;

         if( pos2 == 0 )
            pos2 = GetLineLength( s_ED, s_ED->current_line, &rdl );

         MoveText( s_ED, s_ED->current_line + pos2, s_ED->current_line + pos1,
                   s_ED->buffer_size - s_ED->current_line - pos2 );
         FormatParagraph ( s_ED );
         s_ED->stable         = HB_FALSE;
         s_ED->next_stabil    = s_ED->first_display;
         s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
         s_ED->dir            = DOWN;
         s_ED->current_stabil = 0;
      }
      else
      {
         if( ( GetLineLength( s_ED, s_ED->current_line, &rdl ) ) == 0 )
         {
            HB_FUNC_EXEC( ED_DELLINE )
            Home( s_ED );
         }
      }
   }
}


/* Insert the CRLF characters
 */
static void Return( int INS )
{
   long int ii, j;
   unsigned int nEsc, nLen;

   if( Check_length( 2 ) )
   {
      if( INS )
      {
         /* only if INSERT state is ON
          */
         nLen = Clear( s_ED, s_ED->current_line, &nEsc );

         s_ED->line_number++;

         j = Next( s_ED, s_ED->current_line );
         if( j < 0 )
         {
            s_ED->last_line = s_ED->text_length;
            s_ED->text_length += 2;

            s_ED->begin[ s_ED->text_length - 2 ] = '\r';
            s_ED->begin[ s_ED->text_length - 1 ] = '\n';
            s_ED->begin[ s_ED->text_length     ] = '\x0';
         }
         else
         {
            if( ( s_ED->first_col + s_ED->cursor_col ) > nLen + 1 )
               End( s_ED );
            ii = s_ED->current_line + ( long int ) s_ED->first_col +
                                      ( long int ) s_ED->cursor_col;
            MoveText ( s_ED, ii, ii + 2, s_ED->buffer_size - ii - 2 );

            s_ED->begin[ ( unsigned int ) ii + 0 ] = '\r';
            s_ED->begin[ ( unsigned int ) ii + 1 ] = '\n';
            if( s_ED->last_line == s_ED->current_line )
               s_ED->last_line = Next( s_ED, s_ED->current_line );
         }

         if( s_ED->cursor_row < ( s_ED->bottom - s_ED->top ) )
         {
            j = Prev( s_ED, s_ED->last_display );
            if( j > 0 )
               s_ED->last_display = j;

            s_ED->next_stabil    = s_ED->current_line;
            s_ED->stabil         = s_ED->bottom - s_ED->top + 1 - s_ED->cursor_row;
            s_ED->current_stabil = s_ED->cursor_row;
         }
         else
         {
            s_ED->next_stabil    = s_ED->first_display;
            s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
            s_ED->current_stabil = 0;
         }
      }
   }
   else
   {
      Clear( s_ED, s_ED->current_line, &nEsc );
      j = Next( s_ED, s_ED->current_line );
      if( j > s_ED->last_line )
      {
         if( Check_length( 2 ) )
         {
            s_ED->line_number++;
            s_ED->last_line = j;
            s_ED->begin[ ( unsigned int ) j + 0 ] = '\r';
            s_ED->begin[ ( unsigned int ) j + 1 ] = '\n';
            s_ED->begin[ ( unsigned int ) j + 2 ] = '\x0';
         }
      }
   }

   if( Check_length( 0 ) )
   {
      s_ED->stable         = HB_FALSE;
      s_ED->dir            = DOWN;
      Down( s_ED );
      Home( s_ED );
   }

   if( !s_ED->stable )
   {
      s_ED->next_stabil    = s_ED->first_display;
      s_ED->stabil         = s_ED->bottom - s_ED->top + 1;
      s_ED->current_stabil = 0;
      s_ED->dir            = DOWN;
   }
}


/* Insert the CRLF characters */
HB_FUNC( ED_RETURN )
{
   int INS = hb_parl( 1 );

   Return( INS );
}


/* Returns the current cursor row inside the editor's window */
HB_FUNC( ED_WINROW )
{
   hb_retni( s_ED->cursor_row );
}

/* Returns the line number where the cursor is positioned */
HB_FUNC( ED_ROW )
{
   hb_retni( ( unsigned int ) s_ED->active );
}


/* Return the current cursor column inside the editor's window */
HB_FUNC( ED_WINCOL )
{
   hb_retni( s_ED->cursor_col );
}

/* Returns the current cursor position inside the line */
HB_FUNC( ED_COL )
{
   hb_retni( s_ED->cursor_col + s_ED->first_col + 1 );
}


/* Returns the total number of lines */
HB_FUNC( ED_MAXLINE )
{
   hb_retni( ( unsigned int ) s_ED->line_number );
}

/* Counts the total number of lines in passed editor */
HB_FUNC( ED_LCOUNT )
{
   EDITOR * E = s_ETab[ hb_parni( 1 ) ];

   hb_retni( ( unsigned int ) E->line_number );
}


/* Returns if the editor is correctly displayed */
HB_FUNC( ED_STABLE )
{
   hb_retl( s_ED->stable );
}


/* Returns the number of bytes stored in the text buffer */
HB_FUNC( ED_LENGTH )
{
   hb_retni( ( unsigned int ) s_ED->text_length );
}
