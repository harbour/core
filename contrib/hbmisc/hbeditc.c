/*
 * Copyright 1999 Ryszard Glab
 * www - http://harbour-project.org
 */

/*
   Known bugs:
   ----------
   1) It requires files separated with CR/LF pairs
   2) NextWord() doesn't work correctly
   3) If text contains color escape codes then deleting or inserting
     of characters doesn't work correctly in the line that contains it
   4) Doesn't handle OS-specific EOL (only CRLG)
   5) Unicode support

   To fix:
   ------
   1) All TAB characters are replaced with spaces at startup - if edited file is
     very large and contains many TABs then it can take a vary long time - TAB
     characters should be left unchanged and interpreted during editing
   2) It reformats whole text at startup - again for a very long text it can
     take too much time
   3) Text buffer shold be reallocated dynamically
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapierr.h"

#define _STABILIZE_UP    1
#define _STABILIZE_DOWN  0

#define _MAX_LINE_LEN    4096


typedef struct
{
   int     top;               /* topmost row of editor's window */
   int     left;              /* leftmost column of the editor's window */
   int     bottom;            /* bottom row position */
   int     right;             /* rightmost column */
   HB_ISIZ line_length;       /* maximal line length */
   HB_ISIZ line_number;       /* the number of lines stored in text buffer */
   HB_ISIZ current_line;      /* the offset in memory buffer where the current line starts (where the cursor is positioned) */
   HB_ISIZ first_line;        /* offset of the first line (usually 0) */
   HB_ISIZ last_line;         /* the offset in memory buffer of the last line */
   int     cursor_row;        /* current cursor row in the window */
   HB_ISIZ cursor_col;        /* current cursor column in the window */
   HB_ISIZ first_display;     /* the offset of first visible (displayed) line */
   HB_ISIZ last_display;      /* the offset of last visible line */
   HB_ISIZ first_col;         /* first visible column */
   HB_BOOL fStable;           /* is the editor stabilized? */
   int     current_stabil;    /* currently displayed row (during stabilisation) */
   int     stabil;            /* number of rows to stabilize */
   char    escape;            /* ASCII code of color escaspe character (the next character after this will be used as color index */
   HB_ISIZ next_stabil;       /* the offset in memory buffer of next line to display */
   int     dir;               /* the direction of line stabilization */
   int     tab_size;          /* the number of spaces the replaces TAB character */
   HB_ISIZ active;            /* the line number where the cursor is positioned */
   HB_BOOL fIsConfigured;
   HB_ISIZ next_line;         /* the offset of next line to return by ED_GetNextLine() */
   HB_ISIZ text_length;       /* the size (in bytes) of edited text */
   HB_ISIZ buffer_size;       /* the size of allocated memory buffer */
   char *  begin;             /* the memory buffer */

} HB_EDITOR, * PHB_EDITOR;




static void KillText( PHB_EDITOR pEd );
static HB_ISIZ Clear( PHB_EDITOR pEd, HB_ISIZ e, HB_ISIZ * nEsc );
static void BackSpace( PHB_EDITOR pEd, HB_BOOL fInsert );
static void NextWord( PHB_EDITOR pEd );
static void Return( PHB_EDITOR pEd, HB_BOOL fInsert );
static void GoTo( PHB_EDITOR pEd, HB_ISIZ line );
static HB_BOOL format_line( PHB_EDITOR pEd, char Karetka, HB_ISIZ LineDl );
static void MoveText( PHB_EDITOR pEd, HB_ISIZ source, HB_ISIZ dest, HB_ISIZ ilb );
static HB_ISIZ GetLineLength( PHB_EDITOR pEd, HB_ISIZ off, HB_ISIZ * wsk );



static HB_GARBAGE_FUNC( PHB_EDITOR_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */

      KillText( ( PHB_EDITOR ) *ph );
      hb_xfree( ( ( PHB_EDITOR ) *ph )->begin );
      hb_xfree( ( PHB_EDITOR ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcPHB_EDITOR_funcs =
{
   PHB_EDITOR_release,
   hb_gcDummyMark
};

static void PHB_EDITOR_ret( PHB_EDITOR p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_EDITOR * ), &s_gcPHB_EDITOR_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

static PHB_EDITOR PHB_EDITOR_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcPHB_EDITOR_funcs, iParam );

   return ph ? ( PHB_EDITOR ) *ph : NULL;
}



/* Find the beginning of previous line starting from given offset
 */
static HB_ISIZ Prev( PHB_EDITOR pEd, HB_ISIZ adres )
{
   HB_ISIZ i;

   if( adres > 0 )
   {
      for( i = adres; i >= 0; i-- )
      {
         if( pEd->begin[ i ] == '\n' )
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
static HB_ISIZ Next( PHB_EDITOR pEd, HB_ISIZ adres )
{
   char * tmp;

   tmp = strchr( pEd->begin + adres, '\n' );

   if( tmp && tmp[ 1 ] )
      return ++tmp - pEd->begin;
   else
      return -1;
}

/* Initializes HB_EDITOR structure
 */
static void New( PHB_EDITOR pEd, int tab, HB_ISIZ ll, HB_ISIZ bufferSize )
{

   pEd->line_length    = ll;
   pEd->first_line     = 0;
   pEd->last_line      = 0;
   pEd->current_line   = 0;
   pEd->first_display  = 0;
   pEd->last_display   = 0;
   pEd->cursor_row     = 0;
   pEd->cursor_col     = 0;
   pEd->first_col      = 0;
   pEd->stabil         = 0;
   pEd->current_stabil = 0;
   pEd->fStable        = HB_FALSE;
   pEd->tab_size       = tab;
   pEd->active         = 1;
   pEd->line_number    = 0;
   pEd->fIsConfigured  = HB_FALSE;
   pEd->text_length    = 0;
   pEd->buffer_size    = bufferSize;

   pEd->begin[ 0 ] = '\r';
   pEd->begin[ 1 ] = '\n';
   pEd->begin[ 2 ] = '\0';

}



/* Creates new editor and returns index into internal editors table */
HB_FUNC( ED_NEW )
{
   PHB_EDITOR pEd = ( PHB_EDITOR ) hb_xgrab( sizeof( HB_EDITOR ) );

   HB_ISIZ ll;
   int     tab = hb_parni( 2 );
   HB_ISIZ bufferSize;

   ll = hb_parns( 1 );
   if( ll > _MAX_LINE_LEN )
      ll = _MAX_LINE_LEN;

   bufferSize = HB_ISNUM( 3 ) ? hb_parns( 3 ) + 10 : 32767;
   if( bufferSize <= 0 )
      bufferSize = 32767;

   pEd->escape = ( char ) hb_parni( 4 );
   pEd->begin  = ( char * ) hb_xgrab( bufferSize + 100 );
   memset( pEd->begin, '\0', bufferSize );

   New( pEd, tab, ll, bufferSize );

   PHB_EDITOR_ret( pEd );
}

/* Replaces TAB with spaces and removes spaces from the end of line
 */
static void FormatText( PHB_EDITOR pEd )
{
   HB_ISIZ j;
   HB_ISIZ dl;
   char *  wsk;
   int     i;
   HB_ISIZ nLen;
   HB_ISIZ nEsc;

   dl = pEd->current_line;
   pEd->current_line = pEd->last_line;

   /* TODO: remove this TAB replacement because it is time consuming
    * operation if a very large file is edited
    */
   wsk = pEd->begin + pEd->last_line;
   while( ( wsk = strchr( wsk, '\t' ) ) != 0 )
   {
      j = wsk - pEd->begin;

      MoveText( pEd, j, j + pEd->tab_size - 1,
                pEd->buffer_size - j - pEd->tab_size + 1 );

      for( i = 0; i < pEd->tab_size; i++, wsk++ )
         *wsk = ' ';
   }

   /* TODO: optimize this line formating - format line only if
    * it will be displayed
    */
   while( pEd->current_line >= 0 )
   {
      pEd->last_line = pEd->current_line;
      pEd->line_number++;

      nLen = Clear( pEd, pEd->current_line, &nEsc );

      if( ! format_line( pEd, HB_CHAR_HARD1, nLen ) )
         pEd->current_line = Next( pEd, pEd->current_line );
   }

   pEd->current_line = dl;
   pEd->first_col    = 0;
}

/* Resets the editor state after pasting new content of text buffer
 */
static void NewText( PHB_EDITOR pEd )
{
   HB_ISIZ dl;
   int     i;

   /* text in buffer have to end with CR/LF
    */
   dl = pEd->text_length;
   if( pEd->begin[ dl - 1 ] != '\n' )
   {
      pEd->begin[ dl ]     = '\r';
      pEd->begin[ dl + 1 ] = '\n';
      pEd->begin[ dl + 2 ] = '\0';
      pEd->text_length    += 2;
   }

   FormatText( pEd );

   pEd->cursor_col     = 0;
   pEd->cursor_row     = 0;
   pEd->fStable        = HB_FALSE;
   pEd->current_stabil = 0;
   pEd->first_display  = pEd->last_display = 0;
   pEd->next_stabil    = 0;
   pEd->dir    = _STABILIZE_DOWN;
   pEd->stabil = pEd->bottom - pEd->top + 1;

   for( i = 0; i < pEd->stabil; i++ )
      pEd->last_display = Next( pEd, pEd->last_display );
}

/* Appends passed text to the existing text buffer
 */
static void AddText( PHB_EDITOR pEd, const char * adres )
{
   HB_ISIZ dl, dlold;

   dl    = strlen( adres );
   dlold = pEd->text_length;
   if( dlold == 2 )
      dlold = 0;   /* if current text buffer contains CRLF only then discard it */

   /* TODO: add reallocation of text buffer
    */
   if( ( dl + dlold ) <= ( pEd->buffer_size - 10 ) )
   {
      /* there is enough room in text buffer
       */
      hb_strncpy( pEd->begin + dlold, adres, dl );
      pEd->text_length += dl;
   }
   else
   {
      hb_strncpy( pEd->begin + dlold, adres, pEd->buffer_size - 10 - dlold );
      pEd->text_length = pEd->buffer_size - 10;
   }

   NewText( pEd );     /* reformat text */
}

/* Appends passed text at the end of existing one */
HB_FUNC( ED_ADDTEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      AddText( pEd, hb_parcx( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Moves text from one location into another
 */
static void MoveText( PHB_EDITOR pEd, HB_ISIZ source, HB_ISIZ dest, HB_ISIZ ilb )
{
   HB_ISIZ diff;

   diff = dest - source;
   /* memmove supports overlapped buffers */
   memmove( pEd->begin + dest, pEd->begin + source, ilb );

   if( pEd->last_display > pEd->current_line )
      pEd->last_display += diff;

   if( pEd->current_line < pEd->last_line )
      pEd->last_line += diff;

   pEd->text_length += diff;

   if( pEd->text_length > ( pEd->buffer_size - 8 ) )
   {
      pEd->text_length = pEd->buffer_size - 8;
      pEd->begin[ pEd->text_length ]     = '\0';
      pEd->begin[ pEd->text_length - 1 ] = '\n';
      pEd->begin[ pEd->text_length - 2 ] = '\r';
   }
}

/* Skips to the beginning of given line
 */
static HB_ISIZ GoToLine( PHB_EDITOR pEd, HB_ISIZ linia )
{
   char *  p;
   HB_ISIZ i;

   i = 0;
   p = pEd->begin;
   while( ( ++i <= linia ) && ( p = strchr( p, '\n' ) ) != 0 )
      p += 2;

   if( i > linia )
      return p - pEd->begin - 1;
   else
      return pEd->text_length;  /* no such line number - go to the end */
}

/* Counts the number of printable characters in given line
 */
static HB_ISIZ GetLineLength( PHB_EDITOR pEd, HB_ISIZ off, HB_ISIZ * wsk )
{
   HB_ISIZ i, j;
   char *  p;
   char *  tmp;

   tmp = pEd->begin + off;
   p   = strchr( tmp, '\n' );  /* find EOL starting from given position */

   if( p )
   {
      off = ( p - tmp );
      i   = off - 1;
   }
   else
      i = strlen( tmp );

   *wsk = 0;   /* number of characters used in color escape codes */
   if( pEd->escape )
   {
      for( j = 0; j < i; j++ )
      {
         if( tmp[ j ] == pEd->escape )
         {
            *wsk += 2;
            j++;
         }
      }
   }

   return i - *wsk;  /* number of all chars minus number of escape chars */
}

/* Inserts text into existing text buffer starting from given line number
 */
static HB_ISIZ InsText( PHB_EDITOR pEd, char * adres, HB_ISIZ line )
{
   HB_ISIZ dl, off, il, dl1;
   HB_BOOL addCRLF;
   HB_ISIZ cc;

   addCRLF = HB_FALSE;
   dl      = strlen( adres );  /* length of text to insert */
   dl1     = pEd->text_length; /* length of text that is currently in the buffer */

   /* TODO: add reallocation  of text buffer
    */
   if( dl1 < ( pEd->buffer_size - 10 ) )
   {
      /* there is some free space in text buffer
       */
      /* Find the offset of given line */
      if( line > 0 )
         off = GoToLine( pEd, line );  /* Find the offset of given line */
      else
         off = 0;

      if( ( dl + dl1 ) < ( pEd->buffer_size - 10 ) )
      {
         /* there is enough free room in text buffer
          */
         if( adres[ dl - 1 ] != '\n' && adres[ dl - 2 ] != '\r' )
         {
            /* There is no CRLF at the end of inserted text -
             * we have to add CRLF to separate it from existing text
             */
            addCRLF = HB_TRUE;
            dl     += 2;
         }
         MoveText( pEd, off, off + dl, pEd->buffer_size - ( off - 1 ) - dl );
         hb_strncpy( pEd->begin + off, adres, dl );
      }
      else
      {
         /* not enough free space
          * text at the end of existing text buffer will be lost
          */
         dl = pEd->buffer_size - 10 - dl1;
         if( adres[ dl - 1 ] == '\r' )
            adres[ dl - 1 ] = ' ';

         if( adres[ dl - 1 ] != '\n' && adres[ dl - 2 ] != '\r' )
         {
            addCRLF = HB_TRUE;
            dl     += 2;
         }
         MoveText( pEd, off, off + dl, pEd->buffer_size - ( off - 1 ) - dl );
         hb_strncpy( pEd->begin + off, adres, dl );
      }

      if( addCRLF )
      {
         pEd->begin[ off + dl - 2 ] = '\r';
         pEd->begin[ off + dl - 1 ] = '\n';
         pEd->text_length += 2;
      }

      if( ( off + dl ) == pEd->text_length )
         pEd->begin[ pEd->text_length ] = '\0';
      pEd->text_length = strlen( pEd->begin );

      il = pEd->line_number;
      cc = pEd->cursor_col;

      FormatText( pEd );

      pEd->cursor_col = cc;

      if( off <= pEd->current_line )
      {
         pEd->current_line += pEd->text_length - dl1;
         pEd->active       += pEd->line_number - il;
      }
   }

   return dl;
}

/* Inserts passed text into text buffer */
HB_FUNC( ED_INSTEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ dl;

      char *  adres = hb_strdup( hb_parc( 2 ) );
      HB_ISIZ linia = hb_parns( 3 );

      dl = InsText( pEd, adres, linia );
      pEd->last_line = Prev( pEd, strlen( pEd->begin ) );

      hb_retns( dl );

      hb_xfree( adres );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Selects the editor as active - all next ED_*() calls will be send
 * to this editor.
 */
HB_FUNC( ED_CONFIG )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      int     szer, wys;
      int     nszer, nwys;
      int     diff;
      HB_ISIZ tmp;
      HB_ISIZ j;

      int top, left, bottom, right;
      int nRow, nCol;
      int i;

      top    = hb_parni( 2 );
      left   = hb_parni( 3 );
      bottom = hb_parni( 4 );
      right  = hb_parni( 5 );
      nRow   = hb_parni( 6 );
      nCol   = hb_parni( 7 );

      szer = pEd->right - pEd->left + 1;
      wys  = pEd->bottom - pEd->top + 1;

      pEd->top    = top;
      pEd->left   = left;
      pEd->bottom = bottom;
      pEd->right  = right;

      pEd->last_display = pEd->first_display;
      pEd->stabil       = pEd->bottom - pEd->top + 1;

      if( pEd->fIsConfigured )
      {
         /* In event driven world the position and size of the editor window can
          * change between activations - recalculate some required values
          */
         pEd->first_display = pEd->current_line;

         /* find the first line to display - try to keep visible the current line
          * and display this line in the same row in the window (if possible)
          */
         for( i = 0; i < pEd->cursor_row; i++ )
         {
            j = Prev( pEd, pEd->first_display );
            if( j >= 0 )
               pEd->first_display = j;
            else
               pEd->cursor_row--;
            if( pEd->cursor_row < 0 )
               pEd->cursor_row = 0;
         }

         /* find the last line for display */
         for( i = 0; i < pEd->bottom - pEd->top; i++ )
         {
            j = Next( pEd, pEd->last_display );
            if( j >= 0 )
               pEd->last_display = j;
         }
      }
      else
      {
         pEd->first_display = pEd->first_line;

         /* find the last line for display */
         nwys = pEd->bottom - pEd->top;
         for( i = 0; i < nwys; i++ )
         {
            j = Next( pEd, pEd->last_display );
            if( j >= 0 )
               pEd->last_display = j;
         }
         /* check if this line is empty */
         if( strlen( pEd->begin + pEd->last_display ) == 0 )
            pEd->last_display = Prev( pEd, pEd->last_display );

         /* set initial cursor position in the window */
         pEd->cursor_row = nRow;
         pEd->cursor_col = nCol;
      }

      if( pEd->fIsConfigured )
      {
         nszer = pEd->right - pEd->left + 1;
         nwys  = pEd->bottom - pEd->top + 1;

         diff = abs( szer - nszer );
         if( szer < nszer )
         {
            /* current width of the window is greater then during previous activation
             * adjust the first visible column
             */
            if( pEd->first_col > diff )
            {
               pEd->first_col  -= diff;
               pEd->cursor_col += diff;
            }
            else
            {
               pEd->cursor_col += pEd->first_col;
               pEd->first_col   = 0;
            }
         }
         if( szer > nszer ) /* current width of the window is smaller then during previous activation
                             */
         {
            if( pEd->cursor_col > ( nszer - 1 ) )
            {
               pEd->first_col += pEd->cursor_col - nszer + 1;
               pEd->cursor_col = nszer - 1;
            }
         }

         diff = abs( nwys - wys );
         if( wys > nwys )
         {
            /* current height of the window is smaller then during previous activation
             */
            if( pEd->cursor_row < nwys )
            {
               /* the old cursor row position is smaller then the window height
                */
               tmp = pEd->last_display;
               for( i = 0; i < diff; i++ )
               {
                  j = Prev( pEd, tmp );
                  if( j >= 0 )
                     tmp = j;

               }
               pEd->last_display = tmp;
            }
            else
            {
               /* old cursor row position is greater then current window height
                *  display the line where the cursor is placed as the last visible
                *  line in the window
                */
               pEd->last_display = pEd->current_line;
               tmp = pEd->last_display;

               for( i = 0; i < nwys - 1; i++ )
               {
                  j = Prev( pEd, tmp );
                  if( j >= 0 )
                     tmp = j;
               }
               pEd->first_display = tmp;
               pEd->cursor_row    = nwys - 1;
            }
         }
      }
      else
      {
         pEd->current_line = pEd->first_line;
         pEd->active       = 1;
      }

      pEd->fIsConfigured  = HB_TRUE;
      pEd->fStable        = HB_FALSE;
      pEd->current_stabil = 0;
      pEd->next_stabil    = pEd->first_display;
      pEd->dir = _STABILIZE_DOWN;
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Returns current text buffer */
HB_FUNC( ED_GETTEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ dl;
      char *  buffer;
      char *  help;

      char mietka = ( char ) hb_parni( 2 );

      dl = strlen( pEd->begin ) + 3;

      buffer = ( char * ) hb_xgrab( dl + 3 );

      hb_strncpy( buffer, pEd->begin, dl - 1 );

      help = buffer;
      if( mietka != HB_CHAR_SOFT1 )
      {
         while( help != NULL )
         {
            help = strstr( buffer, "\x8D\n" );   /* Chr( 141 ) + Chr( 10 ) */
            if( help )
               help[ 0 ] = '\r';
         }
      }

      hb_retc_buffer( buffer );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Returns given line of text and positions caret at the beginning of next line */
HB_FUNC( ED_GETLINE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ l, j;
      HB_ISIZ tmp;
      HB_ISIZ dl;
      HB_ISIZ rdl;
      HB_ISIZ i;

      HB_ISIZ linia = hb_parns( 2 );

      l   = 1;
      tmp = pEd->first_line;
      for( i = 1; i < linia; i++ )
      {
         j = Next( pEd, tmp );
         if( j >= 0 )
         {
            tmp = j;
            l++;
         }
      }

      if( l == linia )
      {
         dl = GetLineLength( pEd, tmp, &rdl );

         hb_retclen( pEd->begin + tmp, dl );
      }
      else
         hb_retc_null();

      pEd->next_line = Next( pEd, tmp );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Returns current line pointed by caret position and advances it to the beginning of next line */
HB_FUNC( ED_GETNEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ rdl;
      HB_ISIZ dl;

      if( pEd->next_line > 0 )
      {
         dl = GetLineLength( pEd, pEd->next_line, &rdl );

         hb_retclen( pEd->begin + pEd->next_line, dl );

         pEd->next_line = Next( pEd, pEd->next_line );
      }
      else
         hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Resets text buffer
 */
static void KillText( PHB_EDITOR pEd )
{
   memset( pEd->begin, '\0', pEd->buffer_size );
   pEd->first_line = pEd->last_line = 0;
}

/* Stores new text into editor */
HB_FUNC( ED_SETTEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      KillText( pEd );

      New( pEd, pEd->tab_size, pEd->line_length, pEd->buffer_size );

      AddText( pEd, hb_parcx( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Reads a text from the file */
HB_FUNC( ED_READTEXT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_BOOL    lSuccess = HB_FALSE;
      HB_FHANDLE nFile;
      HB_ISIZ    nSize;
      HB_FOFFSET nRead;
      HB_FOFFSET nSeek;

      KillText( pEd );
      New( pEd, pEd->tab_size, pEd->line_length, pEd->buffer_size );

      nFile = hb_numToHandle( hb_parnint( 2 ) );
      nSeek = hb_parnint( 3 );
      nSize = hb_parns( 4 );

      nRead = hb_fsSeekLarge( nFile, nSeek, FS_SET );
      if( nRead == nSeek )
      {
         if( nSize > ( pEd->buffer_size - 10 ) )
            nSize = pEd->buffer_size - 10;

         nSize = hb_fsReadLarge( nFile, pEd->begin, nSize );
         pEd->begin[ nSize ] = '\0';

         pEd->text_length = nSize;

         NewText( pEd );

         lSuccess     = HB_TRUE;
         pEd->fStable = HB_FALSE;
      }

      hb_retl( lSuccess );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * Stabilize the editor
 * Redisplays all requested lines
 * It is simmilar to TBrowse:forceStable()
 * Incremental stabilisation was too slow
 */
HB_FUNC( ED_STABILIZE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      int     nLeft, nTop;
      int     nRow = 0;
      HB_ISIZ nEscLen, nLen, width, i, j, e;
      char *  EscPtr;
      char *  cPtr;
      char    adres[ _MAX_LINE_LEN + 2 ];

      while( --pEd->stabil >= 0 )
      {
         /* there are some lines of text to display
          */
         width = pEd->right - pEd->left + 1;

         if( pEd->next_stabil >= 0 )
         {
            cPtr = pEd->begin + pEd->next_stabil;
            for( nEscLen = nLen = 0; *cPtr && *cPtr != '\n'; cPtr++ )
            {
               /* copy the line into temporary buffer - count characters used
                * as color escape codes
                */
               adres[ nLen++ ] = *cPtr;
               if( pEd->escape && *cPtr == pEd->escape )
                  nEscLen += 2;
            }
            j = nLen - nEscLen;    /* length of printable text */
            adres[ nLen ] = '\0';

            if( pEd->first_col >= j )
               adres[ nEscLen = nLen = 0 ] = '\0';  /* first visible column is greater then line length */

            else if( pEd->first_col )
            {
               /* text is scrolled right - we need to find the first
                * color escape code that will be visible
                * text ~2 in bold ~1 in normal
                *          ^-first visible column
                */
               e = 0;
               i = 0;
               if( pEd->escape )
               {
                  for( i = 0; i < ( pEd->first_col + e ); i++ )
                  {
                     if( adres[ i ] == pEd->escape )
                     {
                        adres[ 0 ] = adres[ i ];
                        adres[ 1 ] = adres[ ++i ];
                        e += 2;
                     }
                  }
               }

               if( e )
               {
                  nLen -= ( i - 2 );
                  if( adres[ i - 1 ] == pEd->escape )
                     i++, nLen--;
                  hb_strncpy( adres + 2, adres + i, nLen - 2 );
                  nEscLen -= ( e - 2 );
               }
               else
               {
                  nLen -= pEd->first_col;
                  hb_strncpy( adres, adres + pEd->first_col, nLen );
               }
               adres[ nLen ] = '\0';
            }

            if( nLen )
            {
               if( adres[ nLen - 1 ] & '\xd' ) /* soft or hard carriage */
                  adres[ --nLen ] = '\0';
            }

            /* find next line for displaying */
            switch( pEd->dir )
            {
               case _STABILIZE_DOWN:
                  pEd->next_stabil = Next( pEd, pEd->next_stabil );
                  nRow = pEd->current_stabil++;
                  break;

               case _STABILIZE_UP:
                  pEd->next_stabil = Prev( pEd, pEd->next_stabil );
                  nRow = pEd->current_stabil--;
                  break;
            }

            hb_gtColorSelect( 0 );    /* select default color */

            nTop = pEd->top + nRow;
            if( nLen )
            {
               if( pEd->escape && ( EscPtr = strchr( adres, pEd->escape ) ) != 0 )
               {
                  i     = EscPtr - adres;
                  nLeft = ( int ) ( pEd->left + i );

                  if( i )
                     hb_gtWriteAt( nTop, pEd->left, adres, ( width < i ) ? width : i );

                  for(; i < nLen && nLeft <= pEd->right; i++ )
                  {
                     if( adres[ i ] == pEd->escape )
                        hb_gtColorSelect( ( adres[ ++i ] & 0x0F ) - 1 );
                     else
                        hb_gtWriteAt( nTop, nLeft++, adres + i, 1 );
                  }
               }
               else
                  hb_gtWriteAt( nTop, pEd->left, adres, ( ( width < nLen ) ? width : nLen ) );
            }

            /* fill the rest of the row with spaces */
            if( ( nLen - nEscLen ) < width )
               hb_gtRepChar( nTop, ( int ) ( pEd->left + nLen - nEscLen ), ' ', width - nLen + nEscLen );
         }
         else
         {
            /* no more lines of text to display - fill the window with spaces
             */
            switch( pEd->dir )
            {
               case _STABILIZE_DOWN:
                  nRow = pEd->current_stabil++;
                  break;
               case _STABILIZE_UP:
                  nRow = pEd->current_stabil--;
                  break;
            }

            hb_gtColorSelect( 0 );
            hb_gtRepChar( pEd->top + nRow, pEd->left, ' ', width );
         }

         hb_gtColorSelect( 0 );
      }

      pEd->fStable = HB_TRUE;

      hb_retni( nRow );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Removes trailing spaces from the end of line
 */
static HB_ISIZ Clear( PHB_EDITOR pEd, HB_ISIZ e, HB_ISIZ * nEsc )
{
   HB_ISIZ nLen, i;

   nLen = GetLineLength( pEd, e, nEsc );
   i    = e + nLen + *nEsc;

   if( i )
   {
      while( pEd->begin[ i - 1 ] == ' ' )
      {
         if( pEd->cursor_col > 0 )
            pEd->cursor_col--;

         else if( pEd->first_col > 0 )
            pEd->first_col--;

         MoveText( pEd, i, i - 1, pEd->text_length - i + 3 );
         nLen--;
      }
   }

   return nLen;
}

/* Moves the cursor to the next line of text
 */
static void Down( PHB_EDITOR pEd )
{
   HB_ISIZ j;
   HB_ISIZ nEsc;

   j = Next( pEd, pEd->current_line ); /* find the offset of next line */
   if( pEd->begin[ j ] == '\0' )
      j = -1;                          /* no more lines */
   if( j < 0 )
   {
      pEd->fStable = HB_TRUE;
   }
   else
   {
      pEd->active++;
      Clear( pEd, pEd->current_line, &nEsc );

      j = Next( pEd, pEd->current_line );

      if( j >= 0 )
         pEd->current_line = j;

      if( ( ( ++pEd->cursor_row ) + pEd->top ) > pEd->bottom )
      {
         /* attempt to move to the line that was not visible yet
          */
         pEd->stabil = 1;          /* only one line needs to be redisplayed */

         pEd->cursor_row    = pEd->bottom - pEd->top;
         pEd->first_display = Next( pEd, pEd->first_display );
         pEd->last_display  = j;

         pEd->fStable        = HB_FALSE;
         pEd->next_stabil    = pEd->last_display;
         pEd->dir            = _STABILIZE_UP;
         pEd->current_stabil = pEd->cursor_row;
      }
      else
      {
         /* the new line is already visible */
         if( pEd->line_number <= ( pEd->bottom - pEd->top + 1 ) )
            pEd->last_display = pEd->last_line;  /* the total number of lines is smaller then rows to display */
      }
   }
   if( pEd->current_line > pEd->last_display )
      pEd->last_display = pEd->current_line;
   if( pEd->current_line > pEd->last_line )
      pEd->last_line = pEd->current_line;
}


/* Moves cursor to the next line of text */
HB_FUNC( ED_DOWN )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      Down( pEd );

      hb_retl( pEd->fStable );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Moves the cursor to the previous line of text
 */
static void Up( PHB_EDITOR pEd )
{
   int     j, i;
   HB_ISIZ jj, tmp;
   HB_ISIZ nEsc;

   /* find the previous line */
   jj = Prev( pEd, pEd->current_line );
   if( jj < 0 )
   {
      pEd->fStable = HB_TRUE;
   }
   else
   {
      pEd->active--;
      Clear( pEd, pEd->current_line, &nEsc );
      pEd->current_line = jj;
      if( ( ( --pEd->cursor_row ) + pEd->top ) < pEd->top )
      {
         /* the new line was not displayed yet */
         pEd->stabil = 1;   /* only one line needs redisplay - rest of lines will be scrolled */

         j = 0;
         pEd->cursor_row = 0;

         /* count the number of lines that will be visible in the window.
          * If the number of lines is smaller then the window height then
          * the offset of last displayed line will be not changed
          */
         pEd->first_display = Prev( pEd, pEd->first_display );
         tmp = pEd->first_display;
         for( i = 0; i < pEd->bottom - pEd->top + 1; i++ )
         {
            jj = Next( pEd, tmp );
            if( jj >= 0 )
            {
               tmp = jj;
               j++;
            }
         }
         if( j == ( pEd->bottom - pEd->top + 1 ) )
            pEd->last_display = Prev( pEd, pEd->last_display );

         pEd->fStable        = HB_FALSE;
         pEd->next_stabil    = pEd->current_line;
         pEd->dir            = _STABILIZE_DOWN;
         pEd->current_stabil = 0;
      }
   }
}

/* Moves the cursor to the previous line */
HB_FUNC( ED_UP )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      Up( pEd );

      hb_retl( pEd->fStable );
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Moves the cursor to the next page of text */
HB_FUNC( ED_PGDOWN )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      int     i;
      HB_ISIZ j;

      j = Next( pEd, pEd->last_display );
      if( pEd->begin[ j ] == '\0' ) /* no more lines */
      {
         pEd->fStable = HB_TRUE;
         /* advance the cursor as much as possible (to the last line) */
         for( i = 0; i < pEd->bottom - pEd->top + 1; i++ )
            Down( pEd );
         return;
      }

      /* the last possible line will be displayed at the very bottom of the window */
      /* find the last line to display */
      for( i = 0; i < pEd->bottom - pEd->top; i++ )
      {
         j = Next( pEd, pEd->last_display );
         if( j >= 0 )
         {
            if( pEd->begin[ j ] != '\0' )
            {
               pEd->active++;
               pEd->last_display = j;
            }
         }
         else
            break;   /* no more lines */
      }

      if( pEd->begin[ pEd->last_display ] == '\0' )
         pEd->last_display = Prev( pEd, pEd->last_display );
      pEd->first_display = pEd->last_display;

      /* find the first displayed line now */
      for( i = 0; i < pEd->bottom - pEd->top; i++ )
      {
         j = Prev( pEd, pEd->first_display );
         if( j >= 0 )
            pEd->first_display = j;
         else
            pEd->first_display = 0;
      }

      /* find the offset of the line where the currsor will be displayed */
      pEd->current_line = pEd->last_display;
      for( i = 0; i < pEd->bottom - pEd->top - pEd->cursor_row; i++ )
      {
         j = Prev( pEd, pEd->current_line );
         if( j >= 0 )
            pEd->current_line = j;
         else
            pEd->current_line = 0;
      }

      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Moves the cursor to the previous page of text */
HB_FUNC( ED_PGUP )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      int     i, bt;
      HB_ISIZ j;

      bt = pEd->bottom - pEd->top;

      j = Prev( pEd, pEd->first_display );
      if( j < 0 ) /* no more lines to move */
      {
         pEd->fStable = HB_TRUE;
         /* advannce the cursor to the topmost line */
         for( i = 0; i < bt + 1; i++ )
            Up( pEd );
         return;
      }

      /* find the offset of the first visible line */
      for( i = 0; i < bt; i++ )
      {
         j = Prev( pEd, pEd->first_display );
         if( j >= 0 )
         {
            pEd->active--;
            pEd->first_display = j;
         }
         else
            break;   /* no more line */
      }
      /* now the last visible line */
      pEd->last_display = pEd->first_display;
      for( i = 0; i < bt; i++ )
      {
         j = Next( pEd, pEd->last_display );
         if( j >= 0 )
            pEd->last_display = j;
      }

      /* update the offset of line where the cursor will be displayed
       * keep the cursor in the same row if possible
       */
      pEd->current_line = pEd->last_display;
      for( i = 0; i < bt - pEd->cursor_row; i++ )
      {
         j = Prev( pEd, pEd->current_line );
         if( j >= 0 )
            pEd->current_line = j;
         else
            pEd->current_line = 0;
      }

      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->last_display;
      pEd->stabil         = bt + 1;
      pEd->dir            = _STABILIZE_UP;
      pEd->current_stabil = bt;
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the beginning of the text */
HB_FUNC( ED_TOP )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ j;
      int     i;
      HB_ISIZ nEsc;

      Clear( pEd, pEd->current_line, &nEsc );
      pEd->current_line  = pEd->first_line;
      pEd->cursor_row    = 0;
      pEd->first_display = pEd->last_display = pEd->first_line;

      /* find the last visible line */
      for( i = 0; i < pEd->bottom - pEd->top; i++ )
      {
         j = Next( pEd, pEd->last_display );
         if( j >= 0 )
            pEd->last_display = j;
      }

      pEd->cursor_row   = pEd->current_stabil = 0;
      pEd->current_line = pEd->next_stabil = pEd->first_line;
      pEd->active       = 1;
      pEd->fStable      = HB_FALSE;
      pEd->stabil       = pEd->bottom - pEd->top + 1;
      pEd->dir       = _STABILIZE_DOWN;
      pEd->first_col = pEd->cursor_col = 0;
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the last line of text */
HB_FUNC( ED_BOTTOM )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      int     i, j;
      HB_ISIZ jj;
      HB_ISIZ nEsc;

      j = 0;
      Clear( pEd, pEd->current_line, &nEsc );
      pEd->current_line  = pEd->last_line;
      pEd->cursor_row    = 0;
      pEd->first_display = pEd->last_line;

      /* find the first visible line */
      /* We have to count from the bottom to make it work in case the number
       * of lines is smaller then the height of the window
       */
      pEd->last_display = pEd->first_display;
      for( i = 0; i < pEd->bottom - pEd->top; i++ )
      {
         jj = Prev( pEd, pEd->first_display );
         if( jj >= 0 )
         {
            pEd->first_display = jj;
            j++;
         }
      }

      pEd->current_line = pEd->last_display;
      pEd->active       = pEd->line_number;
      pEd->fStable      = HB_FALSE;
      pEd->next_stabil  = pEd->first_display;
      pEd->dir = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
      pEd->first_col      = pEd->cursor_col = 0;
      pEd->cursor_row     = j;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Go to the specified line number
 */
static void GoTo( PHB_EDITOR pEd, HB_ISIZ line )
{
   int     i;
   HB_ISIZ j;
   HB_ISIZ nEsc;

   Clear( pEd, pEd->current_line, &nEsc );

   /* find specified line */
   pEd->current_line = pEd->first_line;
   for( i = 0; i < line - 1; i++ )
   {
      j = Next( pEd, pEd->current_line );
      if( j >= 0 )
         pEd->current_line = j;
   }
   pEd->cursor_row    = 0;
   pEd->first_display = pEd->current_line;

   /* find the offset of the last visible line */
   pEd->last_display = pEd->first_display;
   for( i = 0; i < pEd->bottom - pEd->top; i++ )
   {
      j = Next( pEd, pEd->last_display );
      if( j >= 0 )
         pEd->last_display = j;
   }

   pEd->active         = line;
   pEd->fStable        = HB_FALSE;
   pEd->next_stabil    = pEd->current_line;
   pEd->stabil         = pEd->bottom - pEd->top + 1;
   pEd->dir            = _STABILIZE_DOWN;
   pEd->current_stabil = pEd->cursor_row;
   pEd->first_col      = 0;
}

/* Move the cursor to the given line using line number */
HB_FUNC( ED_GOTO )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      GoTo( pEd, hb_parns( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the previous character
 */
static void Left( PHB_EDITOR pEd )
{
   if( pEd->cursor_col > 0 )
      pEd->cursor_col--;    /* inside the window - decrement current column number */
   else
   {
      if( pEd->first_col > 0 )
      {
         /* the text is scrolled right - scroll it back to the left by one column */
         pEd->first_col--;
         pEd->fStable        = HB_FALSE;
         pEd->next_stabil    = pEd->first_display;
         pEd->stabil         = pEd->bottom - pEd->top + 1;
         pEd->dir            = _STABILIZE_DOWN;
         pEd->current_stabil = 0;
      }
      /* else no wrap allowed */
   }
}

/* Move the cursor to the previous character */
HB_FUNC( ED_LEFT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      Left( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Move the cursor to the next character
 */
static void Right( PHB_EDITOR pEd )
{
   if( pEd->cursor_col < ( pEd->right - pEd->left ) ) /* inside the window */
   {
      if( ( pEd->first_col + pEd->cursor_col ) < pEd->line_length )
         pEd->cursor_col++;
      /* else no wrap allowed */
   }
   else
   {
      /* scroll text to the right no more then the maximal line length */
      if( ( ++pEd->first_col + pEd->cursor_col ) > pEd->line_length )
         pEd->first_col--;

      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
}

/* Move the cursor to the next character */
HB_FUNC( ED_RIGHT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      Right( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the beginning of line
 */
static void Home( PHB_EDITOR pEd )
{
   if( pEd->first_col > 0 )
   {
      /* the line was scrolled to the right */
      pEd->cursor_col     = 0;
      pEd->first_col      = 0;
      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
   else
   {
      pEd->cursor_col = 0;
      pEd->first_col  = 0;
   }
}


/* Move the cursor to the beginning of the line */
HB_FUNC( ED_HOME )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      Home( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the end of line
 */
static void End( PHB_EDITOR pEd )
{
   HB_ISIZ ll;
   HB_ISIZ nEsc;

   ll = Clear( pEd, pEd->current_line, &nEsc );
   if( ll < pEd->first_col )
   {
      /* the line length is smaller then the number of characters scrolled -
       * adjust the first visible column to make the end of line visible
       */
      pEd->first_col      = ll;
      pEd->cursor_col     = 0;
      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
   else if( ( ll - pEd->first_col ) > ( pEd->right - pEd->left ) )
   {
      /* scroll text to the right  */
      pEd->cursor_col     = pEd->right - pEd->left;
      pEd->first_col      = ll - ( pEd->right - pEd->left );
      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
   else
      pEd->cursor_col = ll - pEd->first_col;
}


/* Move the cursor the the end of line (after the last non-space character) */
HB_FUNC( ED_END )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      End( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Format the current paragraph */
static void FormatParagraph( PHB_EDITOR pEd )
{
   HB_ISIZ rdl;
   int     cr, cor;
   HB_ISIZ cc;
   HB_ISIZ dl, source, CrLine;
   char    pom[ _MAX_LINE_LEN * 2 ];
   char *  tmp;
   HB_ISIZ nEsc;

   cc  = pEd->cursor_col;
   cr  = pEd->cursor_row;
   cor = 0;

   rdl         = format_line( pEd, HB_CHAR_SOFT1, 0 );
   pEd->stabil = 1;    /* at least one line will be redisplayed */

/*  if( rdl )
 */
   {
      dl = GetLineLength( pEd, pEd->current_line, &rdl );
      hb_strncpy( pom, pEd->begin + pEd->current_line, dl + rdl + 10 );
      pom[ pEd->line_length + rdl + 1 ] = '\0';
      tmp = strchr( pom, '\n' );
      if( tmp && ( *( tmp - 1 ) == HB_CHAR_SOFT1 ) )
      {
         tmp--;
         cor++;
      }
      else
         tmp = NULL;

      CrLine = pEd->current_line;

      while( tmp )
      {
         source = pEd->current_line + ( tmp - pom - 1 );
         MoveText( pEd, source + 2, source + 1, pEd->buffer_size - source + 2 );
         pEd->begin[ source + 1 ] = ' ';

         rdl = format_line( pEd, HB_CHAR_SOFT1, 0 );
         Clear( pEd, pEd->current_line, &nEsc );

         pEd->current_line = Next( pEd, pEd->current_line );
         dl = GetLineLength( pEd, pEd->current_line, &rdl );
         hb_strncpy( pom, pEd->begin + pEd->current_line, dl + rdl + 10 );
         pom[ pEd->line_length + rdl + 1 ] = '\0';
         tmp = strchr( pom, '\n' );
         if( tmp && ( *( tmp - 1 ) == HB_CHAR_SOFT1 ) )
         {
            tmp--;
            cor++;
         }
         else
            tmp = NULL;

      }
      pEd->current_line   = CrLine;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->next_stabil    = pEd->first_display;
      pEd->current_stabil = 0;
   }
/*  else
 */
   {
      pEd->stabil         = 1;
      pEd->next_stabil    = pEd->current_line;
      pEd->current_stabil = pEd->cursor_row;
   }

   pEd->fStable      = HB_FALSE;
   pEd->dir          = _STABILIZE_DOWN;
   pEd->cursor_col   = cc;
   pEd->cursor_row   = cr;
   pEd->line_number -= cor;
}



/* Delete the character under the cursor
 */
static void DelChar( PHB_EDITOR pEd )
{
   HB_ISIZ ccc;
   HB_ISIZ rdl;
   HB_ISIZ cl;

   cl  = pEd->current_line;
   ccc = pEd->cursor_col + pEd->first_col;
   if( ccc <= GetLineLength( pEd, pEd->current_line, &rdl ) )
   {
      if( pEd->escape )
      {
         while( pEd->begin[ pEd->current_line + ccc ] == pEd->escape )
            ccc += 2;
      }
      MoveText( pEd, pEd->current_line + ccc + 1,
                pEd->current_line + ccc,
                ( pEd->buffer_size - ( pEd->current_line + ccc + 1 ) ) );

      pEd->fStable     = HB_FALSE;
      pEd->next_stabil = pEd->current_line;

      pEd->dir = _STABILIZE_DOWN;
      pEd->current_stabil = pEd->cursor_row;

      FormatParagraph( pEd );

      if( pEd->current_line == pEd->last_line )
         pEd->last_display = pEd->last_line;

      pEd->current_line = cl;

      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
}

/* Delete the character at current cursor position */
HB_FUNC( ED_DELCHAR )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ j;
      HB_ISIZ rdl;

      if( ( pEd->cursor_col + pEd->first_col ) >=
          GetLineLength( pEd, pEd->current_line, &rdl ) )
      {
         /* The cursor is positioned after the last non-space character
          */
         j = Next( pEd, pEd->current_line );
         if( j >= 0 )
         {
            /* there are more lines below the cursor - join the lines
             */
            Down( pEd );               /* goto the next line */
            Home( pEd );               /* goto the beginning of line */
            BackSpace( pEd, HB_TRUE ); /* delete separating CR/LF */

            pEd->fStable        = HB_FALSE;
            pEd->next_stabil    = pEd->first_display;
            pEd->stabil         = pEd->bottom - pEd->top + 1;
            pEd->dir            = _STABILIZE_DOWN;
            pEd->current_stabil = 0;
         }
      }
      else
      {
         /* The cursor is inside the line or at the last character
          */
         if( ( pEd->cursor_col + pEd->first_col ) <
             GetLineLength( pEd, pEd->current_line, &rdl ) )
            DelChar( pEd );   /* inside a line */
         else /* at the last character */
         {
            j = Next( pEd, pEd->current_line );
            if( j >= 0 )  /* if it is not the last line then delete character unde the cursor */
               DelChar( pEd );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Delete a character on the left side of the cursor
 */
static void BackSpace( PHB_EDITOR pEd, HB_BOOL fInsert )
{
   char    tmp[ _MAX_LINE_LEN + 2 ];
   char    tmp1[ _MAX_LINE_LEN + 2 ];
   char *  w;
   HB_ISIZ ww, j, ccc, kk;
   HB_ISIZ rdl, nLen;

   pEd->fStable        = HB_FALSE;
   pEd->next_stabil    = pEd->current_line;
   pEd->stabil         = 1;
   pEd->dir            = _STABILIZE_DOWN;
   pEd->current_stabil = pEd->cursor_row;

   if( fInsert )
   {
      if( ( ccc = pEd->cursor_col + pEd->first_col ) >
          GetLineLength( pEd, pEd->current_line, &rdl ) )
         fInsert = HB_FALSE;  /* cursor is scrolled after the last character in the line - just move the cursor left */
   }

   if( fInsert )
   {
      /* in destructive mode
       */
      *tmp  = '\0';
      *tmp1 = '\0';
      if( ( ccc = pEd->cursor_col + pEd->first_col ) > 0 )
      {
         /* inside the line */
         MoveText( pEd, pEd->current_line + ccc, pEd->current_line + ccc - 1,
                   ( pEd->buffer_size - ( pEd->current_line + ccc + 1 ) ) );
         Left( pEd );
      }
      else
      {
         /* at the beginning of the line */
         if( pEd->current_line != pEd->first_line )
         {
            /* this is not the first line */
            nLen = GetLineLength( pEd, pEd->current_line, &rdl );

            if( pEd->current_line == pEd->last_line )
               if( nLen == 0 )
                  pEd->last_line = Prev( pEd, pEd->last_line );

            /* copy the last line into temporary buffer */
            hb_strncpy( tmp, pEd->begin + pEd->current_line, nLen + rdl );

            /* find the first space in current line (the new line will
             * be wrapped eventually at this position) */
            if( ( w = strchr( tmp, ' ' ) ) != 0 )
               ww = w - tmp;
            else
               ww = nLen + rdl;

            /* go to the previous line */
            j  = Prev( pEd, pEd->current_line );
            kk = GetLineLength( pEd, j, &rdl );
            hb_strncpy( tmp1, pEd->begin + j, kk + rdl );
            Up( pEd );
            End( pEd );

            /* the lines can be joined
             * the sum of whole length of these lines is smaller then maximal allowed
             * or there is a space where the line can be wrapped
             */
            if( ( ww + kk + rdl - 1 ) < pEd->line_length )
            {
               kk = GetLineLength( pEd, pEd->current_line, &rdl );
               j  = pEd->current_line + kk + rdl;
               /* remove separating CRLF characters */
               MoveText( pEd, j + 2, j, pEd->buffer_size - j - 2 );

               pEd->line_number--;

               j = Next( pEd, pEd->last_display );
               if( j >= 0 )
                  pEd->last_display = j;
               if( pEd->begin[ pEd->last_display + 1 ] == '\0' )
                  pEd->last_display = Prev( pEd, pEd->last_display );

               /* split the new line if it is too long */
               format_line( pEd, HB_CHAR_HARD1, 0 );

               j = Next( pEd, pEd->current_line );
               if( j < 0 )
               {
                  pEd->last_display = Prev( pEd, pEd->last_display );
                  pEd->last_line    = pEd->current_line;
               }
               pEd->fStable        = HB_FALSE;
               pEd->next_stabil    = pEd->first_display;
               pEd->stabil         = pEd->bottom - pEd->top + 1;
               pEd->dir            = _STABILIZE_DOWN;
               pEd->current_stabil = 0;
            }
         }
      }
      FormatParagraph( pEd );

      pEd->fStable        = HB_FALSE;
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->current_stabil = 0;
   }
   else
   {
      /* non-destructive mode - move cursor only */
      ccc = pEd->cursor_col + pEd->first_col;
      if( ccc > 0 )
      {
         Left( pEd );
      }
      else
      {
         if( pEd->current_line != pEd->first_line )
         {
            Up( pEd );
            End( pEd );
         }
      }
   }
}


/* Delete a character on the left side of the cursor */
HB_FUNC( ED_BSPACE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      BackSpace( pEd, hb_parl( 2 ) /* fInsert */ );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Move to the beginning of next non-empty line
 */
static void GotoNextNonEmptyLine( PHB_EDITOR pEd )
{
   HB_ISIZ rdl;

   Down( pEd );
   Home( pEd );

   while( GetLineLength( pEd, pEd->current_line, &rdl ) == 0 )
   {
      Down( pEd );
      Home( pEd );
      if( Next( pEd, pEd->current_line ) < 0 )
         break;
   }
}


/* Move the cursor to the next word
 */
static void NextWord( PHB_EDITOR pEd )
{
   char *  adr;
   char    tmp[ _MAX_LINE_LEN + 2 ];
   HB_ISIZ ccc;
   HB_ISIZ nLen;
   HB_ISIZ nEsc;

   ccc  = pEd->cursor_col + pEd->first_col;
   nLen = Clear( pEd, pEd->current_line, &nEsc );

   if( nLen < ccc )
      GotoNextNonEmptyLine( pEd );
   else
   {
      *tmp = '\0';
      hb_strncpy( tmp, pEd->begin + pEd->current_line + ccc,
                  nLen - pEd->cursor_col - pEd->first_col );
      if( ( adr = strchr( tmp, ' ' ) ) == NULL )
      {
         GotoNextNonEmptyLine( pEd );
         if( ! pEd->fStable )
         {
            pEd->next_stabil    = pEd->first_display;
            pEd->stabil         = pEd->bottom - pEd->top + 1;
            pEd->dir            = _STABILIZE_DOWN;
            pEd->current_stabil = 0;
         }
      }
      else
      {
         pEd->cursor_col = adr - tmp + 1 + pEd->cursor_col + pEd->first_col;

         if( pEd->cursor_col > ( pEd->right - pEd->left ) )
         {
            pEd->first_col     += pEd->cursor_col - ( pEd->right - pEd->left );
            pEd->cursor_col     = pEd->right - pEd->left;
            pEd->fStable        = HB_FALSE;
            pEd->next_stabil    = pEd->first_display;
            pEd->stabil         = pEd->bottom - pEd->top + 1;
            pEd->dir            = _STABILIZE_DOWN;
            pEd->current_stabil = 0;
         }
      }
   }

   if( pEd->begin[ ( pEd->current_line +
                     pEd->cursor_col + pEd->first_col ) ] == ' ' )
      NextWord( pEd );
}


/* Move the cursor to the next word */
HB_FUNC( ED_NWORD )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      NextWord( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Move the cursor to the previous word
 */
static void PreviousWord( PHB_EDITOR pEd )
{
   HB_ISIZ pom;
   HB_ISIZ i;
   HB_ISIZ rdl;
   HB_ISIZ nLen;
   HB_ISIZ nEsc;

   pom  = -1;
   nLen = Clear( pEd, pEd->current_line, &nEsc );
   if( nLen < ( pEd->cursor_col + pEd->first_col ) )
      End( pEd );

   if( ( pEd->first_col + pEd->cursor_col ) == 0 )
   {
      Up( pEd );
      End( pEd );
   }

   for( i = pEd->first_col + pEd->cursor_col - 2; i >= 0; i-- )
   {
      if( pEd->begin[ pEd->current_line + i ] == ' ' )
      {
         pom = i;
         break;
      }
      else
         pom = -1;
   }

   if( pom < 0 )
   {
      Home( pEd );
      while( GetLineLength( pEd, pEd->current_line, &rdl ) == 0 )
      {
         Up( pEd );
         Home( pEd );
         if( Prev( pEd, pEd->current_line ) < 0 )
            break;
      }
      if( ! pEd->fStable )
      {
         pEd->next_stabil    = pEd->first_display;
         pEd->stabil         = pEd->bottom - pEd->top + 1;
         pEd->dir            = _STABILIZE_DOWN;
         pEd->current_stabil = 0;
      }
   }
   else
   {
      pEd->cursor_col = pom + 1;
      if( pEd->first_col > 0 )
      {
         pEd->cursor_col -= pEd->first_col;
      }
      if( pEd->cursor_col < 0 )
      {
         pEd->first_col      = pEd->cursor_col - ( pEd->right - pEd->left );
         pEd->fStable        = HB_FALSE;
         pEd->next_stabil    = pEd->first_display;
         pEd->stabil         = pEd->bottom - pEd->top + 1;
         pEd->dir            = _STABILIZE_DOWN;
         pEd->current_stabil = 0;
      }
   }

   if( pEd->begin[ pEd->current_line + pEd->cursor_col +
                   pEd->first_col ] == ' ' )
      PreviousWord( pEd );
}


/* Move the cursor to the previous word */
HB_FUNC( ED_PWORD )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      PreviousWord( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Format given line - returns it the line has changed
 */
static HB_BOOL format_line( PHB_EDITOR pEd, char Karetka, HB_ISIZ LineDl )
{
   char    pom[ _MAX_LINE_LEN * 2 ];
   char *  p;
   HB_ISIZ podz, jj, i;
   HB_BOOL status;
   HB_ISIZ j;
   HB_ISIZ rdl = 0;

   if( ! LineDl )
      LineDl = GetLineLength( pEd, pEd->current_line, &rdl );

   status = HB_FALSE; /* the line is not splitted yet */
   if( LineDl > pEd->line_length )
   {
      /* the line is longer then maximal allowed length  -
       * wrap the line
       */
      status = HB_TRUE; /* the line will be splitted */

      /* copy maximum allowed bytes form the line into temporary buffer */
      hb_strncpy( pom, pEd->begin + pEd->current_line,
                  pEd->line_length + 10 + rdl );
      pom[ pEd->line_length + rdl ] = '\0';

      /* find the last space where the line can be splitted */
      p = strrchr( pom, ' ' );
      if( p )
      {
         /* use this position to split the line */
         podz = p - pom + 1;
         jj   = 1 - podz + pEd->cursor_col + pEd->first_col;
      }
      else
      {
         /* there is no space in the line - split it at the maximal line length */
         podz = pEd->line_length;
         jj   = 1;
      }

      j = pEd->current_line + podz;
      MoveText( pEd, j, j + 2, pEd->buffer_size - j - 2 );

      /* replace with separators */
      pEd->begin[ j + 0 ] = Karetka;
      pEd->begin[ j + 1 ] = '\n';
      pEd->line_number++;

      if( ( pEd->cursor_col + pEd->first_col ) >= podz )
      {
         Home( pEd );
         Down( pEd );
         for( i = 0; i < jj; i++ )
            Right( pEd );
      }
      else
         Right( pEd );
   }

   return status;
}

/* Appends the character at the end of line
 */
static HB_BOOL AppendChar( PHB_EDITOR pEd, char znak, char podz )
{
   HB_BOOL status;
   HB_ISIZ diff;
   HB_ISIZ rdl;
   HB_ISIZ iPos, nLen;
   HB_ISIZ cl;
   HB_ISIZ ccol, fcol;
   char *  cNew;

   nLen = GetLineLength( pEd, pEd->current_line, &rdl );
   iPos = pEd->current_line + nLen;
   diff = pEd->cursor_col + pEd->first_col - nLen;

   /* the cursor is positioned 'diff' columns after the last character
    * in the line - fill the gap with spaces before appending the character
    */
   MoveText( pEd, iPos, iPos + diff + 1, pEd->buffer_size - 1 - iPos - diff );
   memset( pEd->begin + ( pEd->current_line + nLen ), ' ', diff );

   /* move the CRLF characters after the appended character */
   cNew    = pEd->begin + iPos + diff;
   *cNew++ = znak;
   if( *cNew != '\r' )
      *cNew = podz;  /* insert requested soft/hard carriage */
   *++cNew = '\n';

   /* the last line always have to end with the hard carriage return */
   pEd->begin[ pEd->text_length - 2 ] = '\r';

   status = format_line( pEd, HB_CHAR_SOFT1, 0 );

   cl   = pEd->current_line;
   ccol = pEd->cursor_col;
   fcol = pEd->first_col;

   FormatParagraph( pEd );

   pEd->current_line = cl;
   pEd->cursor_col   = ccol;
   pEd->first_col    = fcol;

   return status;
}


/* Checks if there is enough free room in the text buffer
 */
static HB_BOOL Check_length( PHB_EDITOR pEd, HB_ISIZ iRequested )
{
   if( ( pEd->text_length + iRequested ) <= ( pEd->buffer_size - 8 ) )
      return HB_TRUE;
   else
      return HB_FALSE;
}


/* Adjusts the offset of last line
 */
static void SetLastLine( PHB_EDITOR pEd )
{
   if( pEd->current_line > pEd->last_line )
      pEd->last_line = pEd->current_line;
}


/* Insert or replace the new character into the text buffer
 */
static void PutChar( PHB_EDITOR pEd, HB_BOOL fInsert, char znak )
{
   HB_BOOL jj;
   HB_ISIZ i, cc;
   HB_ISIZ rdl;
   HB_ISIZ cl;
   HB_ISIZ ccol, fcol;

   jj = HB_FALSE;
   cc = pEd->cursor_col + pEd->first_col;   /* currnt position in the line */
   if( fInsert )
   {
      /* INSERT is ON */
      if( Check_length( pEd, 1 ) )
      {
         /* TODO: add reallocation of text buffer
          */
         if( cc < GetLineLength( pEd, pEd->current_line, &rdl ) )
         {
            /* the character will be inserted within the line - the cursor
             * is placed inside the line
             */
            i = pEd->current_line + cc;
            MoveText( pEd, i, i + 1, pEd->buffer_size - pEd->current_line - cc - 1 );
            pEd->begin[ i ] = znak;

            jj = format_line( pEd, HB_CHAR_SOFT1, 0 );

            cl   = pEd->current_line;
            ccol = pEd->cursor_col;
            fcol = pEd->first_col;

            FormatParagraph( pEd );

            pEd->current_line = cl;
            pEd->cursor_col   = ccol;
            pEd->first_col    = fcol;

            if( jj )
               SetLastLine( pEd );
         }
         else  /* the cursor is located after the last character in the line */
            jj = AppendChar( pEd, znak, HB_CHAR_SOFT1 );

         if( ! jj )
            Right( pEd );
         else
            SetLastLine( pEd );
      }
   }
   else
   {
      if( cc < GetLineLength( pEd, pEd->current_line, &rdl ) )
      {
         pEd->begin[ pEd->current_line + cc ] = znak;
         jj = HB_FALSE;
         Right( pEd );
      }
      else
      if( Check_length( pEd, 1 ) )
      {
         jj = AppendChar( pEd, znak, HB_CHAR_SOFT1 );
         if( ! jj )
            Right( pEd );
         else
            SetLastLine( pEd );
      }
   }

   if( ! jj )
   {
      if( ( pEd->cursor_col + pEd->first_col ) > ( pEd->right - pEd->left ) )
         jj = HB_TRUE;
   }

   if( jj )
   {
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->next_stabil    = pEd->first_display;
      pEd->current_stabil = 0;
   }
   else
   {
      pEd->stabil         = 1;
      pEd->next_stabil    = pEd->current_line;
      pEd->current_stabil = pEd->cursor_row;
   }

   pEd->fStable = HB_FALSE;
   pEd->dir     = _STABILIZE_DOWN;
}


/* Insert or replace the character into the text buffer */
HB_FUNC( ED_PUTCHAR )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      PutChar( pEd,
               hb_parl( 3 ),              /* current INSERT state */
               ( char ) hb_parni( 2 ) );  /* character to paste */
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


#if 0
static void Tab( PHB_EDITOR pEd, HB_BOOL fInsert )
{
   if( fInsert )
   {
      for( i = 0; i < pEd->tab_size; i++ )
         PutChar( pEd, HB_TRUE, ' ' );
   }
   else
   {
      for( i = 0; i < pEd->tab_size; i++ )
         Right( pEd );
   }
}
#endif

/*
**
*/

#if 0
HB_FUNC( ED_TAB )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      Tab( pEd, hb_parl( 2 ) /* fInsert */ );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
#endif

static void DelLine( PHB_EDITOR pEd )
{
   HB_ISIZ tmp, j;

   if( pEd->active < pEd->line_number )
   {
      j = Next( pEd, pEd->last_display );
      if( j >= 0 )
         pEd->last_display = j;

      tmp = Next( pEd, pEd->current_line );
      if( tmp < 0 )
         tmp = 0;

      pEd->stabil = pEd->bottom - pEd->top + 1 - pEd->cursor_row;
      pEd->dir    = _STABILIZE_DOWN;

      MoveText( pEd, tmp, pEd->current_line, pEd->buffer_size - pEd->current_line - 2 );

      if( pEd->line_number > 0 )
         pEd->line_number--;

      pEd->next_stabil    = pEd->current_line;
      pEd->current_stabil = pEd->cursor_row;
      pEd->fStable        = HB_FALSE;
   }
   else
   {
      pEd->begin[ pEd->current_line + 0 ] = '\r';
      pEd->begin[ pEd->current_line + 1 ] = '\n';
      pEd->begin[ pEd->current_line + 2 ] = '\0';
      memset( pEd->begin + pEd->current_line + 2, '\0',
              pEd->buffer_size - strlen( pEd->begin ) );

      pEd->last_display = pEd->last_line;
      pEd->stabil       = 1;
      pEd->dir            = _STABILIZE_DOWN;
      pEd->next_stabil    = pEd->current_line;
      pEd->current_stabil = pEd->cursor_row;
      pEd->fStable        = HB_FALSE;
   }
   if( pEd->text_length == 0 )
   {
      pEd->begin[ 0 ] = '\r';
      pEd->begin[ 1 ] = '\n';
      pEd->begin[ 2 ] = '\0';
   }
}


/* Delete the current line */
HB_FUNC( ED_DELLINE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      DelLine( pEd );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Delete the word on the right side of the cursor */
HB_FUNC( ED_DELWORD )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
   {
      HB_ISIZ pos1, pos2, j;
      HB_ISIZ cc, fc;
      int     cr;
      HB_ISIZ rdl;
      HB_ISIZ fd, ld;
      HB_ISIZ l;

      j = pEd->current_line + pEd->cursor_col + pEd->first_col;
      if( pEd->begin[ j ] != ' ' )
      {
         if( ( pEd->cursor_col + pEd->first_col ) <
             GetLineLength( pEd, pEd->current_line, &rdl ) )
         {
            cc = pEd->cursor_col;
            cr = pEd->cursor_row;
            fc = pEd->first_col;
            fd = pEd->first_display;
            ld = pEd->last_display;
            l  = pEd->current_line;
            NextWord( pEd );
            pos2 = pEd->cursor_col + pEd->first_col;
            PreviousWord( pEd );
            pos1 = pEd->cursor_col + pEd->first_col;

            pEd->current_line  = l;
            pEd->cursor_col    = cc;
            pEd->cursor_row    = cr;
            pEd->first_col     = fc;
            pEd->first_display = fd;
            pEd->last_display  = ld;

            if( pos2 == 0 )
               pos2 = GetLineLength( pEd, pEd->current_line, &rdl );

            MoveText( pEd, pEd->current_line + pos2, pEd->current_line + pos1,
                      pEd->buffer_size - pEd->current_line - pos2 );
            FormatParagraph( pEd );
            pEd->fStable        = HB_FALSE;
            pEd->next_stabil    = pEd->first_display;
            pEd->stabil         = pEd->bottom - pEd->top + 1;
            pEd->dir            = _STABILIZE_DOWN;
            pEd->current_stabil = 0;
         }
         else
         {
            if( ( GetLineLength( pEd, pEd->current_line, &rdl ) ) == 0 )
            {
               DelLine( pEd );
               Home( pEd );
            }
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Insert the CRLF characters
 */
static void Return( PHB_EDITOR pEd, HB_BOOL fInsert )
{
   HB_ISIZ ii, j;
   HB_ISIZ nLen;
   HB_ISIZ nEsc;

   if( Check_length( pEd, 2 ) )
   {
      if( fInsert )
      {
         /* only if INSERT state is ON
          */
         nLen = Clear( pEd, pEd->current_line, &nEsc );

         pEd->line_number++;

         j = Next( pEd, pEd->current_line );
         if( j < 0 )
         {
            pEd->last_line    = pEd->text_length;
            pEd->text_length += 2;

            pEd->begin[ pEd->text_length - 2 ] = '\r';
            pEd->begin[ pEd->text_length - 1 ] = '\n';
            pEd->begin[ pEd->text_length ]     = '\0';
         }
         else
         {
            if( ( pEd->first_col + pEd->cursor_col ) > nLen + 1 )
               End( pEd );
            ii = pEd->current_line + pEd->first_col +
                 pEd->cursor_col;
            MoveText( pEd, ii, ii + 2, pEd->buffer_size - ii - 2 );

            pEd->begin[ ii + 0 ] = '\r';
            pEd->begin[ ii + 1 ] = '\n';
            if( pEd->last_line == pEd->current_line )
               pEd->last_line = Next( pEd, pEd->current_line );
         }

         if( pEd->cursor_row < ( pEd->bottom - pEd->top ) )
         {
            j = Prev( pEd, pEd->last_display );
            if( j > 0 )
               pEd->last_display = j;

            pEd->next_stabil    = pEd->current_line;
            pEd->stabil         = pEd->bottom - pEd->top + 1 - pEd->cursor_row;
            pEd->current_stabil = pEd->cursor_row;
         }
         else
         {
            pEd->next_stabil    = pEd->first_display;
            pEd->stabil         = pEd->bottom - pEd->top + 1;
            pEd->current_stabil = 0;
         }
      }
   }
   else
   {
      Clear( pEd, pEd->current_line, &nEsc );
      j = Next( pEd, pEd->current_line );
      if( j > pEd->last_line )
      {
         if( Check_length( pEd, 2 ) )
         {
            pEd->line_number++;
            pEd->last_line      = j;
            pEd->begin[ j + 0 ] = '\r';
            pEd->begin[ j + 1 ] = '\n';
            pEd->begin[ j + 2 ] = '\0';
         }
      }
   }

   if( Check_length( pEd, 0 ) )
   {
      pEd->fStable = HB_FALSE;
      pEd->dir     = _STABILIZE_DOWN;
      Down( pEd );
      Home( pEd );
   }

   if( ! pEd->fStable )
   {
      pEd->next_stabil    = pEd->first_display;
      pEd->stabil         = pEd->bottom - pEd->top + 1;
      pEd->current_stabil = 0;
      pEd->dir = _STABILIZE_DOWN;
   }
}


/* Insert the CRLF characters */
HB_FUNC( ED_RETURN )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      Return( pEd, hb_parl( 2 ) /* fInsert */ );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Returns the current cursor row inside the editor's window */
HB_FUNC( ED_WINROW )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retni( pEd->cursor_row );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Returns the line number where the cursor is positioned */
HB_FUNC( ED_ROW )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->active );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Return the current cursor column inside the editor's window */
HB_FUNC( ED_WINCOL )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->cursor_col );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Returns the current cursor position inside the line */
HB_FUNC( ED_COL )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->cursor_col + pEd->first_col + 1 );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Returns the total number of lines */
HB_FUNC( ED_MAXLINE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->line_number );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Counts the total number of lines in passed editor */
HB_FUNC( ED_LCOUNT )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->line_number );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Returns if the editor is correctly displayed */
HB_FUNC( ED_STABLE )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retl( pEd->fStable );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* Returns the number of bytes stored in the text buffer */
HB_FUNC( ED_LENGTH )
{
   PHB_EDITOR pEd = PHB_EDITOR_par( 1 );

   if( pEd )
      hb_retns( pEd->text_length );
   else
      hb_errRT_BASE( EG_ARG, 3001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
