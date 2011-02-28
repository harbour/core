/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

/* *********************************************************************** */

#include "gtsln.h"

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

static HB_FHANDLE s_hStdIn, s_hStdOut, s_hStdErr;
static HB_BOOL s_fStdInTTY = HB_FALSE, s_fStdOutTTY = HB_FALSE, s_fStdErrTTY = HB_FALSE;

/* does terminal works in Unicode (UTF-8) mode? */
HB_BOOL hb_sln_Is_Unicode = HB_FALSE;

/* Slang color names */
static const char * s_colorNames[] =
{
    "black"         ,
    "blue"          ,
    "green"         ,
    "cyan"          ,
    "red"           ,
    "magenta"       ,
    "brown"         ,
    "lightgray"     ,

    "gray"          ,
    "brightblue"    ,
    "brightgreen"   ,
    "brightcyan"    ,
    "brightred"     ,
    "brightmagenta" ,
    "yellow"        ,
    "white"
};

/* to convert Clipper colors into Slang ones */
#ifdef HB_SLN_UTF8
static SLsmg_Color_Type s_colorTab[ 256 ];
#else
static SLsmg_Char_Type s_colorTab[ 256 ];
#endif
/* to convert displayed characters */
static SLsmg_Char_Type s_outputTab[ 256 ];

/* to convert box displayed characters */
static SLsmg_Char_Type s_outboxTab[ 256 ];

/* to convert input characters */
unsigned char hb_sln_inputTab[ 256 ];
PHB_CODEPAGE hb_sln_cdpIN;

static HB_BOOL s_fActive = HB_FALSE;

static int  s_iCursorStyle = SC_NORMAL;

/* indicate if we are currently running a command from system */
static HB_BOOL s_bSuspended = HB_FALSE;

/* the name of an environmet variable containig a definition of nation chars.*/
/* A definition is a list of pairs of chars. The first char in each pair is  */
/* an ASCII key, which should be pressed *after* a "DeadKey" was pressed to  */
/* get the nation char, a second in that pair is a corresponding nation char */
static const char * hb_NationCharsEnvName = "HRBNATIONCHARS";

/* *********************************************************************** */

/* *********************************************************************** */

volatile HB_BOOL hb_sln_bScreen_Size_Changed = HB_FALSE;

/* window's resize handler */
static void sigwinch_handler( int iSig )
{
   HB_SYMBOL_UNUSED( iSig );

   hb_sln_bScreen_Size_Changed = HB_TRUE;
   SLsignal( SIGWINCH, sigwinch_handler );
}

/* *********************************************************************** */

static void hb_sln_colorTrans( void )
{
   int i, clr, fg, bg;

   for( i = 0; i < 256; i++ )
   {
      fg = ( i & 0x0F );
      /*
       * bit 7 is a blinking attribute - not used when console is not in
       * UTF-8 mode because we are using it for changing into ACSC
       * In SLANG 2.0 the character attributes are hold in HB_USHORT not HB_BYTE
       * so we can use all colors, blinking bit and ACSC switch without
       * any problems also when console is not in UTF-8 mode.
       */
#ifdef HB_SLN_UTF8 /* slang 2.0 */
      bg = ( i >> 4 ) & 0x0F;
#else
      bg = ( i >> 4 ) & ( hb_sln_Is_Unicode ? 0x0F : 0x07 );
#endif
      /*
       * in Clipper default color i 0x07 when in Slang 0x00,
       * we make a small trick with XOR 7 to make default colors
       * the same.
       */
      clr = ( bg << 4 ) | ( fg ^ 0x07 );
      SLtt_set_color( clr, ( char * ) NULL, ( char * ) s_colorNames[ fg ],
                                            ( char * ) s_colorNames[ bg ] );
#ifdef HB_SLN_UTF8
      s_colorTab[ i ] = clr;
#else
      HB_SLN_BUILD_RAWCHAR( s_colorTab[ i ], 0, clr );
#endif
   }
}

/* *********************************************************************** */

static void hb_sln_setSingleBox( void )
{
   /* convert all box chars into Clipper _B_SINBLE */
   s_outputTab[ 186 ] = s_outputTab[ 179 ];
   s_outputTab[ 205 ] = s_outputTab[ 196 ];

   s_outputTab[ 181 ] = s_outputTab[ 180 ];
   s_outputTab[ 182 ] = s_outputTab[ 180 ];
   s_outputTab[ 185 ] = s_outputTab[ 180 ];

   s_outputTab[ 183 ] = s_outputTab[ 191 ];
   s_outputTab[ 184 ] = s_outputTab[ 191 ];
   s_outputTab[ 187 ] = s_outputTab[ 191 ];

   s_outputTab[ 200 ] = s_outputTab[ 192 ];
   s_outputTab[ 211 ] = s_outputTab[ 192 ];
   s_outputTab[ 212 ] = s_outputTab[ 192 ];

   s_outputTab[ 202 ] = s_outputTab[ 193 ];
   s_outputTab[ 207 ] = s_outputTab[ 193 ];
   s_outputTab[ 208 ] = s_outputTab[ 193 ];

   s_outputTab[ 203 ] = s_outputTab[ 194 ];
   s_outputTab[ 209 ] = s_outputTab[ 194 ];
   s_outputTab[ 210 ] = s_outputTab[ 194 ];

   s_outputTab[ 198 ] = s_outputTab[ 195 ];
   s_outputTab[ 199 ] = s_outputTab[ 195 ];
   s_outputTab[ 204 ] = s_outputTab[ 195 ];

   s_outputTab[ 206 ] = s_outputTab[ 197 ];
   s_outputTab[ 215 ] = s_outputTab[ 197 ];
   s_outputTab[ 216 ] = s_outputTab[ 197 ];

   s_outputTab[ 188 ] = s_outputTab[ 217 ];
   s_outputTab[ 189 ] = s_outputTab[ 217 ];
   s_outputTab[ 190 ] = s_outputTab[ 217 ];

   s_outputTab[ 201 ] = s_outputTab[ 218 ];
   s_outputTab[ 213 ] = s_outputTab[ 218 ];
   s_outputTab[ 214 ] = s_outputTab[ 218 ];
}

/* *********************************************************************** */

static void hb_sln_setACSCtrans( void )
{
   unsigned char * p, ch;
   SLsmg_Char_Type chBoard[3], chArrow[4];

   memset( &chArrow, 0, sizeof( chArrow ) );
   memset( &chBoard, 0, sizeof( chBoard ) );

   HB_SLN_BUILD_RAWCHAR( chBoard[ 0 ], 0, 0 );
   HB_SLN_BUILD_RAWCHAR( chBoard[ 1 ], 0, 0 );
   HB_SLN_BUILD_RAWCHAR( chBoard[ 2 ], 0, 0 );

   HB_SLN_BUILD_RAWCHAR( chArrow[ 0 ], '<', 0 );
   HB_SLN_BUILD_RAWCHAR( chArrow[ 1 ], '>', 0 );
   HB_SLN_BUILD_RAWCHAR( chArrow[ 2 ], 'v', 0 );
   HB_SLN_BUILD_RAWCHAR( chArrow[ 3 ], '^', 0 );

   /* init an alternate chars table */
   if( ( p = ( unsigned char * ) SLtt_Graphics_Char_Pairs ) )
   {
      SLsmg_Char_Type SLch;
      int i, len = strlen( ( char * ) p );

      memset( &SLch, 0, sizeof( SLsmg_Char_Type ) );
      for( i = 0; i < len; i += 2 )
      {
         ch = *p++;
         HB_SLN_BUILD_RAWCHAR( SLch, *p++, 0 );
         HB_SLN_SET_ACSC( SLch );
         switch( ch )
         {
#ifdef HB_SLN_UNICODE
            case SLSMG_HLINE_CHAR_TERM   :   s_outputTab[ 196 ] = SLch; break;
            case SLSMG_VLINE_CHAR_TERM   :   s_outputTab[ 179 ] = SLch; break;
            case SLSMG_ULCORN_CHAR_TERM  :   s_outputTab[ 218 ] = SLch; break;
            case SLSMG_URCORN_CHAR_TERM  :   s_outputTab[ 191 ] = SLch; break;
            case SLSMG_LLCORN_CHAR_TERM  :   s_outputTab[ 192 ] = SLch; break;
            case SLSMG_LRCORN_CHAR_TERM  :   s_outputTab[ 217 ] = SLch; break;
            case SLSMG_RTEE_CHAR_TERM    :   s_outputTab[ 180 ] = SLch; break;
            case SLSMG_LTEE_CHAR_TERM    :   s_outputTab[ 195 ] = SLch; break;
            case SLSMG_UTEE_CHAR_TERM    :   s_outputTab[ 194 ] = SLch; break;
            case SLSMG_DTEE_CHAR_TERM    :   s_outputTab[ 193 ] = SLch; break;
            case SLSMG_PLUS_CHAR_TERM    :   s_outputTab[ 197 ] = SLch; break;
/*
            case SLSMG_DEGREE_CHAR_TERM  :   s_outputTab[    ] = SLch; break;
            case SLSMG_PLMINUS_CHAR_TERM :   s_outputTab[    ] = SLch; break;
            case SLSMG_BULLET_CHAR_TERM  :   s_outputTab[    ] = SLch; break;
*/
            case SLSMG_DIAMOND_CHAR_TERM :   s_outputTab[ 04 ] = SLch; break;
            case SLSMG_LARROW_CHAR_TERM  :   chArrow[ 0 ] = SLch; break;
            case SLSMG_RARROW_CHAR_TERM  :   chArrow[ 1 ] = SLch; break;
            case SLSMG_DARROW_CHAR_TERM  :   chArrow[ 2 ] = SLch; break;
            case SLSMG_UARROW_CHAR_TERM  :   chArrow[ 3 ] = SLch; break;
            case SLSMG_BOARD_CHAR_TERM   :   chBoard[ 0 ] = SLch; break;
            case SLSMG_CKBRD_CHAR_TERM   :   chBoard[ 1 ] = SLch; break;
            case SLSMG_BLOCK_CHAR_TERM   :   chBoard[ 2 ] = SLch; break;
#else
            case SLSMG_HLINE_CHAR   :   s_outputTab[ 196 ] = SLch; break;
            case SLSMG_VLINE_CHAR   :   s_outputTab[ 179 ] = SLch; break;
            case SLSMG_ULCORN_CHAR  :   s_outputTab[ 218 ] = SLch; break;
            case SLSMG_URCORN_CHAR  :   s_outputTab[ 191 ] = SLch; break;
            case SLSMG_LLCORN_CHAR  :   s_outputTab[ 192 ] = SLch; break;
            case SLSMG_LRCORN_CHAR  :   s_outputTab[ 217 ] = SLch; break;
            case SLSMG_RTEE_CHAR    :   s_outputTab[ 180 ] = SLch; break;
            case SLSMG_LTEE_CHAR    :   s_outputTab[ 195 ] = SLch; break;
            case SLSMG_UTEE_CHAR    :   s_outputTab[ 194 ] = SLch; break;
            case SLSMG_DTEE_CHAR    :   s_outputTab[ 193 ] = SLch; break;
            case SLSMG_PLUS_CHAR    :   s_outputTab[ 197 ] = SLch; break;
/*
            case SLSMG_DEGREE_CHAR; :   s_outputTab[    ] = SLch; break;
            case SLSMG_PLMINUS_CHAR :   s_outputTab[    ] = SLch; break;
            case SLSMG_BULLET_CHAR  :   s_outputTab[    ] = SLch; break;
*/
            case SLSMG_DIAMOND_CHAR :   s_outputTab[ 04 ] = SLch; break;
            case SLSMG_LARROW_CHAR  :   chArrow[ 0 ] = SLch; break;
            case SLSMG_RARROW_CHAR  :   chArrow[ 1 ] = SLch; break;
            case SLSMG_DARROW_CHAR  :   chArrow[ 2 ] = SLch; break;
            case SLSMG_UARROW_CHAR  :   chArrow[ 3 ] = SLch; break;
            case SLSMG_BOARD_CHAR   :   chBoard[ 0 ] = SLch; break;
            case SLSMG_CKBRD_CHAR   :   chBoard[ 1 ] = SLch; break;
            case SLSMG_BLOCK_CHAR   :   chBoard[ 2 ] = SLch; break;
#endif
         }
      }

      HB_SLN_BUILD_RAWCHAR( SLch, 0, 0 );
      for( i = 0; i < 3 && !HB_SLN_IS_CHAR( SLch ); i++ )
         SLch = chBoard[ i ];
      if( !HB_SLN_IS_CHAR( SLch ) )
         HB_SLN_BUILD_RAWCHAR( SLch, '#', 0 );
      for( i = 0; i < 3; i++ )
      {
         if( !HB_SLN_IS_CHAR( chBoard[ i ] ) )
            chBoard[ i ] = SLch;
      }
      s_outputTab[ 176 ] = chBoard[ 0 ];
      s_outputTab[ 177 ] = chBoard[ 1 ];
      s_outputTab[ 178 ] = chBoard[ 2 ];
      s_outputTab[ 219 ] = chBoard[ 2 ];

      s_outputTab[ 17 ] = s_outputTab[ 27 ] = chArrow[ 0 ];
      s_outputTab[ 16 ] = s_outputTab[ 26 ] = chArrow[ 1 ];
      s_outputTab[ 25 ] = s_outputTab[ 31 ] = chArrow[ 2 ];
      s_outputTab[ 24 ] = s_outputTab[ 30 ] = chArrow[ 3 ];

#ifdef HB_SLN_UNICODE
      /*
       * There is a bug in SLANG lib patched for UTF-8 support
       * SLSMG_UTEE_CHAR_TERM is reverted with SLSMG_DTEE_CHAR_TERM
       * They should be mapped:
       *    SLSMG_UTEE_CHAR_TERM = 'w'
       *    SLSMG_DTEE_CHAR_TERM = 'v'
       * Below it's a hack for this version of slang which fix the
       * problem.
       */
      if( SLSMG_UTEE_CHAR_TERM == 'v' )
      {
         SLch = s_outputTab[ 193 ];
         s_outputTab[ 193 ] = s_outputTab[ 194 ];
         s_outputTab[ 194 ] = SLch;
      }
#endif
   }
}

/* *********************************************************************** */

static void hb_sln_setCharTrans( PHB_CODEPAGE cdpHost, PHB_CODEPAGE cdpTerm, HB_BOOL fBox )
{
   int i, iDst;

#if !defined( HB_SLN_UNICODE )
   HB_SYMBOL_UNUSED( cdpTerm );
#endif

   /* build a conversion chars table */
   for( i = 0; i < 256; i++ )
   {
      if( hb_sln_Is_Unicode )
         iDst = hb_cdpGetU16( cdpHost, HB_TRUE, ( HB_BYTE ) i );
      else
         iDst = i;

      if( iDst < 32 )
         /* under Unix control-chars are not visible in a general meaning */
         HB_SLN_BUILD_RAWCHAR( s_outputTab[ i ], '.', 0 );
      else if( ! hb_sln_Is_Unicode && i >= 128 )
      {
         HB_SLN_BUILD_RAWCHAR( s_outputTab[ i ], iDst, 0 );
         HB_SLN_SET_ACSC( s_outputTab[ i ] );
      }
      else
         HB_SLN_BUILD_RAWCHAR( s_outputTab[ i ], iDst, 0 );
      s_outboxTab[ i ] = s_outputTab[ i ];
   }


   if( ! hb_sln_Is_Unicode )
   {
      hb_sln_setACSCtrans();

      /* QUESTION: do we have double, single-double, ... frames under xterm ? */
      if( hb_sln_UnderXterm )
         hb_sln_setSingleBox();

      memcpy( s_outboxTab, s_outputTab, sizeof( s_outputTab ) );

      if( cdpHost )
      {
         for( i = 0; i < 256; ++i )
         {
            if( hb_cdpIsAlpha( cdpHost, i ) )
            {
#ifdef HB_SLN_UNICODE
               iDst = hb_cdpGetU16( cdpHost, HB_TRUE, ( HB_BYTE ) i );
#else
               if( hb_sln_Is_Unicode )
                  iDst = hb_cdpGetU16( cdpHost, HB_TRUE, ( HB_BYTE ) i );
               else
                  iDst = hb_cdpTranslateChar( i, HB_TRUE, cdpHost, cdpTerm );
#endif
               HB_SLN_BUILD_RAWCHAR( s_outputTab[ i ], iDst, 0 );
               if( fBox )
                  s_outboxTab[ i ] = s_outputTab[ i ];
            }
         }
      }
   }
}

/* *********************************************************************** */
static void hb_sln_setKeyTrans( PHB_CODEPAGE cdpHost, PHB_CODEPAGE cdpTerm )
{
   char *p;
   int i;

   for( i = 0; i < 256; i++ )
      hb_sln_inputTab[ i ] = ( unsigned char )
                           hb_cdpTranslateChar( i, HB_FALSE, cdpTerm, cdpHost );
   hb_sln_cdpIN = cdpTerm ? cdpTerm : cdpHost;

   /* init national chars */
   p = hb_getenv( hb_NationCharsEnvName );
   if( p )
   {
      int len = strlen( p ) >> 1, ch;

      /* no more than 128 National chars are allowed */
      if( len > 128 ) len = 128;

      /* the first element contains a number of Dead keys defined in an ENVAR */
      hb_sln_convKDeadKeys[ 0 ] = ( unsigned char ) len;

      len <<= 1;
      for( i = 0; i < len; i += 2 )
      {
         ch = ( unsigned char ) p[ i + 1 ];
         hb_sln_convKDeadKeys[ i + 1 ] = ( unsigned char ) p[ i ];
         hb_sln_convKDeadKeys[ i + 2 ] = ch;
         hb_sln_inputTab[ ch ] = ch;
      }
      hb_xfree( ( void * ) p );
   }
}

/* *********************************************************************** */

static void hb_sln_SetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_sln_SetCursorStyle(%d)", iStyle));

   if( s_iCursorStyle == SC_UNAVAIL )
      return;

   if( s_iCursorStyle >= SC_NONE && s_iCursorStyle <= SC_SPECIAL2 )
   {
      SLtt_set_cursor_visibility( iStyle != SC_NONE );

      /* NOTE: cursor apearence works only under linux console */
      if( hb_sln_UnderLinuxConsole && s_iCursorStyle != iStyle )
      {
         /* keyseq to define cursor shape under linux console */
         char cursDefseq[] = { 27, '[', '?', '1', 'c', 0 };

         switch( iStyle )
         {
            case SC_NONE:
               cursDefseq[ 3 ] = '1';
               break;

            case SC_NORMAL:
               cursDefseq[ 3 ] = '2';
               break;

            case SC_INSERT:
               cursDefseq[ 3 ] = '4';
               break;

            case SC_SPECIAL1:
               cursDefseq[ 3 ] = '8';
               break;

            case SC_SPECIAL2:
               /* TODO: find a proper sequqnce to set a cursor
                  to SC_SPECIAL2 under Linux console  */
               cursDefseq[ 3 ] = '4';
               break;
         }
         SLtt_write_string( cursDefseq );
      }
      s_iCursorStyle = iStyle;
   }
}

/* *********************************************************************** */
#ifdef HB_SLN_UTF8
static int hb_sln_isUTF8( int iStdOut, int iStdIn )
{
   if( isatty( iStdOut ) && isatty( iStdIn ) )
   {
      const char * szBuf = "\r\303\255\033[6n  \r";
      int len = strlen( szBuf );

      if( write( iStdOut, szBuf, len ) == len )
      {
         char rdbuf[ 64 ];
         int i, j, n, d, y, x;
         HB_MAXUINT end_timer, time;

         n = j = x = y = 0;
         /* wait up to 2 seconds for answer */
         end_timer = hb_dateMilliSeconds() + 2000;
         for( ; ; )
         {
            /* loking for cursor position in "\033[%d;%dR" */
            while( j < n && rdbuf[ j ] != '\033' )
               ++j;
            if( n - j >= 6 )
            {
               i = j + 1;
               if( rdbuf[ i ] == '[' )
               {
                  y = 0;
                  d = ++i;
                  while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                     y = y * 10 + ( rdbuf[ i++ ] - '0' );
                  if( i < n && i > d && rdbuf[ i ] == ';' )
                  {
                     x = 0;
                     d = ++i;
                     while( i < n && rdbuf[ i ] >= '0' && rdbuf[ i ] <= '9' )
                        x = x * 10 + ( rdbuf[ i++ ] - '0' );
                     if( i < n && i > d && rdbuf[ i ] == 'R' )
                     {
                        return x == 2 ? 1 : 0;
                     }
                  }
               }
               if( i < n )
               {
                  j = i;
                  continue;
               }
            }
            if( n == sizeof( rdbuf ) )
               break;
            time = hb_dateMilliSeconds();
            if( time > end_timer )
               break;
            else
            {
               struct timeval tv;
               fd_set rdfds;
               int iMilliSec;

               FD_ZERO( &rdfds );
               FD_SET( iStdIn, &rdfds );
               iMilliSec = ( int ) ( end_timer - time );
               tv.tv_sec = iMilliSec / 1000;
               tv.tv_usec = ( iMilliSec % 1000 ) * 1000;

               if( select( iStdIn + 1, &rdfds, NULL, NULL, &tv ) <= 0 )
                  break;
               i = read( iStdIn, rdbuf + n, sizeof( rdbuf ) - n );
               if( i <= 0 )
                  break;
               n += i;
            }
         }
      }
   }
   return -1;
}
#endif
/* *********************************************************************** */

/* I think this function should not be void. It should be HB_BOOL */
static void hb_gt_sln_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_BOOL gt_Inited = HB_FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr));

   /* stdin && stdout && stderr */
   s_hStdIn  = hFilenoStdin;
   s_hStdOut = hFilenoStdout;
   s_hStdErr = hFilenoStderr;

   s_fStdInTTY  = isatty( s_hStdIn );
   s_fStdOutTTY = isatty( s_hStdOut );
   s_fStdErrTTY = isatty( s_hStdErr );

   /* Slang file descriptors */
   SLang_TT_Read_FD  = -1;
   SLang_TT_Write_FD = -1;

   /* read a terminal descripion from a terminfo database */
   SLtt_get_terminfo();

   /* initialize higher-level Slang routines */
   if( SLkp_init() != -1 )
   {
      /* initialize a terminal stuff and a Slang
         keyboard subsystem for the first time */
      if( hb_sln_Init_Terminal( 0 ) )
      {
         /* fix an OutStd()/OutErr() output */
         if( !isatty( hFilenoStdout ) )
             SLang_TT_Write_FD = SLang_TT_Read_FD;

#ifdef HB_SLN_UTF8
         hb_sln_Is_Unicode = SLutf8_enable(
                        hb_sln_isUTF8( SLang_TT_Write_FD, SLang_TT_Read_FD ) );
#endif
#ifdef HB_SLN_UNICODE
            /* SLsmg_Setlocale = 0; */
#endif
            /* initialize a screen handling subsytem */
         if( SLsmg_init_smg() != -1 )
         {
            /* install window resize handler */
            SLsignal( SIGWINCH, sigwinch_handler );

            /* do not indicate USER_BREAK in SLang_Error - ??? */
            SLang_Ignore_User_Abort = 1;

            /* no default abort procesing */
            SLang_set_abort_signal( NULL );

            /* NOTE: this is incompatible with CLIPPER
               but under Unix we should assume cursor is
               visible on startup because we cannot figure
               out a current cursor state */

            /* turn on a cursor visibility */
            if( SLtt_set_cursor_visibility( 1 ) == -1 )
                s_iCursorStyle = SC_UNAVAIL;

            /* NOTE: this driver is implemented in a way that it is
               imposible to get intensity/blinking background mode.
               The reason is the way Slang is written.
               This is incompatible with Clipper.
               But when the console is in UTF-8 mode we don't need
               to switch into ACSC because we can display all supported
               characters using it's UNICODE values so we can use
               blink bit as in Clipper.
               In SLANG 2.0 the character attributes are hold in HB_USHORT
               not HB_BYTE so we can use all colors, blinking bit and ACSC
               switch without any problems also when console is not in
               UTF-8 mode.
             */
#ifdef HB_SLN_UTF8
            SLtt_Blink_Mode = 1;
            SLtt_Use_Blink_For_ACS = 0;
#else
#  ifdef HB_SLN_UNICODE
            hb_sln_Is_Unicode = SLsmg_Is_Unicode;
#  endif
            if( hb_sln_Is_Unicode )
            {
               SLtt_Blink_Mode = 1;
               SLtt_Use_Blink_For_ACS = 1;
            }
            else
            {
               SLtt_Blink_Mode = 0;
               SLtt_Use_Blink_For_ACS = 0;
            }
#endif
            SLsmg_Display_Eight_Bit = 128;
            SLsmg_Newline_Behavior = SLSMG_NEWLINE_SCROLLS;

            /* initialize conversion tables */
            hb_sln_colorTrans();
            hb_sln_setCharTrans( hb_vmCDP(), NULL, HB_TRUE );
            hb_sln_setKeyTrans( hb_vmCDP(), NULL );

            /* ensure we are in a normal chars set */
            SLtt_set_alt_char_set( 0 );

             /* set the normal Slang color */
            SLsmg_set_color( 0 );

            /* NOTE: due to a work of a Slang library which does not
               prepare its internal screen buffer properly, a screen
               must be cleared before normal work. This is not
               compatible with Clipper */
            SLsmg_cls();
            SLsmg_gotorc( 0, 0 );
            SLsmg_refresh();

            gt_Inited = HB_TRUE;
         }
      }
   }

   if( ! gt_Inited )
   {
      /* something went wrong - restore default settings */
      SLang_reset_tty();
      hb_errInternal( 9997, "Internal error: screen driver initialization failure", NULL, NULL );
   }

   s_fActive = HB_TRUE;
   hb_gt_sln_mouse_Init();
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, SLtt_Screen_Rows, SLtt_Screen_Cols );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_COMPATBUFFER, HB_FALSE );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDOUTCON, s_fStdOutTTY );
   HB_GTSELF_SETFLAG( pGT, HB_GTI_STDERRCON, s_fStdErrTTY );

   HB_GTSELF_SETBLINK( pGT, HB_TRUE );
   HB_GTSELF_SETPOS( pGT, SLsmg_get_row(), SLsmg_get_column() );
}

/* *********************************************************************** */

static void hb_gt_sln_Exit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_Exit(%p)", pGT));

   /* restore a standard bell frequency and duration */
   if( hb_sln_UnderLinuxConsole )
   {
      SLtt_write_string( ( char * ) "\033[10]" );
      SLtt_write_string( ( char * ) "\033[11]" );
      SLtt_flush_output();
   }

   HB_GTSELF_REFRESH( pGT );
   hb_gt_sln_mouse_Exit();
   /* NOTE: This is incompatible with Clipper - on exit leave a cursor visible */
   hb_sln_SetCursorStyle( SC_NORMAL );

   SLsmg_refresh();
   SLsmg_reset_smg();
   SLang_reset_tty();

   s_fStdInTTY = s_fStdOutTTY = s_fStdErrTTY = s_fActive = HB_FALSE;

   HB_GTSUPER_EXIT( pGT );
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_SetMode(%p,%d,%d)", pGT, iRows, iCols));

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   /* TODO: How to change the size of the screen? */
   return HB_FALSE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_IsColor( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_IsColor(%p)", pGT));

   HB_SYMBOL_UNUSED( pGT );

   return SLtt_Use_Ansi_Colors;
}

/* *********************************************************************** */

static void hb_gt_sln_SetBlink( PHB_GT pGT, HB_BOOL fBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_SetBlink(%p,%d)", pGT, (int) fBlink));

   /*
    * We cannot switch remote terminal between blinking and highlight mode
    * for server side using standard termcap/terminfo codes - few rather
    * exotic terminals have such capabilities but this are non standard
    * extensions which can be hard coded only for given hardware (or
    * software terminal emulator). I think that if it's necessary then
    * user should add such tricks yourself to his programs using
    * outstd( <cBlinkSequence> )
    * The only one thing I can make in portable way which will always
    * work is disabling sending BLINK attribute to remote terminal. So
    * in GTSLN like in GTCRS the function SetBlink( .f. ) does it, [Druzus]
    */

   SLtt_Blink_Mode = fBlink ? 1 : 0;
   HB_GTSUPER_SETBLINK( pGT, fBlink );
}

/* *********************************************************************** */

static void hb_gt_sln_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_sln_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

   /* TODO: Implement this for other consoles than linux ? */

   HB_SYMBOL_UNUSED( pGT );

   if( hb_sln_UnderLinuxConsole )
   {
      char escstr[ 64 ];

      dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
      hb_snprintf( escstr, 63, "\033[10;%d]", ( int ) dFrequency );
      SLtt_write_string( escstr );
      hb_snprintf( escstr, 63, "\033[11;%d]", ( int ) ( dDuration * 1000.0 / 18.2 ) );
      SLtt_write_string( escstr );
      SLtt_flush_output();
   }
   else
   {
      SLtt_beep();
   }

   if( hb_sln_UnderLinuxConsole )
   {
      /* The conversion from Clipper (DOS) timer tick units to
         milliseconds is * 1000.0 / 18.2. */
      dDuration /= 18.2;
      hb_idleSleep( dDuration );
   }
}

/* *********************************************************************** */

static const char * hb_gt_sln_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_sln_Version(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Slang";
}

/* *********************************************************************** */

/* NOTE: these two are for prepare Slang to temporary
   finish its work. They should be called from run.c. */

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_Suspend( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( ! s_bSuspended )
   {
      if( SLsmg_suspend_smg() != -1 )
      {
         SLang_reset_tty();
         s_bSuspended = HB_TRUE;
      }
   }

   return s_bSuspended;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_Resume( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_bSuspended && SLsmg_resume_smg() != -1 &&
       hb_sln_Init_Terminal( 1 ) != -1 )
   {
      SLsmg_refresh(); /* reinitialize a terminal */
#if defined( HB_HAS_GPM )
      hb_gt_sln_mouse_FixTrash();
#endif
      s_bSuspended = HB_FALSE;
   }

   return !s_bSuspended;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_PreExt( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   SLsmg_refresh();
#if defined( HB_HAS_GPM )
   hb_gt_sln_mouse_FixTrash();
#endif
   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_PostExt( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_sln_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, hb_sln_Is_Unicode );
         break;

      case HB_GTI_ESCDELAY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_sln_escDelay );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_sln_escDelay = hb_itemGetNI( pInfo->pNewVal );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}


/* *********************************************************************** */

static HB_BOOL hb_gt_sln_SetDispCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox )
{
   HB_GTSUPER_SETDISPCP( pGT, pszTermCDP, pszHostCDP, fBox );

   {
      PHB_CODEPAGE cdpTerm = NULL, cdpHost = NULL;

      if( pszHostCDP )
         cdpHost = hb_cdpFind( pszHostCDP );
      if( ! cdpHost )
         cdpHost = hb_vmCDP();

      if( pszTermCDP )
         cdpTerm = hb_cdpFind( pszTermCDP );

      hb_sln_setCharTrans( cdpHost, cdpTerm, fBox );
   }

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_sln_SetKeyCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   HB_GTSUPER_SETKEYCP( pGT, pszTermCDP, pszHostCDP );

   {
      PHB_CODEPAGE cdpTerm = NULL, cdpHost = NULL;

      if( pszHostCDP )
         cdpHost = hb_cdpFind( pszHostCDP );
      if( ! cdpHost )
         cdpHost = hb_vmCDP();

      if( pszTermCDP )
         cdpTerm = hb_cdpFind( pszTermCDP );

      hb_sln_setKeyTrans( cdpHost, cdpTerm );
   }

   return HB_TRUE;
}

/* *********************************************************************** */

static void hb_gt_sln_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_sln_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   if( s_fActive )
   {
      SLsmg_Char_Type SLchar;
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;

      while( iSize-- > 0 )
      {
         if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
            break;
         SLsmg_gotorc( iRow, iCol );
         HB_SLN_BUILD_CHAR( SLchar, usChar & 0xFF, iColor, bAttr );
         SLsmg_write_raw( &SLchar, 1 );
         ++iCol;
      }
   }
}

/* *********************************************************************** */

static void hb_gt_sln_Refresh( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_sln_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );
   if( s_fActive )
   {
      int iRow, iCol, iStyle;

      HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
      if( iStyle != SC_NONE && ( iRow < 0 || iCol < 0 ||
                      iRow >= SLtt_Screen_Rows || iCol >= SLtt_Screen_Cols ) )
         iStyle = SC_NONE;
      SLsmg_gotorc( iRow, iCol );
      hb_sln_SetCursorStyle( iStyle );
      SLsmg_refresh();
   }
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_sln_Init;
   pFuncTable->Exit                       = hb_gt_sln_Exit;
   pFuncTable->IsColor                    = hb_gt_sln_IsColor;
   pFuncTable->SetMode                    = hb_gt_sln_SetMode;
   pFuncTable->Redraw                     = hb_gt_sln_Redraw;
   pFuncTable->Refresh                    = hb_gt_sln_Refresh;
   pFuncTable->SetBlink                   = hb_gt_sln_SetBlink;
   pFuncTable->Version                    = hb_gt_sln_Version;
   pFuncTable->Suspend                    = hb_gt_sln_Suspend;
   pFuncTable->Resume                     = hb_gt_sln_Resume;
   pFuncTable->PreExt                     = hb_gt_sln_PreExt;
   pFuncTable->PostExt                    = hb_gt_sln_PostExt;
   pFuncTable->Tone                       = hb_gt_sln_Tone;
   pFuncTable->Info                       = hb_gt_sln_Info;
   pFuncTable->SetDispCP                  = hb_gt_sln_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_sln_SetKeyCP;

   pFuncTable->ReadKey                    = hb_gt_sln_ReadKey;

   pFuncTable->MouseIsPresent             = hb_gt_sln_mouse_IsPresent;
   pFuncTable->MouseShow                  = hb_gt_sln_mouse_Show;
   pFuncTable->MouseHide                  = hb_gt_sln_mouse_Hide;
   pFuncTable->MouseGetPos                = hb_gt_sln_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_sln_mouse_SetPos;
   pFuncTable->MouseButtonState           = hb_gt_sln_mouse_ButtonState;
   pFuncTable->MouseCountButton           = hb_gt_sln_mouse_CountButton;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
