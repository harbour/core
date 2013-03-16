/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_strAtI()
 *    tip_TimeStamp() rework
 *    cleanup
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbdate.h"

/************************************************************
 * Useful internet timestamp based on RFC822
 */

HB_FUNC( TIP_TIMESTAMP )
{
   static const char * s_days[]   = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

   char szRet[ 64 ];
   int  iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
   long lOffset = hb_timeUTCOffset();

   if( HB_ISDATE( 1 ) )
   {
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
      iHour = iMinute = iSecond = 0;
   }
   else if( HB_ISDATETIME( 1 ) )
      hb_timeStampUnpack( hb_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
   else
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );

   /* For compatibility */
   if( HB_ISNUM( 2 ) )
   {
      HB_ULONG ulHour = hb_parnl( 2 );

      iHour   = ( int ) ( ulHour / 3600 );
      iMinute = ( int ) ( ( ulHour % 3600 ) / 60 );
      iSecond = ( int ) ( ulHour % 60 );
   }

   hb_snprintf( szRet, sizeof( szRet ), "%s, %d %s %d %02d:%02d:%02d %+03d%02d",
                s_days[ hb_dateDOW( iYear, iMonth, iDay ) - 1 ],
                iDay, s_months[ iMonth - 1 ], iYear,
                iHour, iMinute, iSecond,
                ( int ) ( lOffset / 3600 ),
                ( int ) ( ( lOffset % 3600 ) / 60 ) );

   hb_retc( szRet );
}

/** Detects the mimetype of a given file */

typedef struct tag_mime
{
   HB_ISIZ            pos;       /* Position in stream from which the match begins */
   const char *       pattern;   /* String to match */
   const char *       mime_type; /* Mimetype if complete */
   int                next;      /* following entry to determine a mimetype, relative to current position (or 0) */
   int                alternate; /* alternative entry to determine a mimetype, relative to current position (or 0) */
   short unsigned int flags;     /* flags for confrontation */
} MIME_ENTRY;

#define MIME_FLAG_TRIMSPACES  0x0001
#define MIME_FLAG_TRIMTABS    0x0002
#define MIME_FLAG_CASEINSENS  0x0004
#define MIME_FLAG_CONTINUE    0x0008
#define MIME_TABLE_SIZE       71

static const MIME_ENTRY s_mimeTable[ MIME_TABLE_SIZE ] =
{
   /* Dos/win executable */
   /*  0*/ { 0,  "MZ",                                "application/x-dosexec",         0, 0, 0                                                                },

   /* ELF file */
   /*  1*/ { 0,  "\177ELF",                           NULL,                            1, 0, 0                                                                },
   /*  2*/ { 4,  "\x00",                              NULL,                            3, 1, MIME_FLAG_CONTINUE                                               },
   /*  3*/ { 4,  "\x01",                              NULL,                            2, 1, MIME_FLAG_CONTINUE                                               },
   /*  4*/ { 4,  "\x02",                              NULL,                            1, 0, MIME_FLAG_CONTINUE                                               },
   /*  5*/ { 5,  "\x00",                              NULL,                            2, 1, MIME_FLAG_CONTINUE                                               },
   /*  6*/ { 5,  "\x01",                              NULL,                            1, 0, MIME_FLAG_CONTINUE                                               },
   /*  7*/ { 16, "\x00",                              "application/x-object",          0, 1, MIME_FLAG_CONTINUE                                               },
   /*  8*/ { 16, "\x01",                              "application/x-object",          0, 1, MIME_FLAG_CONTINUE                                               },
   /*  9*/ { 16, "\x02",                              "application/x-executable",      0, 1, MIME_FLAG_CONTINUE                                               },
   /* 10*/ { 16, "\x03",                              "application/x-sharedlib",       0, 1, MIME_FLAG_CONTINUE                                               },
   /* 11*/ { 16, "\x04",                              "application/x-coredump",        0, 0, MIME_FLAG_CONTINUE                                               },

   /* Shell script */
   /* 12*/ { 0,  "#!/bin/sh",                         "application/x-shellscript",     0, 0, 0                                                                },
   /* 13*/ { 0,  "#! /bin/sh",                        "application/x-shellscript",     0, 0, 0                                                                },
   /* 14*/ { 0,  "#!/bin/csh",                        "application/x-shellscript",     0, 0, 0                                                                },
   /* 15*/ { 0,  "#! /bin/csh",                       "application/x-shellscript",     0, 0, 0                                                                },
   /* 16*/ { 0,  "#!/bin/ksh",                        "application/x-shellscript",     0, 0, 0                                                                },
   /* 17*/ { 0,  "#! /bin/ksh",                       "application/x-shellscript",     0, 0, 0                                                                },
   /* 18*/ { 0,  "#!/bin/tcsh",                       "application/x-shellscript",     0, 0, 0                                                                },
   /* 19*/ { 0,  "#! /bin/tcsh",                      "application/x-shellscript",     0, 0, 0                                                                },
   /* 20*/ { 0,  "#!/usr/local/bin/tcsh",             "application/x-shellscript",     0, 0, 0                                                                },
   /* 21*/ { 0,  "#! /usr/local/bin/tcsh",            "application/x-shellscript",     0, 0, 0                                                                },
   /* 22*/ { 0,  "#!/bin/bash",                       "application/x-shellscript",     0, 0, 0                                                                },
   /* 23*/ { 0,  "#! /bin/bash",                      "application/x-shellscript",     0, 0, 0                                                                },
   /* 24*/ { 0,  "#!/usr/local/bin/bash",             "application/x-shellscript",     0, 0, 0                                                                },
   /* 25*/ { 0,  "#! /usr/local/bin/bash",            "application/x-shellscript",     0, 0, 0                                                                },

   /* Java object code*/
   /* 26*/ { 0,  "\xCA\xFE\xBA\xBE",                  "application/java",              0, 0, 0                                                                },

   /* Perl */
   /* 27*/ { 0,  "#!/bin/perl",                       "application/x-perl",            0, 0, 0                                                                },
   /* 28*/ { 0,  "#! /bin/perl",                      "application/x-perl",            0, 0, 0                                                                },
   /* 29*/ { 0,  "eval \"exec /bin/perl",             "application/x-perl",            0, 0, 0                                                                },
   /* 30*/ { 0,  "#!/usr/bin/perl",                   "application/x-perl",            0, 0, 0                                                                },
   /* 31*/ { 0,  "#! /usr/bin/perl",                  "application/x-perl",            0, 0, 0                                                                },
   /* 32*/ { 0,  "eval \"exec /usr/bin/perl",         "application/x-perl",            0, 0, 0                                                                },
   /* 33*/ { 0,  "#!/usr/local/bin/perl",             "application/x-perl",            0, 0, 0                                                                },
   /* 34*/ { 0,  "#! /usr/local/bin/perl",            "application/x-perl",            0, 0, 0                                                                },
   /* 35*/ { 0,  "eval \"exec /usr/local/bin/perl",   "application/x-perl",            0, 0, 0                                                                },

   /* Python */
   /* 36*/ { 0,  "#!/bin/python",                     "application/x-python",          0, 0, 0                                                                },
   /* 37*/ { 0,  "#! /bin/python",                    "application/x-python",          0, 0, 0                                                                },
   /* 38*/ { 0,  "eval \"exec /bin/python",           "application/x-python",          0, 0, 0                                                                },
   /* 39*/ { 0,  "#!/usr/bin/python",                 "application/x-python",          0, 0, 0                                                                },
   /* 40*/ { 0,  "#! /usr/bin/python",                "application/x-python",          0, 0, 0                                                                },
   /* 41*/ { 0,  "eval \"exec /usr/bin/python",       "application/x-python",          0, 0, 0                                                                },
   /* 42*/ { 0,  "#!/usr/local/bin/python",           "application/x-python",          0, 0, 0                                                                },
   /* 43*/ { 0,  "#! /usr/local/bin/python",          "application/x-python",          0, 0, 0                                                                },
   /* 44*/ { 0,  "eval \"exec /usr/local/bin/python", "application/x-python",          0, 0, 0                                                                },

   /* Unix compress (.z) */
   /* 45*/ { 0,  "\x1F\x9D",                          "application/x-compress",        0, 0, 0                                                                },

   /* Unix gzip */
   /* 46*/ { 0,  "\x1F\x8B",                          "application/x-gzip",            0, 0, 0                                                                },

   /* PKzip */
   /* 47 { 0, "PK\x03\x04", "application/x-zip", 0, 0, 0 }, 2010-12-15 support of xlsx/ods */

   /* xml */
   /* 48*/ { 0,  "<?xml",                             "text/xml",                      0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },

   /* html */
   /* 49*/ { 0,  "<html",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* 50*/ { 0,  "<title",                            "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* 51*/ { 0,  "<head",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* 52*/ { 0,  "<body",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* 53*/ { 0,  "<!--",                              "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS                        },
   /* 54*/ { 0,  "<h",                                "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* 55*/ { 0,  "<!",                                "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },

   /* Postscript */
   /* 56*/ { 0,  "%!",                                "application/postscript",        0, 0, 0                                                                },
   /* 57*/ { 0,  "\x04%!",                            "application/postscript",        0, 0, 0                                                                },

   /* PDF */
   /* 58*/ { 0,  "%PDF-",                             "application/pdf",               0, 0, 0                                                                },

   /* DVI */
   /* 59*/ { 0,  "\xF7\x02",                          "application/dvi",               0, 0, 0                                                                },

   /* PNG image */
   /* 60*/ { 0,  "\x89PNG",                           "image/png",                     0, 0, 0                                                                },

   /* XPM image */
   /* 61*/ { 0,  "/* XPM",                            "image/x-xpm",                   0, 0, 0                                                                },

   /* TIFF image */
   /* 62*/ { 0,  "II",                                "image/tiff",                    0, 0, 0                                                                },
   /* 63*/ { 0,  "MM",                                "image/tiff",                    0, 0, 0                                                                },

   /* GIF image */
   /* 64*/ { 0,  "GIF89z",                            "image/x-compressed-gif",        0, 0, 0                                                                },
   /* 65*/ { 0,  "GIF",                               "image/gif",                     0, 0, 0                                                                },

   /* JPEG image */
   /* 66*/ { 0,  "\xFF\xD8",                          "image/jpeg",                    0, 0, 0                                                                },

   /* ICO image */
   /* 67*/ { 2,  "\x01\x00",                          "image/x-icon",                  0, 0, 0                                                                },

   /* OGG file */
   /* 68*/ { 0,  "OggS",                              "application/ogg",               0, 0, 0                                                                },

   /* FLV file */
   /* 69*/ { 0,  "FLV",                               "video/x-flv",                   0, 0, 0                                                                },

   /* SWF compressed file */
   /* 70*/ { 0,  "CWS",                               "application/x-shockwave-flash", 0, 0, 0                                                                },

   /* SWF uncompressed file */
   /* 71*/ { 0,  "FWS",                               "application/x-shockwave-flash", 0, 0, 0                                                                }

};

/* Find mime by extension */

typedef struct tag_mime_ext
{
   const char * pattern;   /* Extension to match */
   const char * mime_type; /* Mimetype if complete */
   HB_USHORT    flags;     /* flags for confrontation */
} EXT_MIME_ENTRY;

#define EXT_MIME_TABLE_SIZE  24

static EXT_MIME_ENTRY s_extMimeTable[ EXT_MIME_TABLE_SIZE ] =
{
   /* Dos/win executable */
   /*  0*/ { "EXE",   "application/x-dosexec",                                             MIME_FLAG_CASEINSENS },

   /* HTML file */
   /*  1*/ { "HTM",   "text/html",                                                         MIME_FLAG_CASEINSENS },
   /*  2*/ { "HTML",  "text/html",                                                         MIME_FLAG_CASEINSENS },

   /* XLS file */
   /*  3*/ { "xls",   "application/vnd.ms-excel",                                          MIME_FLAG_CASEINSENS },

   /* XML file */
   /*  4*/ { "XML",   "text/xml",                                                          MIME_FLAG_CASEINSENS },

   /* text file */
   /*  5*/ { "TXT",   "text/txt",                                                          MIME_FLAG_CASEINSENS },

   /* PDF file */
   /*  6*/ { "pdf",   "application/pdf",                                                   MIME_FLAG_CASEINSENS },

   /* PS file */
   /*  7*/ { "ps",    "application/postscript",                                            MIME_FLAG_CASEINSENS },

   /* C source */
   /*  8*/ { "c",     "text/x-c",                                                          MIME_FLAG_CASEINSENS },
   /*  9*/ { "c++",   "text/x-c++",                                                        MIME_FLAG_CASEINSENS },
   /* 10*/ { "cpp",   "text/x-c++",                                                        MIME_FLAG_CASEINSENS },
   /* 11*/ { "cxx",   "text/x-c++",                                                        MIME_FLAG_CASEINSENS },
   /* 12*/ { "h",     "text/x-c-header",                                                   MIME_FLAG_CASEINSENS },
   /* 13*/ { "hpp",   "text/x-c++-header",                                                 MIME_FLAG_CASEINSENS },
   /* 14*/ { "hxx",   "text/x-c++-header",                                                 MIME_FLAG_CASEINSENS },

   /* Java */
   /* 15*/ { "class", "application/java",                                                  0                    }, /* case sensitive! */
   /* 16*/ { "java",  "text/java",                                                         0                    },

   /* RTF file */
   /* 17*/ { "rtf",   "application/rtf",                                                   MIME_FLAG_CASEINSENS },

   /* CSV file */
   /* 18*/ { "csv",   "text/csv",                                                          MIME_FLAG_CASEINSENS },

   /* CSS file */
   /* 19*/ { "css",   "text/css",                                                          MIME_FLAG_CASEINSENS },

   /* JS file */
   /* 20*/ { "js",    "application/x-javascript",                                          MIME_FLAG_CASEINSENS },

   /* ODS file */
   /* 21*/ { "ods",   "application/vnd.oasis.opendocument.spreadsheet",                    MIME_FLAG_CASEINSENS },

   /* XLSX file */
   /* 22*/ { "xlsx",  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", MIME_FLAG_CASEINSENS },

   /* ZIP file */
   /* 23*/ { "zip",   "application/x-zip",                                                 MIME_FLAG_CASEINSENS }

};

static const char * s_findExtMimeType( const char * cExt )
{
   int iCount;

   for( iCount = 0; iCount < EXT_MIME_TABLE_SIZE; iCount++ )
   {
      if( s_extMimeTable[ iCount ].flags == MIME_FLAG_CASEINSENS )
      {
         if( hb_stricmp( cExt, s_extMimeTable[ iCount ].pattern ) == 0 )
            return s_extMimeTable[ iCount ].mime_type;
      }
      else
      {
         if( strcmp( cExt, s_extMimeTable[ iCount ].pattern ) == 0 )
            return s_extMimeTable[ iCount ].mime_type;
      }
   }

   return NULL;
}

static const char * s_findMimeStringInTree( const char * cData, HB_ISIZ nLen, int iElem )
{
   const MIME_ENTRY * elem = s_mimeTable + iElem;
   HB_ISIZ nPos     = elem->pos;
   HB_ISIZ nDataLen = strlen( elem->pattern );

   /* allow \0 to be used for matches */
   if( nDataLen == 0 )
      nDataLen = 1;

   /* trim spaces if required */
   while( nPos < nLen &&
          ( ( ( elem->flags & MIME_FLAG_TRIMSPACES ) == MIME_FLAG_TRIMSPACES && (
                 cData[ nPos ] == ' ' || cData[ nPos ] == '\r' || cData[ nPos ] == '\n' ) ) ||
            ( ( elem->flags & MIME_FLAG_TRIMTABS ) == MIME_FLAG_TRIMSPACES && cData[ nPos ] == '\t' ) ) )
   {
      nPos++;
   }

   if( ( nPos < nLen ) && ( nLen - nPos >= nDataLen ) )
   {
      if( ( elem->flags & MIME_FLAG_CASEINSENS ) == MIME_FLAG_CASEINSENS )
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || hb_strnicmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            /* is this the begin of a match tree? */
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iElem + elem->next );
            else
               return elem->mime_type;
         }
      }
      else
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || strncmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iElem + elem->next );
            else
               return elem->mime_type;
         }
      }
   }

   /* match failed! */
   if( elem->alternate != 0 )
      return s_findMimeStringInTree( cData, nLen, iElem + elem->alternate );

   return NULL;  /* total giveup */
}

static const char * s_findStringMimeType( const char * cData, HB_ISIZ nLen )
{
   int iCount;

   for( iCount = 0; iCount < MIME_TABLE_SIZE; iCount++ )
   {
      const MIME_ENTRY * elem = s_mimeTable + iCount;
      HB_ISIZ nPos     = elem->pos;
      HB_ISIZ nDataLen = strlen( elem->pattern );

      if( ( elem->flags & MIME_FLAG_CONTINUE ) == MIME_FLAG_CONTINUE )
         continue;

      /* trim spaces if required */
      while( nPos < nLen &&
             ( ( ( elem->flags & MIME_FLAG_TRIMSPACES ) == MIME_FLAG_TRIMSPACES && (
                    cData[ nPos ] == ' ' || cData[ nPos ] == '\r' || cData[ nPos ] == '\n' ) ) ||
               ( ( elem->flags & MIME_FLAG_TRIMTABS ) == MIME_FLAG_TRIMSPACES && cData[ nPos ] == '\t' ) ) )
      {
         nPos++;
      }

      if( nPos >= nLen )
         continue;

      if( nLen - nPos < nDataLen )
         continue;

      if( ( elem->flags & MIME_FLAG_CASEINSENS ) == MIME_FLAG_CASEINSENS )
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || hb_strnicmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            /* is this the begin of a match tree? */
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iCount + elem->next );
            else
               return elem->mime_type;
         }
      }
      else
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || strncmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iCount + elem->next );
            else
               return elem->mime_type;
         }
      }
   }
   return NULL;
}

static const char * s_findFileMimeType( HB_FHANDLE fileIn )
{
   char       buf[ 512 ];
   int        iLen;
   HB_FOFFSET nPos;

   nPos = hb_fsSeekLarge( fileIn, 0, FS_RELATIVE );
   hb_fsSeek( fileIn, 0, FS_SET );
   iLen = hb_fsRead( fileIn, buf, sizeof( buf ) );

   if( iLen > 0 )
   {
      hb_fsSeekLarge( fileIn, nPos, FS_SET );
      return s_findStringMimeType( buf, iLen );
   }

   return NULL;
}

HB_FUNC( TIP_FILEMIMETYPE )
{
   PHB_ITEM pFile = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );

   if( pFile )
   {
      HB_FHANDLE   fileIn;
      const char * ext_type   = NULL;
      const char * magic_type = NULL;

      if( HB_IS_STRING( pFile ) )
      {
         /* decode the extension */
         const char * fname = hb_itemGetCPtr( pFile );
         HB_ISIZ      nPos  = strlen( fname ) - 1;

         while( nPos >= 0 && fname[ nPos ] != '.' )
            nPos--;

         if( nPos > 0 )
            ext_type = s_findExtMimeType( fname + nPos + 1 );

         fileIn = hb_fsOpen( fname, FO_READ );

         if( hb_fsError() == 0 )
            magic_type = s_findFileMimeType( fileIn );

         hb_fsClose( fileIn );
      }
      else
      {
         fileIn     = ( HB_FHANDLE ) hb_itemGetNInt( pFile );
         magic_type = s_findFileMimeType( fileIn );
      }

      if( magic_type )
         hb_retc_const( magic_type );
      else
         hb_retc_const( ext_type ? ext_type : "unknown" );  /* "unknown" is a valid MIME type */
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( TIP_MIMETYPE )
{
   PHB_ITEM pData = hb_param( 1, HB_IT_STRING );

   if( pData )
   {
      const char * magic_type = s_findStringMimeType( hb_itemGetCPtr( pData ), hb_itemGetCLen( pData ) );

      hb_retc_const( magic_type ? magic_type : "unknown" );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   Case insensitive string comparison to optimize this expression:
   IF Lower( <cSubStr> ) == Lower( SubStr( <cString>, <nStart>, Len( <cSubStr> ) ) )
   <cString> must be provided as a pointer to the character string containing a substring
   <nStart> is the numeric position to start comparison in <cString>
   <cSubStr> is the character string to compare with characters in <cString>, beginning at <nStart>
 */

HB_FUNC( __TIP_PSTRCOMPI )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pStart  = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pSubstr = hb_param( 3, HB_IT_STRING );

   if( pString && pStart && pSubstr )
      hb_retl( hb_strnicmp( hb_itemGetCPtr( pString ) + hb_itemGetNS( pStart ) - 1,
                            hb_itemGetCPtr( pSubstr ),
                            hb_itemGetCLen( pSubstr ) ) == 0 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( TIP_HTMLSPECIALCHARS )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         const char * pszData = hb_parc( 1 );
         char *       cRet;
         HB_ISIZ      nPos    = 0;
         HB_ISIZ      nPosRet = 0;
         HB_BYTE      cElem;

         while( nLen && HB_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         cRet = ( char * ) hb_xgrab( nLen * 6 + 1 );

         while( nPos < nLen )
         {
            cElem = ( HB_BYTE ) pszData[ nPos ];

            if( cElem == '&' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = 'a';
               cRet[ nPosRet++ ] = 'm';
               cRet[ nPosRet++ ] = 'p';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '<' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = 'l';
               cRet[ nPosRet++ ] = 't';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '>' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = 'g';
               cRet[ nPosRet++ ] = 't';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '"' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = 'q';
               cRet[ nPosRet++ ] = 'u';
               cRet[ nPosRet++ ] = 'o';
               cRet[ nPosRet++ ] = 't';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\'' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = '#';
               cRet[ nPosRet++ ] = '0';
               cRet[ nPosRet++ ] = '3';
               cRet[ nPosRet++ ] = '9';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\r' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = '#';
               cRet[ nPosRet++ ] = '0';
               cRet[ nPosRet++ ] = '1';
               cRet[ nPosRet++ ] = '3';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem == '\n' )
            {
               cRet[ nPosRet++ ] = '&';
               cRet[ nPosRet++ ] = '#';
               cRet[ nPosRet++ ] = '0';
               cRet[ nPosRet++ ] = '1';
               cRet[ nPosRet++ ] = '0';
               cRet[ nPosRet++ ] = ';';
            }
            else if( cElem >= ' ' )
            {
               cRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         hb_retclen_buffer( cRet, nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( TIP_CRLF )
{
   hb_retc_const( "\r\n" );
}

HB_FUNC( TIP_JSONSPECIALCHARS )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         const char * pszData = hb_parc( 1 );
         char *       cRet;
         HB_ISIZ      nPos    = 0;
         HB_ISIZ      nPosRet = 0;
         HB_BYTE      cElem;

         while( nLen && HB_ISSPACE( pszData[ nLen - 1 ] ) )
            nLen--;

         /* Giving maximum final length possible */
         cRet = ( char * ) hb_xgrab( nLen * 6 + 1 );

         while( nPos < nLen )
         {
            cElem = ( HB_BYTE ) pszData[ nPos ];

            if( cElem == '"' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = '"';
            }
            else if( cElem == '\\' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = '\\';
            }
            else if( cElem == '/' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = '/';
            }
            else if( cElem == '\b' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = 'b';
            }
            else if( cElem == '\f' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = 'f';
            }
            else if( cElem == '\r' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = 'r';
            }
            else if( cElem == '\n' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = 'n';
            }
            else if( cElem == '\t' )
            {
               cRet[ nPosRet++ ] = '\\';
               cRet[ nPosRet++ ] = 't';
            }
            else if( cElem >= ' ' )
            {
               cRet[ nPosRet++ ] = cElem;
            }

            nPos++;
         }

         hb_retclen_buffer( cRet, nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
