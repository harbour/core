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
 * Useful internet timestamp based on RFC 822 & RFC 2822
 */

HB_FUNC( TIP_TIMESTAMP )
{
   static const char * s_days[]   = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

   char szRet[ 64 ];
   int  iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
   long lOffset;

   if( HB_ISDATE( 1 ) )
   {
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );

      /* For compatibility, seconds() value */
      if( HB_ISNUM( 2 ) )
         hb_timeDecode( hb_parnd( 2 ) * 1000,
                        &iHour, &iMinute, &iSecond, &iMSec );
      else
         iHour = iMinute = iSecond = 0;
   }
   else if( HB_ISDATETIME( 1 ) )
      hb_timeStampUnpack( hb_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
   else
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );

   lOffset = hb_timeStampUTCOffset( iYear, iMonth, iDay, iHour, iMinute, iSecond );

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

#define EXT_MIME_TABLE_SIZE  244

static EXT_MIME_ENTRY s_extMimeTable[ EXT_MIME_TABLE_SIZE ] =
{
   /*   */ { "3dm"     , "x-world/x-3dmf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "3dmf"    , "x-world/x-3dmf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "aab"     , "application/x-authorware-bin"                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "aam"     , "application/x-authorware-map"                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "aas"     , "application/x-authorware-map"                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "adr"     , "application/x-msaddr"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "afl"     , "video/animaflex"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "ai"      , "application/postscript"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "aif"     , "audio/x-aiff"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "aifc"    , "audio/x-aiff"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "aiff"    , "audio/x-aiff"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "alt"     , "application/x-up-alert"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "arj"     , "application/x-arj"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "asd"     , "application/astound"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "asf"     , "video/x-ms-asf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "asn"     , "application/astound"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "asp"     , "application/x-asap"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "asx"     , "video/x-ms-asf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "asz"     , "application/astound"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "au"      , "audio/basic"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "avi"     , "video/x-msvideo"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "axs"     , "application/olescript"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "bcpio"   , "application/x-bcpio"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "bin"     , "application/octet-stream"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "bin"     , "application/x-macbinary"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "c"       , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "c++"     , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "cdf"     , "application/x-netcdf"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "chat"    , "application/x-chat"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "che"     , "application/x-up-cacheop"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "cht"     , "audio/x-dspeech"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "class"   , "application/java-vm"                                               , 0                    }, /* case sensitive! */
   /*   */ { "cnc"     , "application/x-cnc"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "cod"     , "image/cis-cod"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "coda"    , "application/x-coda"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "con"     , "application/x-connector"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "cpi"     , "image/cpi"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "cpio"    , "application/x-cpio"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "cpp"     , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "cu"      , "application/x-cu-seeme"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "cxx"     , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "ccs"     , "text/ccs"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "csh"     , "application/x-csh"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "csm"     , "application/x-cu-seeme"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "css"     , "application/x-pointplus"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "css"     , "text/css"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "csv"     , "text/csv"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "dbf"     , "application/octet-stream"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "dcr"     , "application/x-director"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "dig"     , "multipart/mixed"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "dir"     , "application/x-director"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "doc"     , "application/msword"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "dsf"     , "image/x-mgx-dsf"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "dst"     , "application/tajima"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "dus"     , "audio/x-dspeech"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "dvi"     , "application/x-dvi"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "dwf"     , "drawing/x-dwf"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "dwg"     , "image/vnd"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "dxf"     , "image/vnd"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "dxr"     , "application/x-director"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "ebk"     , "application/x-expandedbook"                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "eps"     , "application/postscript"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "es"      , "audio/echospeech"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "etf"     , "image/x-etf"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "etx"     , "text/x-setext"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "evy"     , "application/x-envoy"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "exe"     , "application/x-msdownload"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "fh4"     , "image/x-freehand"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "fh5"     , "image/x-freehand"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "fhc"     , "image/x-freehand"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "fif"     , "image/fif"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "fpx"     , "image/x-fpx"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "frl"     , "application/freeloader"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "gif"     , "image/gif"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "gsd"     , "audio/gsm"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "gsm"     , "audio/gsm"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "gtar"    , "application/x-gtar"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "gz"      , "application/gzip"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "h"       , "text/x-c-header"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "hdf"     , "application/x-hdf"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "hdml"    , "text/x-hdml"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "hpp"     , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "hqx"     , "application/mac-binhex40"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "htm"     , "text/html"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "html"    , "text/html"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "hxx"     , "text/x-c"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "ica"     , "application/x-ica"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "ief"     , "image/ief"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "ins"     , "application/x-NET-Install"                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "ips"     , "application/x-ipscript"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "ipx"     , "application/x-ipix"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "ivr"     , "i-world/i-vrml"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "java"    , "text/java"                                                         , 0                    },
   /*   */ { "jpe"     , "image/jpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "jpeg"    , "image/jpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "jpg"     , "image/jpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "js"      , "application/x-javascript"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "latex"   , "application/x-latex"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "lha"     , "application/octet-stream"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "lzh"     , "application/octet-stream"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "lzx"     , "application/octet-stream"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "m3u"     , "audio/x-mpegurl"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "man"     , "application/x-troff-man"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "map"     , "application/x-httpd-imap"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "mbd"     , "application/mbedlet"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "mcf"     , "image/vasa"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "me"      , "application/x-troff-me"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "mfp"     , "application/mirage"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "mid"     , "audio/x-midi"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "midi"    , "audio/x-midi"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "mif"     , "application/x-mif"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "mol"     , "chemical/x-mdl-molfile"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "mov"     , "video/quicktime"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "movie"   , "video/x-sgi-movie"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "mp2"     , "audio/x-mpeg"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "mp3"     , "audio/x-mpeg"                                                      , MIME_FLAG_CASEINSENS },
   /*   */ { "mpe"     , "video/mpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "mpeg"    , "video/mpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "mpg"     , "video/mpeg"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "mpire"   , "application/x-mpire"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "mpl"     , "application/x-mpire"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "ms"      , "application/x-troff-ms"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "n2p"     , "application/n2p"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "nc"      , "application/x-netcdf"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "npx"     , "application/x-netfpx"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "nsc"     , "application/x-nschat"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "oda"     , "application/oda"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "ods"     , "application/vnd.oasis.opendocument.spreadsheet"                    , MIME_FLAG_CASEINSENS },
   /*   */ { "ofml"    , "application/fml"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "page"    , "application/x-coda"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "pbm"     , "image/x-portable-bitmap"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "pdb"     , "chemical/x-pdb"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "pdf"     , "application/pdf"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "pfr"     , "application/font-tdpfr"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "pgm"     , "image/x-portable-graymap"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "pgp"     , "application/x-pgp-plugin"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "pgr"     , "text/parsnegar-document"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "php3"    , "application/x-httpd-php3"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "phtml"   , "application/x-httpd-php"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "pnm"     , "image/x-portable-anymap"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "pot"     , "application/mspowerpoint"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "ppm"     , "image/x-portable-pixmap"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "pps"     , "application/mspowerpoint"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "ppt"     , "application/mspowerpoint"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "ppz"     , "application/mspowerpoint"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "pqf"     , "application/x-cprplayer"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "pqi"     , "application/cprplayer"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "ps"      , "application/postscript"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "ptlk"    , "application/listenup"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "push"    , "multipart/x-mixed-replace"                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "qd3"     , "x-world/x-3dmf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "qd3d"    , "x-world/x-3dmf"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "qrt"     , "application/quest"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "qt"      , "video/quicktime"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "ra"      , "audio/x-pn-realaudio"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "ram"     , "audio/x-pn-realaudio"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "ras"     , "image/x-cmu-raster"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "rgb"     , "image/x-rgb"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "rip"     , "image/rip"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "rmf"     , "audio/x-rmf"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "roff"    , "application/x-troff"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "rpm"     , "audio/x-pn-realaudio-plugin"                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "rrf"     , "application/x-InstallFromTheWeb"                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "rtc"     , "application/rtc"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "rtf"     , "application/rtf"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "rtx"     , "text/richtext"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "sca"     , "application/x-supercard"                                           , MIME_FLAG_CASEINSENS },
   /*   */ { "sh"      , "application/x-sh"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "shar"    , "application/x-shar"                                                , MIME_FLAG_CASEINSENS },
   /*   */ { "shw"     , "application/presentations"                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "sit"     , "application/x-stuffit"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "sml"     , "application/fml"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "smp"     , "application/studiom"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "snd"     , "audio/basic"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "spc"     , "text/x-speech"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "spl"     , "application/futuresplash"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "spr"     , "application/x-sprite"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "sprite"  , "application/x-sprite"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "src"     , "application/x-wais-source"                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "stk"     , "application/hstu"                                                  , MIME_FLAG_CASEINSENS },
   /*   */ { "stream"  , "audio/x-qt-stream"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "sv4cpio" , "application/x-sv4cpio"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "sv4crc"  , "application/x-sv4crc"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "svf"     , "image/vnd"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "svh"     , "image/svh"                                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "svr"     , "x-world/x-svr"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "swa"     , "application/x-director"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "swf"     , "application/x-shockwave-flash"                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "t"       , "application/x-troff"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "talk"    , "text/x-speech"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "tar"     , "application/x-tar"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "tbk"     , "application/toolbook"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "tcl"     , "application/x-tcl"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "tex"     , "application/x-tex"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "texi"    , "application/x-texinfo"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "texinfo" , "application/x-texinfo"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "tif"     , "image/tiff"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "tiff"    , "image/tiff"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "tlk"     , "application/x-tlk"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "tmv"     , "application/x-Parable-Thing"                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "tr"      , "application/x-troff"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "tsi"     , "audio/tsplayer"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "tsp"     , "application/dsptype"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "tsv"     , "text/tab-separated-values"                                         , MIME_FLAG_CASEINSENS },
   /*   */ { "txt"     , "text/plain"                                                        , MIME_FLAG_CASEINSENS },
   /*   */ { "ustar"   , "application/x-ustar"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "vbd"     , "application/activexdocument"                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "vcd"     , "application/x-cdlink"                                              , MIME_FLAG_CASEINSENS },
   /*   */ { "vgm"     , "video/x-videogram"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "vgp"     , "video/x-videogram-plugin"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "vgx"     , "video/x-videogram"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "viv"     , "video/vnd.vivo"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "vivo"    , "video/vnd.vivo"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "vmd"     , "application/vocaltec-media-desc"                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "vmf"     , "application/vocaltec-media-file"                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "vox"     , "audio/voxware"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "vqe"     , "audio/x-twinvq-plugin"                                             , MIME_FLAG_CASEINSENS },
   /*   */ { "vqf"     , "audio/x-twinvq"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "vql"     , "audio/x-twinvq"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "vrt"     , "x-world/x-vrt"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "vts"     , "workbook/formulaone"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "vtts"    , "workbook/formulaone"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "waf"     , "plugin/wanimate"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "wan"     , "plugin/wanimate"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "wav"     , "audio/x-wav"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "wi"      , "image/wavelet"                                                     , MIME_FLAG_CASEINSENS },
   /*   */ { "wid"     , "application/x-DemoShield"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "wis"     , "application/x-InstallShield"                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "wlt"     , "application/x-mswallet"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "wri"     , "application/write"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "wrl"     , "x-world/x-vrml"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "wrz"     , "x-world/x-vrml"                                                    , MIME_FLAG_CASEINSENS },
   /*   */ { "wtx"     , "audio/x-wtx"                                                       , MIME_FLAG_CASEINSENS },
   /*   */ { "xbm"     , "image/x-xbitmap"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "xdr"     , "video/x-videogram"                                                 , MIME_FLAG_CASEINSENS },
   /*   */ { "xls"     , "application/vnd.ms-excel"                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "xlsx"    , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" , MIME_FLAG_CASEINSENS },
   /*   */ { "xlt"     , "application/xlt"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "xml"     , "text/xml"                                                          , MIME_FLAG_CASEINSENS },
   /*   */ { "xpm"     , "image/x-xpixmap"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "xwd"     , "image/x-xwindowdump"                                               , MIME_FLAG_CASEINSENS },
   /*   */ { "z"       , "application/x-compress"                                            , MIME_FLAG_CASEINSENS },
   /*   */ { "zip"     , "application/zip"                                                   , MIME_FLAG_CASEINSENS },
   /*   */ { "zpa"     , "application/pcphoto"                                               , MIME_FLAG_CASEINSENS }
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

HB_FUNC( TIP_FILENAMEMIMETYPE )
{
   const char * fname = hb_parc( 1 );

   if( fname )
   {
      const char * ext_type = NULL;
      HB_ISIZ nPos = strlen( fname ) - 1;

      while( nPos >= 0 && fname[ nPos ] != '.' )
         nPos--;

      if( nPos > 0 )
         ext_type = s_findExtMimeType( fname + nPos + 1 );

      hb_retc_const( ext_type ? ext_type : "unknown" );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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
         if( fileIn != FS_ERROR )
         {
            magic_type = s_findFileMimeType( fileIn );
            hb_fsClose( fileIn );
         }
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
