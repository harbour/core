/*
 * Harbour Project source code:
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#define MIME_FLAG_CASESENS    0x0000
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
   HB_USHORT    flags;     /* flags for confrontation */
   const char * mime_type; /* Mimetype if complete */
} EXT_MIME_ENTRY;

#define EXT_MIME_TABLE_SIZE  316

static EXT_MIME_ENTRY s_extMimeTable[ EXT_MIME_TABLE_SIZE ] =
{
   { "3dm"     , MIME_FLAG_CASEINSENS, "x-world/x-3dmf" },
   { "3dmf"    , MIME_FLAG_CASEINSENS, "x-world/x-3dmf" },
   { "3gp"     , MIME_FLAG_CASEINSENS, "video/3gpp" },
   { "3gpp"    , MIME_FLAG_CASEINSENS, "video/3gpp" },
   { "7z"      , MIME_FLAG_CASEINSENS, "application/x-7z-compressed" },
   { "7z"      , MIME_FLAG_CASEINSENS, "application/x-7z-compressed" },
   { "aab"     , MIME_FLAG_CASEINSENS, "application/x-authorware-bin" },
   { "aam"     , MIME_FLAG_CASEINSENS, "application/x-authorware-map" },
   { "aas"     , MIME_FLAG_CASEINSENS, "application/x-authorware-map" },
   { "adr"     , MIME_FLAG_CASEINSENS, "application/x-msaddr" },
   { "afl"     , MIME_FLAG_CASEINSENS, "video/animaflex" },
   { "ai"      , MIME_FLAG_CASEINSENS, "application/postscript" },
   { "aif"     , MIME_FLAG_CASEINSENS, "audio/x-aiff" },
   { "aifc"    , MIME_FLAG_CASEINSENS, "audio/x-aiff" },
   { "aiff"    , MIME_FLAG_CASEINSENS, "audio/x-aiff" },
   { "alt"     , MIME_FLAG_CASEINSENS, "application/x-up-alert" },
   { "arj"     , MIME_FLAG_CASEINSENS, "application/x-arj" },
   { "asd"     , MIME_FLAG_CASEINSENS, "application/astound" },
   { "asf"     , MIME_FLAG_CASEINSENS, "video/x-ms-asf" },
   { "asn"     , MIME_FLAG_CASEINSENS, "application/astound" },
   { "asp"     , MIME_FLAG_CASEINSENS, "application/x-asap" },
   { "asx"     , MIME_FLAG_CASEINSENS, "video/x-ms-asf" },
   { "asz"     , MIME_FLAG_CASEINSENS, "application/astound" },
   { "atom"    , MIME_FLAG_CASEINSENS, "application/atom+xml" },
   { "au"      , MIME_FLAG_CASEINSENS, "audio/basic" },
   { "avi"     , MIME_FLAG_CASEINSENS, "video/x-msvideo" },
   { "axs"     , MIME_FLAG_CASEINSENS, "application/olescript" },
   { "bcpio"   , MIME_FLAG_CASEINSENS, "application/x-bcpio" },
   { "bin"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "bmp"     , MIME_FLAG_CASEINSENS, "image/x-ms-bmp" },
   { "c"       , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "c++"     , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "cco"     , MIME_FLAG_CASEINSENS, "application/x-cocoa" },
   { "cdf"     , MIME_FLAG_CASEINSENS, "application/x-netcdf" },
   { "chat"    , MIME_FLAG_CASEINSENS, "application/x-chat" },
   { "che"     , MIME_FLAG_CASEINSENS, "application/x-up-cacheop" },
   { "cht"     , MIME_FLAG_CASEINSENS, "audio/x-dspeech" },
   { "class"   , MIME_FLAG_CASESENS  , "application/java-vm" },
   { "cnc"     , MIME_FLAG_CASEINSENS, "application/x-cnc" },
   { "cod"     , MIME_FLAG_CASEINSENS, "image/cis-cod" },
   { "coda"    , MIME_FLAG_CASEINSENS, "application/x-coda" },
   { "con"     , MIME_FLAG_CASEINSENS, "application/x-connector" },
   { "cpi"     , MIME_FLAG_CASEINSENS, "image/cpi" },
   { "cpio"    , MIME_FLAG_CASEINSENS, "application/x-cpio" },
   { "cpp"     , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "crt"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "cu"      , MIME_FLAG_CASEINSENS, "application/x-cu-seeme" },
   { "cxx"     , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "ccs"     , MIME_FLAG_CASEINSENS, "text/ccs" },
   { "csh"     , MIME_FLAG_CASEINSENS, "application/x-csh" },
   { "csm"     , MIME_FLAG_CASEINSENS, "application/x-cu-seeme" },
   { "css"     , MIME_FLAG_CASEINSENS, "text/css" },
   { "csv"     , MIME_FLAG_CASEINSENS, "text/csv" },
   { "dbf"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "dcr"     , MIME_FLAG_CASEINSENS, "application/x-director" },
   { "deb"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "der"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "dig"     , MIME_FLAG_CASEINSENS, "multipart/mixed" },
   { "dir"     , MIME_FLAG_CASEINSENS, "application/x-director" },
   { "dll"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "dmg"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "doc"     , MIME_FLAG_CASEINSENS, "application/msword" },
   { "docx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.wordprocessingml.document" },
   { "dsf"     , MIME_FLAG_CASEINSENS, "image/x-mgx-dsf" },
   { "dst"     , MIME_FLAG_CASEINSENS, "application/tajima" },
   { "dus"     , MIME_FLAG_CASEINSENS, "audio/x-dspeech" },
   { "dvi"     , MIME_FLAG_CASEINSENS, "application/x-dvi" },
   { "dwf"     , MIME_FLAG_CASEINSENS, "drawing/x-dwf" },
   { "dwg"     , MIME_FLAG_CASEINSENS, "image/vnd" },
   { "dxf"     , MIME_FLAG_CASEINSENS, "image/vnd" },
   { "dxr"     , MIME_FLAG_CASEINSENS, "application/x-director" },
   { "ear"     , MIME_FLAG_CASEINSENS, "application/java-archive" },
   { "ebk"     , MIME_FLAG_CASEINSENS, "application/x-expandedbook" },
   { "eot"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "eps"     , MIME_FLAG_CASEINSENS, "application/postscript" },
   { "es"      , MIME_FLAG_CASEINSENS, "audio/echospeech" },
   { "etf"     , MIME_FLAG_CASEINSENS, "image/x-etf" },
   { "etx"     , MIME_FLAG_CASEINSENS, "text/x-setext" },
   { "evy"     , MIME_FLAG_CASEINSENS, "application/x-envoy" },
   { "exe"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "fh4"     , MIME_FLAG_CASEINSENS, "image/x-freehand" },
   { "fh5"     , MIME_FLAG_CASEINSENS, "image/x-freehand" },
   { "fhc"     , MIME_FLAG_CASEINSENS, "image/x-freehand" },
   { "fif"     , MIME_FLAG_CASEINSENS, "image/fif" },
   { "flv"     , MIME_FLAG_CASEINSENS, "video/x-flv" },
   { "fpx"     , MIME_FLAG_CASEINSENS, "image/x-fpx" },
   { "frl"     , MIME_FLAG_CASEINSENS, "application/freeloader" },
   { "gif"     , MIME_FLAG_CASEINSENS, "image/gif" },
   { "gsd"     , MIME_FLAG_CASEINSENS, "audio/gsm" },
   { "gsm"     , MIME_FLAG_CASEINSENS, "audio/gsm" },
   { "gtar"    , MIME_FLAG_CASEINSENS, "application/x-gtar" },
   { "gz"      , MIME_FLAG_CASEINSENS, "application/gzip" },
   { "h"       , MIME_FLAG_CASEINSENS, "text/x-c-header" },
   { "hdf"     , MIME_FLAG_CASEINSENS, "application/x-hdf" },
   { "hdml"    , MIME_FLAG_CASEINSENS, "text/x-hdml" },
   { "hpp"     , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "hqx"     , MIME_FLAG_CASEINSENS, "application/mac-binhex40" },
   { "htc"     , MIME_FLAG_CASEINSENS, "text/x-component" },
   { "htm"     , MIME_FLAG_CASEINSENS, "text/html" },
   { "html"    , MIME_FLAG_CASEINSENS, "text/html" },
   { "hxx"     , MIME_FLAG_CASEINSENS, "text/x-c" },
   { "ica"     , MIME_FLAG_CASEINSENS, "application/x-ica" },
   { "ico"     , MIME_FLAG_CASEINSENS, "image/x-icon" },
   { "ief"     , MIME_FLAG_CASEINSENS, "image/ief" },
   { "img"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "ins"     , MIME_FLAG_CASEINSENS, "application/x-NET-Install" },
   { "ips"     , MIME_FLAG_CASEINSENS, "application/x-ipscript" },
   { "ipx"     , MIME_FLAG_CASEINSENS, "application/x-ipix" },
   { "iso"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "ivr"     , MIME_FLAG_CASEINSENS, "i-world/i-vrml" },
   { "jad"     , MIME_FLAG_CASEINSENS, "text/vnd.sun.j2me.app-descriptor" },
   { "jar"     , MIME_FLAG_CASEINSENS, "application/java-archive" },
   { "jardiff" , MIME_FLAG_CASEINSENS, "application/x-java-archive-diff" },
   { "java"    , MIME_FLAG_CASESENS  , "text/java" },
   { "jng"     , MIME_FLAG_CASEINSENS, "image/x-jng" },
   { "jnlp"    , MIME_FLAG_CASEINSENS, "application/x-java-jnlp-file" },
   { "jpe"     , MIME_FLAG_CASEINSENS, "image/jpeg" },
   { "jpeg"    , MIME_FLAG_CASEINSENS, "image/jpeg" },
   { "jpg"     , MIME_FLAG_CASEINSENS, "image/jpeg" },
   { "js"      , MIME_FLAG_CASEINSENS, "application/x-javascript" },
   { "kar"     , MIME_FLAG_CASEINSENS, "audio/midi" },
   { "kml"     , MIME_FLAG_CASEINSENS, "application/vnd.google-earth.kml+xml" },
   { "kmz"     , MIME_FLAG_CASEINSENS, "application/vnd.google-earth.kmz" },
   { "latex"   , MIME_FLAG_CASEINSENS, "application/x-latex" },
   { "lha"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "lzh"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "lzx"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "m3u"     , MIME_FLAG_CASEINSENS, "audio/x-mpegurl" },
   { "m4a"     , MIME_FLAG_CASEINSENS, "audio/x-m4a" },
   { "m4v"     , MIME_FLAG_CASEINSENS, "video/x-m4v" },
   { "man"     , MIME_FLAG_CASEINSENS, "application/x-troff-man" },
   { "map"     , MIME_FLAG_CASEINSENS, "application/x-httpd-imap" },
   { "mbd"     , MIME_FLAG_CASEINSENS, "application/mbedlet" },
   { "mcf"     , MIME_FLAG_CASEINSENS, "image/vasa" },
   { "md"      , MIME_FLAG_CASEINSENS, "text/plain" },
   { "me"      , MIME_FLAG_CASEINSENS, "application/x-troff-me" },
   { "mfp"     , MIME_FLAG_CASEINSENS, "application/mirage" },
   { "mid"     , MIME_FLAG_CASEINSENS, "audio/midi" },
   { "midi"    , MIME_FLAG_CASEINSENS, "audio/midi" },
   { "mif"     , MIME_FLAG_CASEINSENS, "application/x-mif" },
   { "mml"     , MIME_FLAG_CASEINSENS, "text/mathml" },
   { "mng"     , MIME_FLAG_CASEINSENS, "video/x-mng" },
   { "mol"     , MIME_FLAG_CASEINSENS, "chemical/x-mdl-molfile" },
   { "mov"     , MIME_FLAG_CASEINSENS, "video/quicktime" },
   { "movie"   , MIME_FLAG_CASEINSENS, "video/x-sgi-movie" },
   { "mp2"     , MIME_FLAG_CASEINSENS, "audio/x-mpeg" },
   { "mp3"     , MIME_FLAG_CASEINSENS, "audio/mpeg" },
   { "mp4"     , MIME_FLAG_CASEINSENS, "video/mp4" },
   { "mpe"     , MIME_FLAG_CASEINSENS, "video/mpeg" },
   { "mpeg"    , MIME_FLAG_CASEINSENS, "video/mpeg" },
   { "mpg"     , MIME_FLAG_CASEINSENS, "video/mpeg" },
   { "mpire"   , MIME_FLAG_CASEINSENS, "application/x-mpire" },
   { "mpl"     , MIME_FLAG_CASEINSENS, "application/x-mpire" },
   { "ms"      , MIME_FLAG_CASEINSENS, "application/x-troff-ms" },
   { "msi"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "msm"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "msp"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "n2p"     , MIME_FLAG_CASEINSENS, "application/n2p" },
   { "nc"      , MIME_FLAG_CASEINSENS, "application/x-netcdf" },
   { "npx"     , MIME_FLAG_CASEINSENS, "application/x-netfpx" },
   { "nsc"     , MIME_FLAG_CASEINSENS, "application/x-nschat" },
   { "oda"     , MIME_FLAG_CASEINSENS, "application/oda" },
   { "odb"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.database" },
   { "odc"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.chart" },
   { "odf"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.formula" },
   { "odg"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.graphics" },
   { "odi"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.image" },
   { "odm"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.text-master" },
   { "odp"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.presentation" },
   { "ods"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.spreadsheet" },
   { "odt"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.text" },
   { "ofml"    , MIME_FLAG_CASEINSENS, "application/fml" },
   { "ogg"     , MIME_FLAG_CASEINSENS, "audio/ogg" },
   { "oth"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.text-web" },
   { "oxt"     , MIME_FLAG_CASEINSENS, "application/vnd.openofficeorg.extension" },
   { "page"    , MIME_FLAG_CASEINSENS, "application/x-coda" },
   { "pbm"     , MIME_FLAG_CASEINSENS, "image/x-portable-bitmap" },
   { "pdb"     , MIME_FLAG_CASEINSENS, "application/x-pilot" },
   { "pdf"     , MIME_FLAG_CASEINSENS, "application/pdf" },
   { "pem"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "pfr"     , MIME_FLAG_CASEINSENS, "application/font-tdpfr" },
   { "pgm"     , MIME_FLAG_CASEINSENS, "image/x-portable-graymap" },
   { "pgp"     , MIME_FLAG_CASEINSENS, "application/x-pgp-plugin" },
   { "pgr"     , MIME_FLAG_CASEINSENS, "text/parsnegar-document" },
   { "php3"    , MIME_FLAG_CASEINSENS, "application/x-httpd-php3" },
   { "phtml"   , MIME_FLAG_CASEINSENS, "application/x-httpd-php" },
   { "pl"      , MIME_FLAG_CASEINSENS, "application/x-perl" },
   { "pm"      , MIME_FLAG_CASEINSENS, "application/x-perl" },
   { "png"     , MIME_FLAG_CASEINSENS, "image/png" },
   { "pnm"     , MIME_FLAG_CASEINSENS, "image/x-portable-anymap" },
   { "pot"     , MIME_FLAG_CASEINSENS, "application/mspowerpoint" },
   { "ppm"     , MIME_FLAG_CASEINSENS, "image/x-portable-pixmap" },
   { "pps"     , MIME_FLAG_CASEINSENS, "application/mspowerpoint" },
   { "ppsx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.presentationml.slideshow" },
   { "ppt"     , MIME_FLAG_CASEINSENS, "application/vnd.ms-powerpoint" },
   { "pptx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.presentationml.presentation" },
   { "ppz"     , MIME_FLAG_CASEINSENS, "application/mspowerpoint" },
   { "pqf"     , MIME_FLAG_CASEINSENS, "application/x-cprplayer" },
   { "pqi"     , MIME_FLAG_CASEINSENS, "application/cprplayer" },
   { "prc"     , MIME_FLAG_CASEINSENS, "application/x-pilot" },
   { "ps"      , MIME_FLAG_CASEINSENS, "application/postscript" },
   { "ptlk"    , MIME_FLAG_CASEINSENS, "application/listenup" },
   { "push"    , MIME_FLAG_CASEINSENS, "multipart/x-mixed-replace" },
   { "qd3"     , MIME_FLAG_CASEINSENS, "x-world/x-3dmf" },
   { "qd3d"    , MIME_FLAG_CASEINSENS, "x-world/x-3dmf" },
   { "qrt"     , MIME_FLAG_CASEINSENS, "application/quest" },
   { "qt"      , MIME_FLAG_CASEINSENS, "video/quicktime" },
   { "ra"      , MIME_FLAG_CASEINSENS, "audio/x-realaudio" },
   { "ram"     , MIME_FLAG_CASEINSENS, "audio/x-realaudio" },
   { "rar"     , MIME_FLAG_CASEINSENS, "application/x-rar-compressed" },
   { "ras"     , MIME_FLAG_CASEINSENS, "image/x-cmu-raster" },
   { "rgb"     , MIME_FLAG_CASEINSENS, "image/x-rgb" },
   { "rip"     , MIME_FLAG_CASEINSENS, "image/rip" },
   { "rmf"     , MIME_FLAG_CASEINSENS, "audio/x-rmf" },
   { "roff"    , MIME_FLAG_CASEINSENS, "application/x-troff" },
   { "rpm"     , MIME_FLAG_CASEINSENS, "application/x-redhat-package-manager" },
   { "rrf"     , MIME_FLAG_CASEINSENS, "application/x-InstallFromTheWeb" },
   { "rss"     , MIME_FLAG_CASEINSENS, "application/rss+xml" },
   { "rst"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "rtc"     , MIME_FLAG_CASEINSENS, "application/rtc" },
   { "rtf"     , MIME_FLAG_CASEINSENS, "application/rtf" },
   { "rtx"     , MIME_FLAG_CASEINSENS, "text/richtext" },
   { "run"     , MIME_FLAG_CASEINSENS, "application/x-makeself" },
   { "sca"     , MIME_FLAG_CASEINSENS, "application/x-supercard" },
   { "sea"     , MIME_FLAG_CASEINSENS, "application/x-sea" },
   { "sh"      , MIME_FLAG_CASEINSENS, "application/x-sh" },
   { "shar"    , MIME_FLAG_CASEINSENS, "application/x-shar" },
   { "shtml"   , MIME_FLAG_CASEINSENS, "text/html" },
   { "shw"     , MIME_FLAG_CASEINSENS, "application/presentations" },
   { "sit"     , MIME_FLAG_CASEINSENS, "application/x-stuffit" },
   { "sit"     , MIME_FLAG_CASEINSENS, "application/x-stuffit" },
   { "sml"     , MIME_FLAG_CASEINSENS, "application/fml" },
   { "smp"     , MIME_FLAG_CASEINSENS, "application/studiom" },
   { "snd"     , MIME_FLAG_CASEINSENS, "audio/basic" },
   { "spc"     , MIME_FLAG_CASEINSENS, "text/x-speech" },
   { "spl"     , MIME_FLAG_CASEINSENS, "application/futuresplash" },
   { "spr"     , MIME_FLAG_CASEINSENS, "application/x-sprite" },
   { "sprite"  , MIME_FLAG_CASEINSENS, "application/x-sprite" },
   { "src"     , MIME_FLAG_CASEINSENS, "application/x-wais-source" },
   { "stk"     , MIME_FLAG_CASEINSENS, "application/hstu" },
   { "stream"  , MIME_FLAG_CASEINSENS, "audio/x-qt-stream" },
   { "sv4cpio" , MIME_FLAG_CASEINSENS, "application/x-sv4cpio" },
   { "sv4crc"  , MIME_FLAG_CASEINSENS, "application/x-sv4crc" },
   { "svf"     , MIME_FLAG_CASEINSENS, "image/vnd" },
   { "svg"     , MIME_FLAG_CASEINSENS, "image/svg+xml" },
   { "svgz"    , MIME_FLAG_CASEINSENS, "image/svg+xml" },
   { "svh"     , MIME_FLAG_CASEINSENS, "image/svh" },
   { "svr"     , MIME_FLAG_CASEINSENS, "x-world/x-svr" },
   { "swa"     , MIME_FLAG_CASEINSENS, "application/x-director" },
   { "swf"     , MIME_FLAG_CASEINSENS, "application/x-shockwave-flash" },
   { "t"       , MIME_FLAG_CASEINSENS, "application/x-troff" },
   { "talk"    , MIME_FLAG_CASEINSENS, "text/x-speech" },
   { "tar"     , MIME_FLAG_CASEINSENS, "application/x-tar" },
   { "tbk"     , MIME_FLAG_CASEINSENS, "application/toolbook" },
   { "tcl"     , MIME_FLAG_CASEINSENS, "application/x-tcl" },
   { "tex"     , MIME_FLAG_CASEINSENS, "application/x-tex" },
   { "texi"    , MIME_FLAG_CASEINSENS, "application/x-texinfo" },
   { "texinfo" , MIME_FLAG_CASEINSENS, "application/x-texinfo" },
   { "tif"     , MIME_FLAG_CASEINSENS, "image/tiff" },
   { "tiff"    , MIME_FLAG_CASEINSENS, "image/tiff" },
   { "tk"      , MIME_FLAG_CASEINSENS, "application/x-tcl" },
   { "tlk"     , MIME_FLAG_CASEINSENS, "application/x-tlk" },
   { "tmv"     , MIME_FLAG_CASEINSENS, "application/x-Parable-Thing" },
   { "tr"      , MIME_FLAG_CASEINSENS, "application/x-troff" },
   { "tsi"     , MIME_FLAG_CASEINSENS, "audio/tsplayer" },
   { "tsp"     , MIME_FLAG_CASEINSENS, "application/dsptype" },
   { "tsv"     , MIME_FLAG_CASEINSENS, "text/tab-separated-values" },
   { "txt"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "ustar"   , MIME_FLAG_CASEINSENS, "application/x-ustar" },
   { "vbd"     , MIME_FLAG_CASEINSENS, "application/activexdocument" },
   { "vcd"     , MIME_FLAG_CASEINSENS, "application/x-cdlink" },
   { "vgm"     , MIME_FLAG_CASEINSENS, "video/x-videogram" },
   { "vgp"     , MIME_FLAG_CASEINSENS, "video/x-videogram-plugin" },
   { "vgx"     , MIME_FLAG_CASEINSENS, "video/x-videogram" },
   { "viv"     , MIME_FLAG_CASEINSENS, "video/vnd.vivo" },
   { "vivo"    , MIME_FLAG_CASEINSENS, "video/vnd.vivo" },
   { "vmd"     , MIME_FLAG_CASEINSENS, "application/vocaltec-media-desc" },
   { "vmf"     , MIME_FLAG_CASEINSENS, "application/vocaltec-media-file" },
   { "vox"     , MIME_FLAG_CASEINSENS, "audio/voxware" },
   { "vqe"     , MIME_FLAG_CASEINSENS, "audio/x-twinvq-plugin" },
   { "vqf"     , MIME_FLAG_CASEINSENS, "audio/x-twinvq" },
   { "vql"     , MIME_FLAG_CASEINSENS, "audio/x-twinvq" },
   { "vrt"     , MIME_FLAG_CASEINSENS, "x-world/x-vrt" },
   { "vts"     , MIME_FLAG_CASEINSENS, "workbook/formulaone" },
   { "vtts"    , MIME_FLAG_CASEINSENS, "workbook/formulaone" },
   { "waf"     , MIME_FLAG_CASEINSENS, "plugin/wanimate" },
   { "wan"     , MIME_FLAG_CASEINSENS, "plugin/wanimate" },
   { "war"     , MIME_FLAG_CASEINSENS, "application/java-archive" },
   { "wav"     , MIME_FLAG_CASEINSENS, "audio/x-wav" },
   { "wbmp"    , MIME_FLAG_CASEINSENS, "image/vnd.wap.wbmp" },
   { "webm"    , MIME_FLAG_CASEINSENS, "video/webm" },
   { "webp"    , MIME_FLAG_CASEINSENS, "image/webp" },
   { "wi"      , MIME_FLAG_CASEINSENS, "image/wavelet" },
   { "wid"     , MIME_FLAG_CASEINSENS, "application/x-DemoShield" },
   { "wis"     , MIME_FLAG_CASEINSENS, "application/x-InstallShield" },
   { "wlt"     , MIME_FLAG_CASEINSENS, "application/x-mswallet" },
   { "wml"     , MIME_FLAG_CASEINSENS, "text/vnd.wap.wml" },
   { "wmlc"    , MIME_FLAG_CASEINSENS, "application/vnd.wap.wmlc" },
   { "wmv"     , MIME_FLAG_CASEINSENS, "video/x-ms-wmv" },
   { "wri"     , MIME_FLAG_CASEINSENS, "application/write" },
   { "wrl"     , MIME_FLAG_CASEINSENS, "x-world/x-vrml" },
   { "wrz"     , MIME_FLAG_CASEINSENS, "x-world/x-vrml" },
   { "wtx"     , MIME_FLAG_CASEINSENS, "audio/x-wtx" },
   { "xbm"     , MIME_FLAG_CASEINSENS, "image/x-xbitmap" },
   { "xdr"     , MIME_FLAG_CASEINSENS, "video/x-videogram" },
   { "xhtml"   , MIME_FLAG_CASEINSENS, "application/xhtml+xml" },
   { "xls"     , MIME_FLAG_CASEINSENS, "application/vnd.ms-excel" },
   { "xlsx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" },
   { "xlt"     , MIME_FLAG_CASEINSENS, "application/xlt" },
   { "xml"     , MIME_FLAG_CASEINSENS, "text/xml" },
   { "xpi"     , MIME_FLAG_CASEINSENS, "application/x-xpinstall" },
   { "xpm"     , MIME_FLAG_CASEINSENS, "image/x-xpixmap" },
   { "xwd"     , MIME_FLAG_CASEINSENS, "image/x-xwindowdump" },
   { "z"       , MIME_FLAG_CASEINSENS, "application/x-compress" },
   { "zip"     , MIME_FLAG_CASEINSENS, "application/zip" },
   { "zpa"     , MIME_FLAG_CASEINSENS, "application/pcphoto" }
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
