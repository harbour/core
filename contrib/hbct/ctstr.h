/*
 * Harbour Project source code:
 *   internal function header for CT3 string functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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


#ifndef _CTSTR_H
#define _CTSTR_H  1

HB_EXTERN_BEGIN

extern int ct_str_init( void );
extern int ct_str_exit( void );

extern const char * ct_at_exact_forward( const char * pcString, HB_SIZE sStrLen,
                                         const char * pcMatch, HB_SIZE sMatchLen,
                                         HB_SIZE * psMatchStrLen );
extern const char * ct_at_exact_backward( const char * pcString, HB_SIZE sStrLen,
                                          const char * pcMatch, HB_SIZE sMatchLen,
                                          HB_SIZE * psMatchStrLen );
extern const char * ct_at_wildcard_forward( const char * pcString, HB_SIZE sStrLen,
                                            const char * pcMatch, HB_SIZE sMatchLen,
                                            char cWildCard, HB_SIZE * psMatchStrLen );
extern const char * ct_at_wildcard_backward( const char * pcString, HB_SIZE sStrLen,
                                             const char * pcMatch, HB_SIZE sMatchLen,
                                             char cWildCard, HB_SIZE * psMatchStrLen );
extern const char * ct_at_charset_forward( const char * pcString, HB_SIZE sStrLen,
                                           const char * pcCharSet, HB_SIZE sCharSetLen,
                                           HB_SIZE * psMatchedCharPos );
extern const char * ct_at_charset_backward( const char * pcString, HB_SIZE sStrLen,
                                            const char * pcCharSet, HB_SIZE sCharSetLen,
                                            HB_SIZE * psMatchedCharPos );

extern void ct_setref( int iNewSwitch );
extern int  ct_getref( void );
extern void ct_setatmupa( int iNewSwitch );
extern int  ct_getatmupa( void );
extern void ct_setatlike( int iNewSwitch );
extern int  ct_getatlike( void );
extern void ct_setatlikechar( char cNewChar );
extern char ct_getatlikechar( void );

#define CT_SETATLIKE_EXACT     0
#define CT_SETATLIKE_WILDCARD  1

HB_EXTERN_END

#endif
