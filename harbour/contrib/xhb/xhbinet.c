/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * xhb compatibility wrappers.
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.  If not, write to
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

#include "hbapi.h"

HB_FUNC_TRANSLATE( INETINIT                , HB_INETINIT                )
HB_FUNC_TRANSLATE( INETCLEANUP             , HB_INETCLEANUP             )
HB_FUNC_TRANSLATE( INETCREATE              , HB_INETCREATE              )
HB_FUNC_TRANSLATE( INETCLOSE               , HB_INETCLOSE               )
HB_FUNC_TRANSLATE( INETFD                  , HB_INETFD                  )
HB_FUNC_TRANSLATE( INETSTATUS              , HB_INETSTATUS              )
HB_FUNC_TRANSLATE( INETERRORCODE           , HB_INETERRORCODE           )
HB_FUNC_TRANSLATE( INETERRORDESC           , HB_INETERRORDESC           )
HB_FUNC_TRANSLATE( INETCLEARERROR          , HB_INETCLEARERROR          )
HB_FUNC_TRANSLATE( INETCOUNT               , HB_INETCOUNT               )
HB_FUNC_TRANSLATE( INETADDRESS             , HB_INETADDRESS             )
HB_FUNC_TRANSLATE( INETPORT                , HB_INETPORT                )
HB_FUNC_TRANSLATE( INETSETTIMEOUT          , HB_INETTIMEOUT             )
HB_FUNC_TRANSLATE( INETGETTIMEOUT          , HB_INETTIMEOUT             )
HB_FUNC_TRANSLATE( INETCLEARTIMEOUT        , HB_INETCLEARTIMEOUT        )
HB_FUNC_TRANSLATE( INETSETTIMELIMIT        , HB_INETTIMELIMIT           )
HB_FUNC_TRANSLATE( INETGETTIMELIMIT        , HB_INETTIMELIMIT           )
HB_FUNC_TRANSLATE( INETCLEARTIMELIMIT      , HB_INETCLEARTIMELIMIT      )
HB_FUNC_TRANSLATE( INETSETPERIODCALLBACK   , HB_INETPERIODCALLBACK      )
HB_FUNC_TRANSLATE( INETGETPERIODCALLBACK   , HB_INETPERIODCALLBACK      )
HB_FUNC_TRANSLATE( INETCLEARPERIODCALLBACK , HB_INETCLEARPERIODCALLBACK )
HB_FUNC_TRANSLATE( INETRECV                , HB_INETRECV                )
HB_FUNC_TRANSLATE( INETRECVALL             , HB_INETRECVALL             )
HB_FUNC_TRANSLATE( INETRECVLINE            , HB_INETRECVLINE            )
HB_FUNC_TRANSLATE( INETRECVENDBLOCK        , HB_INETRECVENDBLOCK        )
HB_FUNC_TRANSLATE( INETDATAREADY           , HB_INETDATAREADY           )
HB_FUNC_TRANSLATE( INETSEND                , HB_INETSEND                )
HB_FUNC_TRANSLATE( INETSENDALL             , HB_INETSENDALL             )
HB_FUNC_TRANSLATE( INETGETHOSTS            , HB_INETGETHOSTS            )
HB_FUNC_TRANSLATE( INETGETALIAS            , HB_INETGETALIAS            )
HB_FUNC_TRANSLATE( INETSERVER              , HB_INETSERVER              )
HB_FUNC_TRANSLATE( INETACCEPT              , HB_INETACCEPT              )
HB_FUNC_TRANSLATE( INETCONNECT             , HB_INETCONNECT             )
HB_FUNC_TRANSLATE( INETCONNECTIP           , HB_INETCONNECTIP           )
HB_FUNC_TRANSLATE( INETDGRAMBIND           , HB_INETDGRAMBIND           )
HB_FUNC_TRANSLATE( INETDGRAM               , HB_INETDGRAM               )
HB_FUNC_TRANSLATE( INETDGRAMSEND           , HB_INETDGRAMSEND           )
HB_FUNC_TRANSLATE( INETDGRAMRECV           , HB_INETDGRAMRECV           )
HB_FUNC_TRANSLATE( INETCRLF                , HB_INETCRLF                )
HB_FUNC_TRANSLATE( INETISSOCKET            , HB_INETISSOCKET            )
