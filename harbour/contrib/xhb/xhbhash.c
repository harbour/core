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

#include "hbapiitm.h"

HB_FUNC_TRANSLATE( HASH          , HB_HASH          )
HB_FUNC_TRANSLATE( HHASKEY       , HB_HHASKEY       )
HB_FUNC_TRANSLATE( HGETPOS       , HB_HPOS          )
HB_FUNC_TRANSLATE( HGET          , HB_HGET          )
HB_FUNC_TRANSLATE( HSET          , HB_HSET          )
HB_FUNC_TRANSLATE( HDEL          , HB_HDEL          )
HB_FUNC_TRANSLATE( HGETKEYAT     , HB_HKEYAT        )
HB_FUNC_TRANSLATE( HGETVALUEAT   , HB_HVALUEAT      )
HB_FUNC_TRANSLATE( HSETVALUEAT   , HB_HVALUEAT      )
HB_FUNC_TRANSLATE( HGETPAIRAT    , HB_HPAIRAT       )
HB_FUNC_TRANSLATE( HDELAT        , HB_HDELAT        )
HB_FUNC_TRANSLATE( HGETKEYS      , HB_HKEYS         )
HB_FUNC_TRANSLATE( HGETVALUES    , HB_HVALUES       )
HB_FUNC_TRANSLATE( HFILL         , HB_HFILL         )
HB_FUNC_TRANSLATE( HCLONE        , HB_HCLONE        )
HB_FUNC_TRANSLATE( HCOPY         , HB_HCOPY         )
HB_FUNC_TRANSLATE( HMERGE        , HB_HMERGE        )
HB_FUNC_TRANSLATE( HEVAL         , HB_HEVAL         )
HB_FUNC_TRANSLATE( HSCAN         , HB_HSCAN         )
HB_FUNC_TRANSLATE( HALLOCATE     , HB_HALLOCATE     )
HB_FUNC_TRANSLATE( HDEFAULT      , HB_HDEFAULT      )
HB_FUNC_TRANSLATE( HGETCASEMATCH , HB_HCASEMATCH    )
HB_FUNC_TRANSLATE( HSETCASEMATCH , HB_HSETCASEMATCH )
HB_FUNC_TRANSLATE( HSETAUTOADD   , HB_SETAUTOADD    )

HB_FUNC_EXTERN( HB_HAUTOADD   ) ; HB_FUNC( HGETAUTOADD   ) { HB_FUNC_EXEC( HB_HAUTOADD ); hb_retl( hb_parni( -1 ) == HB_HASH_AUTOADD_ALWAYS ); }
