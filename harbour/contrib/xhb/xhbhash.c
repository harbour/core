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

/* Hash item functions */
HB_FUNC_EXTERN( HB_HASH          ) ; HB_FUNC( HASH          ) { HB_FUNC_EXEC( HB_HASH          ); }
HB_FUNC_EXTERN( HB_HHASKEY       ) ; HB_FUNC( HHASKEY       ) { HB_FUNC_EXEC( HB_HHASKEY       ); }
HB_FUNC_EXTERN( HB_HPOS          ) ; HB_FUNC( HGETPOS       ) { HB_FUNC_EXEC( HB_HPOS          ); }
HB_FUNC_EXTERN( HB_HGET          ) ; HB_FUNC( HGET          ) { HB_FUNC_EXEC( HB_HGET          ); }
HB_FUNC_EXTERN( HB_HSET          ) ; HB_FUNC( HSET          ) { HB_FUNC_EXEC( HB_HSET          ); }
HB_FUNC_EXTERN( HB_HDEL          ) ; HB_FUNC( HDEL          ) { HB_FUNC_EXEC( HB_HDEL          ); }
HB_FUNC_EXTERN( HB_HKEYAT        ) ; HB_FUNC( HGETKEYAT     ) { HB_FUNC_EXEC( HB_HKEYAT        ); }
HB_FUNC_EXTERN( HB_HVALUEAT      ) ; HB_FUNC( HGETVALUEAT   ) { HB_FUNC_EXEC( HB_HVALUEAT      ); }
HB_FUNC_EXTERN( HB_HVALUEAT      ) ; HB_FUNC( HSETVALUEAT   ) { HB_FUNC_EXEC( HB_HVALUEAT      ); }
HB_FUNC_EXTERN( HB_HPAIRAT       ) ; HB_FUNC( HGETPAIRAT    ) { HB_FUNC_EXEC( HB_HPAIRAT       ); }
HB_FUNC_EXTERN( HB_HDELAT        ) ; HB_FUNC( HDELAT        ) { HB_FUNC_EXEC( HB_HDELAT        ); }
HB_FUNC_EXTERN( HB_HKEYS         ) ; HB_FUNC( HGETKEYS      ) { HB_FUNC_EXEC( HB_HKEYS         ); }
HB_FUNC_EXTERN( HB_HVALUES       ) ; HB_FUNC( HGETVALUES    ) { HB_FUNC_EXEC( HB_HVALUES       ); }
HB_FUNC_EXTERN( HB_HFILL         ) ; HB_FUNC( HFILL         ) { HB_FUNC_EXEC( HB_HFILL         ); }
HB_FUNC_EXTERN( HB_HCLONE        ) ; HB_FUNC( HCLONE        ) { HB_FUNC_EXEC( HB_HCLONE        ); }
HB_FUNC_EXTERN( HB_HCOPY         ) ; HB_FUNC( HCOPY         ) { HB_FUNC_EXEC( HB_HCOPY         ); }
HB_FUNC_EXTERN( HB_HMERGE        ) ; HB_FUNC( HMERGE        ) { HB_FUNC_EXEC( HB_HMERGE        ); }
HB_FUNC_EXTERN( HB_HEVAL         ) ; HB_FUNC( HEVAL         ) { HB_FUNC_EXEC( HB_HEVAL         ); }
HB_FUNC_EXTERN( HB_HSCAN         ) ; HB_FUNC( HSCAN         ) { HB_FUNC_EXEC( HB_HSCAN         ); }
HB_FUNC_EXTERN( HB_HALLOCATE     ) ; HB_FUNC( HALLOCATE     ) { HB_FUNC_EXEC( HB_HALLOCATE     ); }
HB_FUNC_EXTERN( HB_HDEFAULT      ) ; HB_FUNC( HDEFAULT      ) { HB_FUNC_EXEC( HB_HDEFAULT      ); }
HB_FUNC_EXTERN( HB_HCASEMATCH    ) ; HB_FUNC( HSETCASEMATCH ) { HB_FUNC_EXEC( HB_HCASEMATCH ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC_EXTERN( HB_HCASEMATCH    ) ; HB_FUNC( HGETCASEMATCH ) { HB_FUNC_EXEC( HB_HCASEMATCH    ); }
HB_FUNC_EXTERN( HB_HAUTOADD      ) ; HB_FUNC( HSETAUTOADD   ) { HB_FUNC_EXEC( HB_HAUTOADD ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC_EXTERN( HB_HAUTOADD      ) ; HB_FUNC( HGETAUTOADD   ) { HB_FUNC_EXEC( HB_HAUTOADD ); hb_retl( hb_parni( -1 ) == HB_HASH_AUTOADD_ALWAYS ); }
