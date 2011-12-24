/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility calls (OS version support).
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.hu)
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

#if defined( HB_OS_WIN )

HB_FUNC_EXTERN( WIN_OSVERSIONINFO    ) ; HB_FUNC( OS_VERSIONINFO         ) { HB_FUNC_EXEC( WIN_OSVERSIONINFO    ); }
HB_FUNC_EXTERN( WIN_OSISNT           ) ; HB_FUNC( OS_ISWINNT             ) { HB_FUNC_EXEC( WIN_OSISNT           ); }
HB_FUNC_EXTERN( WIN_OSISNT351        ) ; HB_FUNC( OS_ISWINNT351          ) { HB_FUNC_EXEC( WIN_OSISNT351        ); }
HB_FUNC_EXTERN( WIN_OSISNT4          ) ; HB_FUNC( OS_ISWINNT4            ) { HB_FUNC_EXEC( WIN_OSISNT4          ); }
HB_FUNC_EXTERN( WIN_OSIS2000ORUPPER  ) ; HB_FUNC( OS_ISWIN2000_OR_LATER  ) { HB_FUNC_EXEC( WIN_OSIS2000ORUPPER  ); }
HB_FUNC_EXTERN( WIN_OSIS2000         ) ; HB_FUNC( OS_ISWIN2000           ) { HB_FUNC_EXEC( WIN_OSIS2000         ); }
HB_FUNC_EXTERN( WIN_OSISXP           ) ; HB_FUNC( OS_ISWINXP             ) { HB_FUNC_EXEC( WIN_OSISXP           ); }
HB_FUNC_EXTERN( WIN_OSISWINXPORUPPER ) ; HB_FUNC( OS_ISWINXP_OR_LATER    ) { HB_FUNC_EXEC( WIN_OSISWINXPORUPPER ); }
HB_FUNC_EXTERN( WIN_OSIS2003         ) ; HB_FUNC( OS_ISWIN2003           ) { HB_FUNC_EXEC( WIN_OSIS2003         ); }
HB_FUNC_EXTERN( WIN_OSISVISTA        ) ; HB_FUNC( OS_ISWINVISTA          ) { HB_FUNC_EXEC( WIN_OSISVISTA        ); }
HB_FUNC_EXTERN( WIN_OSISVISTAORUPPER ) ; HB_FUNC( OS_ISWINVISTA_OR_LATER ) { HB_FUNC_EXEC( WIN_OSISVISTAORUPPER ); }
HB_FUNC_EXTERN( WIN_OSIS7            ) ; HB_FUNC( OS_ISWIN7              ) { HB_FUNC_EXEC( WIN_OSIS7            ); }
HB_FUNC_EXTERN( WIN_OSIS9X           ) ; HB_FUNC( OS_ISWIN9X             ) { HB_FUNC_EXEC( WIN_OSIS9X           ); }
HB_FUNC_EXTERN( WIN_OSIS95           ) ; HB_FUNC( OS_ISWIN95             ) { HB_FUNC_EXEC( WIN_OSIS95           ); }
HB_FUNC_EXTERN( WIN_OSIS98           ) ; HB_FUNC( OS_ISWIN98             ) { HB_FUNC_EXEC( WIN_OSIS98           ); }
HB_FUNC_EXTERN( WIN_OSISME           ) ; HB_FUNC( OS_ISWINME             ) { HB_FUNC_EXEC( WIN_OSISME           ); }
HB_FUNC_EXTERN( WIN_OSISTSCLIENT     ) ; HB_FUNC( OS_ISWTSCLIENT         ) { HB_FUNC_EXEC( WIN_OSISTSCLIENT     ); }
HB_FUNC_EXTERN( WIN_OSNETREGOK       ) ; HB_FUNC( OS_NETREGOK            ) { HB_FUNC_EXEC( WIN_OSNETREGOK       ); }
HB_FUNC_EXTERN( WIN_OSNETVREDIROK    ) ; HB_FUNC( OS_NETVREDIROK         ) { HB_FUNC_EXEC( WIN_OSNETVREDIROK    ); }

#else

HB_FUNC( OS_VERSIONINFO         ) {}
HB_FUNC( OS_ISWINNT             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINNT351          ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINNT4            ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN2000_OR_LATER  ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN2000           ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINXP             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINXP_OR_LATER    ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN2003           ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINVISTA          ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINVISTA_OR_LATER ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN7              ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN9X             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN95             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWIN98             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWINME             ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_ISWTSCLIENT         ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_NETREGOK            ) { hb_retl( HB_FALSE ); }
HB_FUNC( OS_NETVREDIROK         ) { hb_retl( HB_FALSE ); }

#endif
