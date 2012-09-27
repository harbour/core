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

/* Inet functions */
HB_FUNC_EXTERN( HB_INETINIT                ) ; HB_FUNC( INETINIT                ) { HB_FUNC_EXEC( HB_INETINIT                ); }
HB_FUNC_EXTERN( HB_INETCLEANUP             ) ; HB_FUNC( INETCLEANUP             ) { HB_FUNC_EXEC( HB_INETCLEANUP             ); }
HB_FUNC_EXTERN( HB_INETCREATE              ) ; HB_FUNC( INETCREATE              ) { HB_FUNC_EXEC( HB_INETCREATE              ); }
HB_FUNC_EXTERN( HB_INETCLOSE               ) ; HB_FUNC( INETCLOSE               ) { HB_FUNC_EXEC( HB_INETCLOSE               ); }
HB_FUNC_EXTERN( HB_INETFD                  ) ; HB_FUNC( INETFD                  ) { HB_FUNC_EXEC( HB_INETFD                  ); }
HB_FUNC_EXTERN( HB_INETSTATUS              ) ; HB_FUNC( INETSTATUS              ) { HB_FUNC_EXEC( HB_INETSTATUS              ); }
HB_FUNC_EXTERN( HB_INETERRORCODE           ) ; HB_FUNC( INETERRORCODE           ) { HB_FUNC_EXEC( HB_INETERRORCODE           ); }
HB_FUNC_EXTERN( HB_INETERRORDESC           ) ; HB_FUNC( INETERRORDESC           ) { HB_FUNC_EXEC( HB_INETERRORDESC           ); }
HB_FUNC_EXTERN( HB_INETCLEARERROR          ) ; HB_FUNC( INETCLEARERROR          ) { HB_FUNC_EXEC( HB_INETCLEARERROR          ); }
HB_FUNC_EXTERN( HB_INETCOUNT               ) ; HB_FUNC( INETCOUNT               ) { HB_FUNC_EXEC( HB_INETCOUNT               ); }
HB_FUNC_EXTERN( HB_INETADDRESS             ) ; HB_FUNC( INETADDRESS             ) { HB_FUNC_EXEC( HB_INETADDRESS             ); }
HB_FUNC_EXTERN( HB_INETPORT                ) ; HB_FUNC( INETPORT                ) { HB_FUNC_EXEC( HB_INETPORT                ); }
HB_FUNC_EXTERN( HB_INETTIMEOUT             ) ; HB_FUNC( INETSETTIMEOUT          ) { HB_FUNC_EXEC( HB_INETTIMEOUT             ); }
HB_FUNC_EXTERN( HB_INETTIMEOUT             ) ; HB_FUNC( INETGETTIMEOUT          ) { HB_FUNC_EXEC( HB_INETTIMEOUT             ); }
HB_FUNC_EXTERN( HB_INETCLEARTIMEOUT        ) ; HB_FUNC( INETCLEARTIMEOUT        ) { HB_FUNC_EXEC( HB_INETCLEARTIMEOUT        ); }
HB_FUNC_EXTERN( HB_INETTIMELIMIT           ) ; HB_FUNC( INETSETTIMELIMIT        ) { HB_FUNC_EXEC( HB_INETTIMELIMIT           ); }
HB_FUNC_EXTERN( HB_INETTIMELIMIT           ) ; HB_FUNC( INETGETTIMELIMIT        ) { HB_FUNC_EXEC( HB_INETTIMELIMIT           ); }
HB_FUNC_EXTERN( HB_INETCLEARTIMELIMIT      ) ; HB_FUNC( INETCLEARTIMELIMIT      ) { HB_FUNC_EXEC( HB_INETCLEARTIMELIMIT      ); }
HB_FUNC_EXTERN( HB_INETPERIODCALLBACK      ) ; HB_FUNC( INETSETPERIODCALLBACK   ) { HB_FUNC_EXEC( HB_INETPERIODCALLBACK      ); }
HB_FUNC_EXTERN( HB_INETPERIODCALLBACK      ) ; HB_FUNC( INETGETPERIODCALLBACK   ) { HB_FUNC_EXEC( HB_INETPERIODCALLBACK      ); }
HB_FUNC_EXTERN( HB_INETCLEARPERIODCALLBACK ) ; HB_FUNC( INETCLEARPERIODCALLBACK ) { HB_FUNC_EXEC( HB_INETCLEARPERIODCALLBACK ); }
HB_FUNC_EXTERN( HB_INETRECV                ) ; HB_FUNC( INETRECV                ) { HB_FUNC_EXEC( HB_INETRECV                ); }
HB_FUNC_EXTERN( HB_INETRECVALL             ) ; HB_FUNC( INETRECVALL             ) { HB_FUNC_EXEC( HB_INETRECVALL             ); }
HB_FUNC_EXTERN( HB_INETRECVLINE            ) ; HB_FUNC( INETRECVLINE            ) { HB_FUNC_EXEC( HB_INETRECVLINE            ); }
HB_FUNC_EXTERN( HB_INETRECVENDBLOCK        ) ; HB_FUNC( INETRECVENDBLOCK        ) { HB_FUNC_EXEC( HB_INETRECVENDBLOCK        ); }
HB_FUNC_EXTERN( HB_INETDATAREADY           ) ; HB_FUNC( INETDATAREADY           ) { HB_FUNC_EXEC( HB_INETDATAREADY           ); }
HB_FUNC_EXTERN( HB_INETSEND                ) ; HB_FUNC( INETSEND                ) { HB_FUNC_EXEC( HB_INETSEND                ); }
HB_FUNC_EXTERN( HB_INETSENDALL             ) ; HB_FUNC( INETSENDALL             ) { HB_FUNC_EXEC( HB_INETSENDALL             ); }
HB_FUNC_EXTERN( HB_INETGETHOSTS            ) ; HB_FUNC( INETGETHOSTS            ) { HB_FUNC_EXEC( HB_INETGETHOSTS            ); }
HB_FUNC_EXTERN( HB_INETGETALIAS            ) ; HB_FUNC( INETGETALIAS            ) { HB_FUNC_EXEC( HB_INETGETALIAS            ); }
HB_FUNC_EXTERN( HB_INETSERVER              ) ; HB_FUNC( INETSERVER              ) { HB_FUNC_EXEC( HB_INETSERVER              ); }
HB_FUNC_EXTERN( HB_INETACCEPT              ) ; HB_FUNC( INETACCEPT              ) { HB_FUNC_EXEC( HB_INETACCEPT              ); }
HB_FUNC_EXTERN( HB_INETCONNECT             ) ; HB_FUNC( INETCONNECT             ) { HB_FUNC_EXEC( HB_INETCONNECT             ); }
HB_FUNC_EXTERN( HB_INETCONNECTIP           ) ; HB_FUNC( INETCONNECTIP           ) { HB_FUNC_EXEC( HB_INETCONNECTIP           ); }
HB_FUNC_EXTERN( HB_INETDGRAMBIND           ) ; HB_FUNC( INETDGRAMBIND           ) { HB_FUNC_EXEC( HB_INETDGRAMBIND           ); }
HB_FUNC_EXTERN( HB_INETDGRAM               ) ; HB_FUNC( INETDGRAM               ) { HB_FUNC_EXEC( HB_INETDGRAM               ); }
HB_FUNC_EXTERN( HB_INETDGRAMSEND           ) ; HB_FUNC( INETDGRAMSEND           ) { HB_FUNC_EXEC( HB_INETDGRAMSEND           ); }
HB_FUNC_EXTERN( HB_INETDGRAMRECV           ) ; HB_FUNC( INETDGRAMRECV           ) { HB_FUNC_EXEC( HB_INETDGRAMRECV           ); }
HB_FUNC_EXTERN( HB_INETCRLF                ) ; HB_FUNC( INETCRLF                ) { HB_FUNC_EXEC( HB_INETCRLF                ); }
HB_FUNC_EXTERN( HB_INETISSOCKET            ) ; HB_FUNC( INETISSOCKET            ) { HB_FUNC_EXEC( HB_INETISSOCKET            ); }
