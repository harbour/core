/*
 * Harbour Project source code:
 * Header file with for logging system
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

#ifndef HB_LOG_CH
#define HB_LOG_CH

#include "hblogdef.ch"

// a pretty high number  that should log any debug level
#define HB_LOG_ALL         9999

#define HB_LOG_ST_DATE     0x0001
#define HB_LOG_ST_TIME     0x0002
#define HB_LOG_ST_SECS     0x0004
#define HB_LOG_ST_LEVEL    0x0008
#define HB_LOG_ST_ISODATE  0x0010
#define HB_LOG_ST_NAME     0x0020

#xcommand INIT LOG [ON] ;
   [<fil: FILE> ([<nFilPrio> [,<cFileName>[,<nFileSize>[,<nFileCount>]]]])] ;
   [<con: CONSOLE> ([<nConPrio>])] ;
   [<mon: MONITOR> ([<nMonPrio>[,<nMonPort>]])] ;
   [<sys: SYSLOG> ([<nSysPrio>[,<nSysId>]])] ;
   [<ema: EMAIL> ([<nEmaPrio> [,<cHelo>[,<cServer>[,<cDest>[,<cSubject>[,<cFrom>]]]]]])] ;
   [<dbg: DEBUG> ( [<nDebugPrio> [,<nMaxDebugPrio>]] )] ;
   [NAME <cName>] => ;
   hb_InitStandardLog() ;;
   IF <.con.> ;;
      hb_StandardLogAdd( HB_LogConsole():New( <nConPrio> ));;
   END ;;
   IF <.fil.> ;;
      hb_StandardLogAdd( HB_LogFile():New( <nFilPrio>, <cFileName>, <nFileSize>, <nFileCount> ));;
   END ;;
   IF <.mon.> ;;
      hb_StandardLogAdd( HB_LogInetPort():New( <nMonPrio>, <nMonPort> ));;
   END ;;
   IF <.sys.> ;;
      hb_StandardLogAdd( HB_LogSysLog():New( <nSysPrio>, <nSysId> ));;
   END ;;
   IF <.ema.> ;;
      hb_StandardLogAdd( HB_LogEmail():New( <nEmaPrio> ,<cHelo>,<cServer>,<cDest>,<cSubject>,<cFrom>));;
   END ;;
   IF <.dbg.> ;;
      hb_StandardLogAdd( HB_LogDebug():New( <nDebugPrio>, <nMaxDebugPrio> ) ) ;;
   END ;;
   hb_StandardLogName( <cName> );;
   hb_OpenStandardLog()


#xcommand SET LOG STYLE <nStyle> => hb_SetStandardLogStyle( <nStyle> )

#xcommand LOG <data,...> [PRIORITY <prio>] => ;
      hb_StandardLog( hb_BldLogMsg( <data> ), <prio> )
#xcommand LOG <data,...> [PRIO <prio>] => ;
      hb_StandardLog( hb_BldLogMsg( <data> ), <prio> )

#xcommand CLOSE LOG =>  hb_CloseStandardLog()

#xtranslate PRIO[RITY] DEFAULT  => PRIO HB_LOG_DEFAULT
#xtranslate PRIO[RITY] CRITICAL => PRIO HB_LOG_CRITICAL
#xtranslate PRIO[RITY] ERROR => PRIO HB_LOG_ERROR
#xtranslate PRIO[RITY] WARNING => PRIO HB_LOG_WARNING
#xtranslate PRIO[RITY] WARN => PRIO HB_LOG_WARN
#xtranslate PRIO[RITY] INFO => PRIO HB_LOG_INFO
#xtranslate PRIO[RITY] DEBUG => PRIO HB_LOG_DEBUG

#endif /* HB_LOGDEF_CH */
