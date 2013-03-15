/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    platform independent task system. It's used when when OS does not
 *    support threads
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_TASK_H_
#define HB_TASK_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

extern void    hb_taskInit( void );
extern void    hb_taskExit( void );
extern void *  hb_taskCreate( void * ( * ) ( void * ), void *, long );
extern void    hb_taskDestroy( void * );
extern void    hb_taskYield( void );
extern void    hb_taskSheduler( void );
extern void    hb_taskSuspend( void );
extern void    hb_taskResume( void * );
extern void    hb_taskSleep( unsigned long );
extern void *  hb_taskMain( void );
extern void *  hb_taskSelf( void );
extern int     hb_taskID( void * );
extern void    hb_taskSetData( void * );
extern void *  hb_taskGetData( void );
extern void    hb_taskSaveData( void *, void * );
extern void *  hb_taskRestoreData( void * );
extern void *  hb_taskResult( void * );
extern int     hb_taskJoin( void *, unsigned long, void ** );
extern void    hb_taskDetach( void * );
extern void    hb_taskQuit( void * );
extern int     hb_taskLock( void **, unsigned long );
extern void    hb_taskUnlock( void ** );
extern void    hb_taskSignal( void ** cond );
extern void    hb_taskBroadcast( void ** cond );
extern int     hb_taskWait( void ** cond, void ** mutex, unsigned long ulMilliSec );
extern void    hb_taskDestroyMutex( void ** );
extern void    hb_taskDestroyCond( void ** );

#define HB_TASK_INFINITE_WAIT       ( ( unsigned long ) -1 )

HB_EXTERN_END

#endif /* HB_TASK_H_ */
