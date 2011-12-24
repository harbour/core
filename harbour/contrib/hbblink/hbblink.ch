/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Blinker compatibility functions.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu)
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

#ifndef BLINKER_CH_
#define BLINKER_CH_

#xtranslate BLINKER EXECUTABLE SERIAL <cString>  => INIT PROCEDURE _init_hb_blivernum() ; HB_BLIVERNUM( <cString> ) ; RETURN
#xtranslate BLINKER DEMONSTRATION DATE <cDate>   => INIT PROCEDURE _init_hb_blidemdte() ; HB_BLIDEMDTE( hb_SToD( <cDate> ) ) ; RETURN
#xtranslate BLINKER DEMONSTRATION MINUTES <nMin> => INIT PROCEDURE _init_hb_blidemmin() ; HB_BLIDEMMIN( <nMin> ) ; RETURN

/* BLIMGRSTS() parameters */
#define BliCacheLoc         1  /* Get location of real mode overlay cache (EMS/XMS) */
#define BliCacheSize        2  /* Get size of overlay cache */
#define BliExtMemAvail      3  /* Get bytes extended memory available to the extender */
#define BliHostMode         4  /* Get DOS extender host mode (DPMI/VCPI/XMS) */
#define BliMachineMode      5  /* Get current machine mode (real, protected) */
#define BliOverlayLoc       6  /* Get location of overlay area */
#define BliOverlaySize      7  /* Get size of overlay area */
#define BliRealMemAvail     8  /* Get bytes real memory available to the extender */
#define BliVirMemAvail      9  /* Get bytes virtual memory available to the extender */

/* BLIMGRSTS() BliCacheLoc values */
#define BliCacheNone        0  /* No overlay cache */
#define BliCacheEMS         1  /* Overlay cache is in EMS */
#define BliCacheXMS         2  /* Overlay cache is in XMS */

/* BLIMGRSTS() BliHostMode values */
#define BliHostNone         0
#define BliHostDPMI         1
#define BliHostVCPI         2
#define BliHostXMS          3

/* BLIMGRSTS() BliMachineMode values */
#define BliModeReal         0
#define BliMode286Prot      1

#endif /* BLINKER_CH_ */
