/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * header puller
 *
 * Copyright 2010-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* command to store header files in hash array */
#command ADD HEADER TO <hash> FILE <(cFile)> => ;
         #pragma __streaminclude <(cFile)> | <hash>\[ <(cFile)> \] := %s

FUNCTION __hbrun_CoreHeaderFiles()
   STATIC s_hHeaders

#ifdef HBRUN_WITH_HEADERS

   IF s_hHeaders == NIL
      s_hHeaders := { => }

      ADD HEADER TO s_hHeaders FILE "achoice.ch"
      ADD HEADER TO s_hHeaders FILE "assert.ch"
      ADD HEADER TO s_hHeaders FILE "blob.ch"
      ADD HEADER TO s_hHeaders FILE "box.ch"
      ADD HEADER TO s_hHeaders FILE "button.ch"
      ADD HEADER TO s_hHeaders FILE "color.ch"
      ADD HEADER TO s_hHeaders FILE "common.ch"
      ADD HEADER TO s_hHeaders FILE "dbedit.ch"
      ADD HEADER TO s_hHeaders FILE "dbinfo.ch"
      ADD HEADER TO s_hHeaders FILE "dbstruct.ch"
      ADD HEADER TO s_hHeaders FILE "directry.ch"
      ADD HEADER TO s_hHeaders FILE "error.ch"
      ADD HEADER TO s_hHeaders FILE "fileio.ch"
      ADD HEADER TO s_hHeaders FILE "getexit.ch"
      ADD HEADER TO s_hHeaders FILE "hb.ch"
      ADD HEADER TO s_hHeaders FILE "hbclass.ch"
      ADD HEADER TO s_hHeaders FILE "hbcom.ch"
      ADD HEADER TO s_hHeaders FILE "hbdebug.ch"
      ADD HEADER TO s_hHeaders FILE "hbdyn.ch"
      ADD HEADER TO s_hHeaders FILE "hbextcdp.ch"
      ADD HEADER TO s_hHeaders FILE "hbextern.ch"
      ADD HEADER TO s_hHeaders FILE "hbextlng.ch"
      ADD HEADER TO s_hHeaders FILE "hbgfx.ch"
      ADD HEADER TO s_hHeaders FILE "hbgfxdef.ch"
      ADD HEADER TO s_hHeaders FILE "hbgtinfo.ch"
      ADD HEADER TO s_hHeaders FILE "hbhash.ch"
      ADD HEADER TO s_hHeaders FILE "hbhrb.ch"
      ADD HEADER TO s_hHeaders FILE "hbinkey.ch"
      ADD HEADER TO s_hHeaders FILE "hblang.ch"
      ADD HEADER TO s_hHeaders FILE "hblpp.ch"
      ADD HEADER TO s_hHeaders FILE "hbmacro.ch"
      ADD HEADER TO s_hHeaders FILE "hbmath.ch"
      ADD HEADER TO s_hHeaders FILE "hbmemory.ch"
      ADD HEADER TO s_hHeaders FILE "hbmemvar.ch"
      ADD HEADER TO s_hHeaders FILE "hboo.ch"
      ADD HEADER TO s_hHeaders FILE "hbpers.ch"
      ADD HEADER TO s_hHeaders FILE "hbsetup.ch"
      ADD HEADER TO s_hHeaders FILE "hbsix.ch"
      ADD HEADER TO s_hHeaders FILE "hbsocket.ch"
      ADD HEADER TO s_hHeaders FILE "hbstdgen.ch"
      ADD HEADER TO s_hHeaders FILE "hbsxdef.ch"
      ADD HEADER TO s_hHeaders FILE "hbthread.ch"
      ADD HEADER TO s_hHeaders FILE "hbtrace.ch"
      ADD HEADER TO s_hHeaders FILE "hbusrrdd.ch"
      ADD HEADER TO s_hHeaders FILE "hbver.ch"
      ADD HEADER TO s_hHeaders FILE "hbzlib.ch"
      ADD HEADER TO s_hHeaders FILE "inkey.ch"
      ADD HEADER TO s_hHeaders FILE "memoedit.ch"
      ADD HEADER TO s_hHeaders FILE "ord.ch"
      ADD HEADER TO s_hHeaders FILE "rddsys.ch"
      ADD HEADER TO s_hHeaders FILE "reserved.ch"
      ADD HEADER TO s_hHeaders FILE "set.ch"
      ADD HEADER TO s_hHeaders FILE "setcurs.ch"
      ADD HEADER TO s_hHeaders FILE "simpleio.ch"
      ADD HEADER TO s_hHeaders FILE "std.ch"
      ADD HEADER TO s_hHeaders FILE "tbrowse.ch"
      ADD HEADER TO s_hHeaders FILE "harbour.hbx"
      ADD HEADER TO s_hHeaders FILE "hbcpage.hbx"
      ADD HEADER TO s_hHeaders FILE "hblang.hbx"
      ADD HEADER TO s_hHeaders FILE "hbscalar.hbx"
      ADD HEADER TO s_hHeaders FILE "hbusrrdd.hbx"

      hb_HCaseMatch( s_hHeaders, .T. )
   ENDIF

#endif /* HBRUN_WITH_HEADERS */

   RETURN s_hHeaders
