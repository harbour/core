/*
 * $Id$
 */

/*
 * Harbour Compatibility Library for CA-Cl*pper source code:
 * Header file to help compile Harbour source files with Clipper
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * NOTE: This file is meant to be included in CA-Clipper applications
 *       that were written to take advantage of specific Harbour extensions
 *       or platform dependent features.
 */

/*
 * TODO: Extend as needed.
 */

#ifndef __HARBOUR__

#ifndef HB_CLIP_CH_
#define HB_CLIP_CH_

/* TODO: Rewrite as much of these in C or Clipper as possible */

#xtranslate HB_ARGCHECK( <s> )                            => ( .F. )
#xtranslate HB_ARGSTRING( <s> )                           => ""
#xtranslate HB_FNAMESPLIT( <s> )                          => NIL
#xtranslate HB_FNAMEMERGE( <s1>[, <s2>[, <s3>[, <s4>]]] ) => ""
#xtranslate HB_PVALUE( <n> )                              => NIL
#xtranslate HB_SETKEYGET( [<n>][, <b>] )                  => NIL
#xtranslate HB_SETKEYSAVE( [<a>] )                        => ( {} )
#xtranslate HB_SETKEYCHECK( <n>[, <x1>[, <x2>[, <x3>]]] ) => ( .F. )
#xtranslate HB_SETKEYARRAY( <a>[, <b> ] )                 => AEval( <a>, {| tmp | SetKey( tmp, <b> ) } )

/* HB_DISKSPACE() types */
#define HB_DISK_AVAIL 0
#define HB_DISK_FREE  1
#define HB_DISK_USED  2
#define HB_DISK_TOTAL 3

/* Strong typing */

#translate AS ARRAY [OF <type>] =>
#translate AS STRING =>
#translate AS CLASS <ClassName> =>
#translate AS NUMERIC =>
#translate AS DATE =>
#translate AS CODEBLOCK =>
#translate AS OBJECT =>
#translate AS LOGICAL =>
#translate AS USUAL =>

#endif
