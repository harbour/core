/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FERROR.CH Internal HTMLLIB module
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef _FERROR_CH_

#define _LAST_IO_ERROR IF(FError() == 0, "", _IO_ERRORS[Ferror()] )

#define _IO_ERRORS  { ;
                    "",                                                   ; //1
                    "File not found",                                     ; //2
                    "Path not found",                                     ; //3
                    "Too many files open",                                ; //4
                    "Access denied",                                      ; //5
                    "Invalid handle",                                     ; //6
                    "",                                                   ; //7
                    "Insufficient memory",                                ; //8
                    "",                                                   ; //9
                    "",                                                   ; //10
                    "",                                                   ; //11
                    "",                                                   ; //12
                    "",                                                   ; //13
                    "",                                                   ; //14
                    "Invalid drive specified",                            ; //15
                    "",                                                   ; //16
                    "",                                                   ; //17
                    "",                                                   ; //18
                    "Attempted to write to a write-protected disk",       ; //19
                    "",                                                   ; //20
                    "Drive not ready",                                    ; //21
                    "",                                                   ; //22
                    "Data CRC error",                                     ; //23
                    "",                                                   ; //24
                    "",                                                   ; //25
                    "",                                                   ; //26
                    "",                                                   ; //27
                    "",                                                   ; //28
                    "Write fault",                                        ; //29
                    "Read fault",                                         ; //30
                    "",                                                   ; //31
                    "Sharing violation",                                  ; //32
                    "Lock Violation"                                      ; //33
                     }

#define _FERROR_CH_
#endif
