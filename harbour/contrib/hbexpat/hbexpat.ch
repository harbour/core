/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * expat API - Harbour header
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

/* NOTE: This file is also used by C code. */

#ifndef HBEXPAT_CH_
#define HBEXPAT_CH_

#define HB_XML_STATUS_ERROR                             0
#define HB_XML_STATUS_OK                                1

#define HB_XML_ERROR_NOT_IMPLEMENTED_                   ( -1 ) /* Harbour specific */
#define HB_XML_ERROR_NONE                               0
#define HB_XML_ERROR_NO_MEMORY                          1
#define HB_XML_ERROR_SYNTAX                             2
#define HB_XML_ERROR_NO_ELEMENTS                        3
#define HB_XML_ERROR_INVALID_TOKEN                      4
#define HB_XML_ERROR_UNCLOSED_TOKEN                     5
#define HB_XML_ERROR_PARTIAL_CHAR                       6
#define HB_XML_ERROR_TAG_MISMATCH                       7
#define HB_XML_ERROR_DUPLICATE_ATTRIBUTE                8
#define HB_XML_ERROR_JUNK_AFTER_DOC_ELEMENT             9
#define HB_XML_ERROR_PARAM_ENTITY_REF                   10
#define HB_XML_ERROR_UNDEFINED_ENTITY                   11
#define HB_XML_ERROR_RECURSIVE_ENTITY_REF               12
#define HB_XML_ERROR_ASYNC_ENTITY                       13
#define HB_XML_ERROR_BAD_CHAR_REF                       14
#define HB_XML_ERROR_BINARY_ENTITY_REF                  15
#define HB_XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF      16
#define HB_XML_ERROR_MISPLACED_XML_PI                   17
#define HB_XML_ERROR_UNKNOWN_ENCODING                   18
#define HB_XML_ERROR_INCORRECT_ENCODING                 19
#define HB_XML_ERROR_UNCLOSED_CDATA_SECTION             20
#define HB_XML_ERROR_EXTERNAL_ENTITY_HANDLING           21
#define HB_XML_ERROR_NOT_STANDALONE                     22
#define HB_XML_ERROR_UNEXPECTED_STATE                   23
#define HB_XML_ERROR_ENTITY_DECLARED_IN_PE              24
#define HB_XML_ERROR_FEATURE_REQUIRES_XML_DTD           25
#define HB_XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING   26
#define HB_XML_ERROR_UNBOUND_PREFIX                     27
#define HB_XML_ERROR_UNDECLARING_PREFIX                 28
#define HB_XML_ERROR_INCOMPLETE_PE                      29
#define HB_XML_ERROR_XML_DECL                           30
#define HB_XML_ERROR_TEXT_DECL                          31
#define HB_XML_ERROR_PUBLICID                           32
#define HB_XML_ERROR_SUSPENDED                          33
#define HB_XML_ERROR_NOT_SUSPENDED                      34
#define HB_XML_ERROR_ABORTED                            35
#define HB_XML_ERROR_FINISHED                           36
#define HB_XML_ERROR_SUSPEND_PE                         37
#define HB_XML_ERROR_RESERVED_PREFIX_XML                38
#define HB_XML_ERROR_RESERVED_PREFIX_XMLNS              39
#define HB_XML_ERROR_RESERVED_NAMESPACE_URI             40

#define HB_XML_INITIALIZED                              0
#define HB_XML_PARSING                                  1
#define HB_XML_FINISHED                                 2
#define HB_XML_SUSPENDED                                3

#define HB_XML_PARAM_ENTITY_PARSING_NEVER               0
#define HB_XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE   1
#define HB_XML_PARAM_ENTITY_PARSING_ALWAYS              2

#define HB_XML_ATTR_cName                               1
#define HB_XML_ATTR_cValue                              2

#endif /* HBEXPAT_CH_ */
