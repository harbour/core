/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBColumn Class
 *
 * Copyright 2007-2008 Viktor Szakats (harbour syenar.net)
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbclass.ch"

#include "tbrowse.ch"

/* NOTE: In CA-Cl*pper TBCOLUMN class does not inherit from any other classes. */

CREATE CLASS TBCOLUMN

   EXPORTED:

   /* === Start of CA-Cl*pper compatible TBColumn instance area === */
   VAR cargo                                         /* 01. User-definable variable */
   VAR nWidth       PROTECTED                        /* 02. */
   VAR bBlock       PROTECTED                        /* 03. */
   VAR aDefColor    PROTECTED INIT { 1, 2 }          /* 04. NOTE: Default value for both CA-Cl*pper 5.2 and 5.3. */
   VAR bColorBlock  PROTECTED INIT {|| NIL }         /* 05. */
   VAR cHeading     PROTECTED INIT ""                /* 06. */
   VAR cHeadSep     PROTECTED                        /* 07. */
   VAR cColSep      PROTECTED                        /* 08. */
   VAR cFootSep     PROTECTED                        /* 09. */
   VAR cFooting     PROTECTED INIT ""                /* 10. */
   VAR picture                                       /* 11. Column picture string */
#ifdef HB_COMPAT_C53
   VAR bPreBlock    PROTECTED                        /* 12. */
   VAR bPostBlock   PROTECTED                        /* 13. */
   VAR aSetStyle    PROTECTED INIT { .F., .F., .F. } /* 14. TBC_READWRITE, TBC_MOVE, TBC_SIZE */
#endif
   /* === End of CA-Cl*pper compatible TBColumn instance area === */

   METHOD block( bBlock ) SETGET                     /* Code block to retrieve data for the column */
   METHOD colorBlock( bColorBlock ) SETGET           /* Code block that determines color of data items */
   METHOD defColor( aDefColor ) SETGET               /* Array of numeric indexes into the color table */
   METHOD colSep( cColSep ) SETGET                   /* Column separator character */
   METHOD heading( cHeading ) SETGET                 /* Column heading */
   METHOD footing( cFooting ) SETGET                 /* Column footing */
   METHOD headSep( cHeadSep ) SETGET                 /* Heading separator character */
   METHOD footSep( cFootSep ) SETGET                 /* Footing separator character */
   METHOD width( nWidth ) SETGET                     /* Column display width */
#ifdef HB_COMPAT_C53
   METHOD preBlock( bPreBlock ) SETGET               /* Code block determining editing */
   METHOD postBlock( bPostBlock ) SETGET             /* Code block validating values */
   METHOD setStyle( nStyle, lNewValue )
#endif

   METHOD New( cHeading, bBlock )                    /* NOTE: This method is a Harbour extension [vszakats] */

ENDCLASS

METHOD block( bBlock ) CLASS TBCOLUMN

   IF bBlock != NIL
      ::bBlock := __eInstVar53( Self, "BLOCK", bBlock, "B", 1001 )
   ENDIF

   RETURN ::bBlock

METHOD colorBlock( bColorBlock ) CLASS TBCOLUMN

   IF bColorBlock != NIL
      ::bColorBlock := __eInstVar53( Self, "COLORBLOCK", bColorBlock, "B", 1001 )
   ENDIF

   RETURN ::bColorBlock

METHOD defColor( aDefColor ) CLASS TBCOLUMN

   IF aDefColor != NIL
      ::aDefColor := __eInstVar53( Self, "DEFCOLOR", aDefColor, "A", 1001 )
   ENDIF

   RETURN ::aDefColor

METHOD colSep( cColSep ) CLASS TBCOLUMN

   IF cColSep != NIL
      ::cColSep := __eInstVar53( Self, "COLSEP", cColSep, "C", 1001 )
   ENDIF

   RETURN ::cColSep

METHOD heading( cHeading ) CLASS TBCOLUMN

   IF cHeading != NIL
      ::cHeading := __eInstVar53( Self, "HEADING", cHeading, "C", 1001 )
   ENDIF

   RETURN ::cHeading

METHOD footing( cFooting ) CLASS TBCOLUMN

   IF cFooting != NIL
      ::cFooting := __eInstVar53( Self, "FOOTING", cFooting, "C", 1001 )
   ENDIF

   RETURN ::cFooting

METHOD headSep( cHeadSep ) CLASS TBCOLUMN

   IF cHeadSep != NIL
      ::cHeadSep := __eInstVar53( Self, "HEADSEP", cHeadSep, "C", 1001 )
   ENDIF

   RETURN ::cHeadSep

METHOD footSep( cFootSep ) CLASS TBCOLUMN

   IF cFootSep != NIL
      ::cFootSep := __eInstVar53( Self, "FOOTSEP", cFootSep, "C", 1001 )
   ENDIF

   RETURN ::cFootSep

METHOD width( nWidth ) CLASS TBCOLUMN

   IF nWidth != NIL
      ::nWidth := __eInstVar53( Self, "WIDTH", nWidth, "N", 1001 )
   ENDIF

   RETURN ::nWidth

#ifdef HB_COMPAT_C53

METHOD preBlock( bPreBlock ) CLASS TBCOLUMN

   IF bPreBlock != NIL
      ::bPreBlock := __eInstVar53( Self, "PREBLOCK", bPreBlock, "B", 1001 )
   ENDIF

   RETURN ::bPreBlock

METHOD postBlock( bPostBlock ) CLASS TBCOLUMN

   IF bPostBlock != NIL
      ::bPostBlock := __eInstVar53( Self, "POSTBLOCK", bPostBlock, "B", 1001 )
   ENDIF

   RETURN ::bPostBlock

METHOD setStyle( nStyle, lNewValue ) CLASS TBCOLUMN

   /* NOTE: CA-Cl*pper 5.3 does no checks on the value of nStyle, so in case
            it is zero or non-numeric, a regular RTE will happen. [vszakats] */

   IF nStyle > Len( ::aSetStyle ) .AND. nStyle <= 4096 /* Some reasonable limit for maximum number of styles */
      ASize( ::aSetStyle, nStyle )
   ENDIF

   IF HB_ISLOGICAL( lNewValue )
      ::aSetStyle[ nStyle ] := lNewValue
   ENDIF

   RETURN ::aSetStyle[ nStyle ]

#endif

METHOD New( cHeading, bBlock ) CLASS TBCOLUMN

   ::cHeading := cHeading /* NOTE: CA-Cl*pper will allow any types for the heading here. [vszakats] */
   ::bBlock := bBlock /* NOTE: CA-Cl*pper allows any types here. [vszakats] */

   RETURN Self

FUNCTION TBColumnNew( cHeading, bBlock )
   RETURN TBColumn():New( cHeading, bBlock )
