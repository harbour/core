/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (ES)
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

/* Language name: Spanish */
/* ISO language code (2 chars): ES */
/* Codepage: ???? */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */
   
      "ES",                        /* ID */
      "Spanish",                   /* Name (in English) */
      "Espa¤ol",                   /* Name (in native language) */
      "ES",                        /* RFC ID */
      "850",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */
   
      /* Month names */
   
      "Enero",
      "Febrero",
      "Marzo",
      "Abril",
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Septiembre",
      "Octubre",
      "Noviembre",
      "Diciembre",
   
      /* Day names */

      "Domingo",
      "Lunes",
      "Martes",
      "Mi‚rcoles",
      "Jueves",
      "Viernes",
      "S bado",
   
      /* CA-Cl*pper compatible natmsg items */
   
      "Database Files    # Records    Last Update     Size",
      "Do you want more samples?",
      "Page No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Invalid date",
      "Range: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",
   
      /* Error description names */
   
      "Error desconocido",
      "Error de argumento",
      "Error de rango",
      "Desbordamiento en cadena de caracteres",
      "Desbordamiento num‚rico",
      "Divisi¢n por cero",
      "Error num‚rico",
      "Error de sintaxis",
      "Operaci¢n demasiado compleja",
      "",
      "",
      "Poca memoria",
      "Funci¢n no definida",
      "No existe el m‚todo",
      "No existe la variable",
      "No existe el alias",
      "No existe la variable de instancia",
      "Alias con caracteres no v lidos",
      "Alias actualmente en uso",
      "",
      "Error de creaci¢n",
      "Error de apertura",
      "Error de cierre",
      "Error de lectura",
      "Error de escritura",
      "Error de impresi¢n",
      "",
      "",
      "",
      "",
      "Operaci¢n no soportada",
      "L¡mite excedido",
      "Se detect¢ corrupci¢n",
      "Error de tipo de datos",
      "Error de anchura de datos",
      "Area de trabajo no usada",
      "Area de trabajo no indexada",
      "Se requiere uso exclusivo",
      "Se requiere bloqueo",
      "Escritura no autorizada",
      "Fallo en el bloqueo de adici¢n",
      "Fallo en bloqueo",
      "",
      "",
      "",
      "",
      "array access",
      "array assign",
      "array dimension", 
      "not an array",
      "conditional",
   
      /* Internal error names */
   
      "Unrecoverable error %lu: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error", 
      "Too many recursive error handler calls", 
      "RDD invalid or failed to load",
      "Invalid method type from %s", 
      "hb_xgrab can't allocate memory", 
      "hb_xrealloc called with a NULL pointer", 
      "hb_xrealloc called with an invalid pointer", 
      "hb_xrealloc can't reallocate memory", 
      "hb_xfree called with an invalid pointer", 
      "hb_xfree called with a NULL pointer", 
      "Can\'t locate the starting procedure: \'%s\'",
      "No starting procedure", 
      "Unsupported VM opcode", 
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s", 
      "Codeblock expected from %s", 
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow", 
      "An item was going to be copied to itself from %s", 
      "Invalid symbol item passed as memvar %s",
   
      /* Texts */
   
      "DD/MM/YYYY",
      "S",
      "N"
   }
};

HB_LANG_ANNOUNCE( ES );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_ES )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_ES )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_ES
#endif

