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
/* Codepage: 850 */

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
   
      "Bases de Datos    # Records    Last Update     Size",
      "Desea Ud. m s ejemplos?",
      "P gina N§.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Fecha no v lida",
      "Rango: ",
      " - ",
      "S/N",
      "EXPRESION NO VALIDA",
   
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
      "acceso al array",
      "asignaci¢n del array",
      "dimensi¢n del array",
      "no es un array",
      "conditional",
   
      /* Internal error names */
   
      "Error irrecuperable %lu: ",
      "Fallo en recuperaci¢n de error",
      "No hay ERRORBLOCK() para el error",
      "Demasiadas llamadas recursivas al controlador de errores",
      "RDD no v lido ¢ fallo al cargar",
      "Tipo de m‚todo no v lido desde %s",
      "hb_xgrab no puede asignar memoria",
      "hb_xrealloc llamado con un puntero nulo",
      "hb_xrealloc llamado con un puntero no v lido",
      "hb_xrealloc no puede reubicar la memoria",
      "hb_xfree llamado con un puntero no v lido",
      "hb_xfree llamado con un puntero nulo",
      "No se puede localizar el procedimiento de inicio: \'%s\'",
      "No hay procedimiento de inicio",
      "Opcode no soportado por la VM",
      "S¡mbolo item esperado desde %s",
      "Tipo de s¡mbolo para self no v lido desde %s",
      "Bloque de c¢digo esperado desde %s",
      "Tipo item incorrecto en la Pila al tratar de sacarlo desde %s",
      "Desbordamiento negativo en la Pila",
      "Un item estaba siendo copiado sobre s¡ mismo desde %s",
      "S¡mbolo item no v lido pasado como memvar %s",

      /* Texts */

      "YYYY/MM/DD",
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

