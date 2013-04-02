/*
 * Harbour Project source code:
 * Language Support Module (es_419)
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 * Copyright 2000 Antonio Linares <alinares@fivetechsoft.com>
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

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "es_419",                    /* ISO ID (2 chars) */
      "Spanish (Latin American)",  /* Name (in English) */
      "Español",                   /* Name (in native language) */
      "ES",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

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
      "Miércoles",
      "Jueves",
      "Viernes",
      "Sábado",

      /* CA-Cl*pper compatible natmsg items */

      "Bases de Datos    # Registros  Ultima act.     Tamaño",
      "Desea Vd. más ejemplos?",
      "Página Nº.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Fecha no válida",
      "Rango: ",
      " - ",
      "S/N",
      "EXPRESION NO VÁLIDA",

      /* Error description names */

      "Error desconocido",
      "Error de argumento",
      "Error de rango",
      "Desbordamiento en cadena de caracteres",
      "Desbordamiento numérico",
      "División por cero",
      "Error numérico",
      "Error de sintaxis",
      "Operación demasiado compleja",
      "",
      "",
      "Poca memoria",
      "Función no definida",
      "No existe el método",
      "No existe la variable",
      "No existe el alias",
      "No existe la variable de instancia",
      "Alias con caracteres no válidos",
      "Alias actualmente en uso",
      "",
      "Error de creación",
      "Error de apertura",
      "Error de cierre",
      "Error de lectura",
      "Error de escritura",
      "Error de impresión",
      "",
      "",
      "",
      "",
      "Operación no soportada",
      "Límite excedido",
      "Se detectó corrupción",
      "Error de tipo de datos",
      "Error de anchura de datos",
      "Area de trabajo no usada",
      "Area de trabajo no indexada",
      "Se requiere uso exclusivo",
      "Se requiere bloqueo",
      "Escritura no autorizada",
      "Fallo en el bloqueo de adición",
      "Fallo en bloqueo",
      "",
      "",
      "",
      "",
      "acceso al array",
      "asignación del array",
      "dimensión del array",
      "no es un array",
      "conditional",

      /* Internal error names */

      "Error irrecuperable %d: ",
      "Fallo en recuperación de error",
      "No hay ERRORBLOCK() para el error",
      "Demasiadas llamadas recursivas al controlador de errores",
      "RDD no válido ó fallo al cargar",
      "Tipo de método no válido desde %s",
      "hb_xgrab no puede asignar memoria",
      "hb_xrealloc llamado con un puntero nulo",
      "hb_xrealloc llamado con un puntero no válido",
      "hb_xrealloc no puede reubicar la memoria",
      "hb_xfree llamado con un puntero no válido",
      "hb_xfree llamado con un puntero nulo",
      "No se puede localizar el procedimiento de inicio: \'%s\'",
      "No hay procedimiento de inicio",
      "Opcode no soportado por la VM",
      "Símbolo item esperado desde %s",
      "Tipo de símbolo para self no válido desde %s",
      "Bloque de código esperado desde %s",
      "Tipo item incorrecto en la Pila al tratar de sacarlo desde %s",
      "Desbordamiento negativo en la Pila",
      "Un item estaba siendo copiado sobre sí mismo desde %s",
      "Símbolo item no válido pasado como memvar %s",
      "Desbordamiento de buffer de memoria",
      "hb_xgrab requirió apartar cero bytes",
      "hb_xrealloc requirió redimensionar a cero bytes",
      "hb_xalloc requirió apartar cero bytes",

      /* Texts */

      "DD/MM/YYYY",
      "S",
      "N"
   }
};

#define HB_LANG_ID      ES
#include "hbmsgreg.h"
