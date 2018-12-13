/* Last Translator: hbtest */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "es",
      "Spanish",
      "Español",
      "",
      "UTF8",
      "",

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
      "EXPRESIÓN NO VÁLIDA",

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
      "Fallo en el bloqueo al añadir",
      "Fallo en bloqueo",
      "",
      "",
      "",
      "Error en destructor del objecto",
      "acceso al array",
      "asignación del array",
      "dimensión del array",
      "no es un array",
      "condicional",

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
      "No se puede localizar el procedimiento de inicio: '%s'",
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
