/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PT)
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

/* Language name: Portuguese */
/* ISO language code (2 chars): PT */
/* Codepage: 850 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */
   
      "PT",                        /* ID */
      "Portuguese",                /* Name (in English) */
      "Portugues",                 /* Name (in native language) */
      "PT",                        /* RFC ID */
      "850",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */
   
      /* Month names */

      "Janeiro",
      "Fevereiro",
      "Maráo",
      "Abril",
      "Maio",
      "Junho",
      "Julho",
      "Agosto",
      "Setembro",
      "Outubro",
      "Novembro",
      "Dezembro",
   
      /* Day names */

      "Domingo",
      "Segunda-feira",
      "Teráa-feira",
      "Quarta-feira",
      "Quinta-feira",
      "Sexta-feira",
      "S†bado",
   
      /* CA-Cl*pper compatible natmsg items */
   
      "Database Files    # Records    Last Update     Size",
      "Voce quer mais testes?",
      "Pagina No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Data invalida",
      "Range: ",
      " - ",
      "S/N",
      "EXPRESS«O INVALIDA",
   
      /* Error description names */
   
      "Erro desconhecido",
      "Erro nos parÉmetros",
      "Erro de limite",
      "Overflow de string",
      "Overflow numÈrico",
      "Divis∆o por zero",
      "Erro numÇrico",
      "Erro de sintaxe",
      "Operaá∆o muito complexa",
      "",
      "",
      "Mem¢ria insuficiente",
      "Funá∆o indefinida",
      "MÇtodo n∆o exportado",
      "Vari†vel n∆o existe",
      "Alias n∆o existe",
      "Nenhuma vari†vel exportada",
      "Nome de alias incorreto",
      "Nome de alias duplicado",
      "",
      "Erro de criaá∆o",
      "Erro de abertura",
      "Erro ao fechar",
      "Erro de leitura",
      "Erro de escrita",
      "Erro de impress∆o",
      "",
      "",
      "",
      "",
      "Operaá∆o n∆o suportada",
      "Limite excedido",
      "Detectado °ndice corrompido",
      "Tipo incorreto de dado",
      "Tamanho do dato muito longo",
      "Workarea n∆o est† em uso",
      "Workarea n∆o indexada",
      "Uso exclusivo requerido",
      "Travamento requerido",
      "Escrita n∆o permitida",
      "Falha no travamento do Append",
      "Falha no travamento",
      "",
      "",
      "",
      "",
      "acesso de array",
      "array assign",
      ""
      "n∆o Ç um array",
      "condicional",

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

HB_LANG_ANNOUNCE( PT );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_PT )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_PT )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_PT
#endif

