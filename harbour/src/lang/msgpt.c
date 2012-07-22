/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PT)
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Felipe Coury <fcoury@creation.com.br>
 *    Small fixes
 *    Internal error names
 *
 * See COPYING for licensing terms.
 *
 */

/* Language name: Portuguese */
/* ISO language code (2 chars): PT */
/* Codepage: CP-850 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "PT",                        /* ID */
      "Portuguese",                /* Name (in English) */
      "Portugues",                 /* Name (in native language) */
      "PT",                        /* RFC ID */
      "CP-850",                    /* Codepage */
      "",                          /* Version */

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

      "Banco de Dados    # Registro    Ult. Atuali.    Tam.",
      "Voce quer mais testes?",
      "Pagina No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Data inv†lida",
      "Faixa: ",
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
      "",
      "n∆o Ç um array",
      "condicional",

      /* Internal error names */

      "Erro irrecuper†vel %d: ",
      "Erro na recuperaáao do erro",
      "ERRORBLOCK() para erro ausente",
      "Muitas chamadas recursivas ao manipulador de erros",
      "Falha ao carregar ou RDD inv†lido",
      "MÇtodo de %s inv†lido",
      "hb_xgrab nao pode alocar mem¢ria",
      "hb_xrealloc chamado com ponteiro NULL",
      "hb_xrealloc chamado com ponteiro inv†lido",
      "hb_xrealloc nao pode realocar mem¢ria",
      "hb_xfree chamado com ponteiro inv†lido",
      "hb_xfree chamado com ponteiro NULL",
      "Impossivel localizar procedure de in°cializaáao: \'%s\'",
      "Nao ha procedure de inicializaáao",
      "VM opcode nao suportado",
      "Item de s°mbolo esperado de %s",
      "Tipo de s°mbolo inv†lido para self em %s",
      "Codeblock esperado em %s",
      "Tipo incorreto de item na pilha tentando executar um pop de %s",
      "Stack underflow",
      "Um item iria ser copiado para ele mesmo em %s",
      "Symbol item inv†lido passado como memvar  %s",
      "Memory buffer overflow",
      "hb_xgrab requisitou para alocar zero bytes",
      "hb_xrealloc requisitou para redimensiorar para zero byte",
      "hb_xalloc requisitou para alocar zero bytes",

      /* Texts */

      "DD/MM/YYYY",
      "S",
      "N"
   }
};

#define HB_LANG_ID      PT
#include "hbmsgreg.h"
