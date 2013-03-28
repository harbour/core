/*
 * Harbour Project source code:
 * Language Support Module (pt)
 *
 * Copyright 2000 Felipe Coury <fcoury@creation.com.br> (small fixes, internal error names)
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

      "pt",                        /* ISO ID (2 chars) */
      "Portuguese",                /* Name (in English) */
      "Portugues",                 /* Name (in native language) */
      "PT",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Janeiro",
      "Fevereiro",
      "Março",
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
      "Terça-feira",
      "Quarta-feira",
      "Quinta-feira",
      "Sexta-feira",
      "Sábado",

      /* CA-Cl*pper compatible natmsg items */

      "Banco de Dados    # Registro   Ult. Atuali.    Tam.",
      "Voce quer mais testes?",
      "Pagina No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Data inválida",
      "Faixa: ",
      " - ",
      "S/N",
      "EXPRESSÃO INVALIDA",

      /* Error description names */

      "Erro desconhecido",
      "Erro nos parâmetros",
      "Erro de limite",
      "Overflow de string",
      "Overflow numérico",
      "Divisão por zero",
      "Erro numérico",
      "Erro de sintaxe",
      "Operação muito complexa",
      "",
      "",
      "Memória insuficiente",
      "Função indefinida",
      "Método não exportado",
      "Variável não existe",
      "Alias não existe",
      "Nenhuma variável exportada",
      "Nome de alias incorreto",
      "Nome de alias duplicado",
      "",
      "Erro de criação",
      "Erro de abertura",
      "Erro ao fechar",
      "Erro de leitura",
      "Erro de escrita",
      "Erro de impressão",
      "",
      "",
      "",
      "",
      "Operação não suportada",
      "Limite excedido",
      "Detectado índice corrompido",
      "Tipo incorreto de dado",
      "Tamanho do dato muito longo",
      "Workarea não está em uso",
      "Workarea não indexada",
      "Uso exclusivo requerido",
      "Travamento requerido",
      "Escrita não permitida",
      "Falha no travamento do Append",
      "Falha no travamento",
      "",
      "",
      "",
      "",
      "acesso de array",
      "array assign",
      "",
      "não é um array",
      "condicional",

      /* Internal error names */

      "Erro irrecuperável %d: ",
      "Erro na recuperaçao do erro",
      "ERRORBLOCK() para erro ausente",
      "Muitas chamadas recursivas ao manipulador de erros",
      "Falha ao carregar ou RDD inválido",
      "Método de %s inválido",
      "hb_xgrab nao pode alocar memória",
      "hb_xrealloc chamado com ponteiro NULL",
      "hb_xrealloc chamado com ponteiro inválido",
      "hb_xrealloc nao pode realocar memória",
      "hb_xfree chamado com ponteiro inválido",
      "hb_xfree chamado com ponteiro NULL",
      "Impossivel localizar procedure de inícializaçao: \'%s\'",
      "Nao ha procedure de inicializaçao",
      "VM opcode nao suportado",
      "Item de símbolo esperado de %s",
      "Tipo de símbolo inválido para self em %s",
      "Codeblock esperado em %s",
      "Tipo incorreto de item na pilha tentando executar um pop de %s",
      "Stack underflow",
      "Um item iria ser copiado para ele mesmo em %s",
      "Symbol item inválido passado como memvar %s",
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
