/* Last Translator: SAMI <sami@laham.com.br> */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "pt_br",
      "Portuguese",
      "Portugues",
      "",
      "UTF8",
      "",

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
      "Tamanho do dado muito longo",
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
      "Falha Object destructor",
      "acesso de array",
      "matriz atribuir",
      "dimensão da matriz",
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
      "Impossivel localizar procedure de inícializaçao: '%s'",
      "Nao ha procedure de inicializaçao",
      "VM opcode nao suportado",
      "Item de símbolo esperado de %s",
      "Tipo de símbolo inválido para self em %s",
      "Codeblock esperado em %s",
      "Tipo incorreto de item na pilha tentando executar um pop de %s",
      "Stack underflow \"estouro negativo de pilha\"",
      "Um item iria ser copiado para ele mesmo em %s",
      "Symbol item inválido passado como memvar %s",
      "Buffer overflow de memória",
      "hb_xgrab requisitou para alocar zero bytes",
      "hb_xrealloc requisitou para redimensiorar para zero byte",
      "hb_xalloc requisitou para alocar zero bytes",

      /* Texts */

      "DD/MM/YYYY",
      "S",
      "N"
   }
};

#define HB_LANG_ID      PT_BR
#include "hbmsgreg.h"
