/*
 * $Id$
 */

#include <hbdefs.h>

char *hb_monthsname[ 12 ] = {
   "Janeiro", "Fevereiro", "Março",
   "Abril", "Maio", "Junho", "Julho",
   "Agosto", "Setembro", "Outubro",
   "Novembro", "Dezembro" };

char *hb_daysname[ 7 ] = {
   "Domingo", "Segunda-feira", "Terça-feira",
   "Quarta-feira", "Quinta-feira", "Sexta-feira",
   "Sábado" };

static char *genericErrors[] =
{
   "Erro desconhecido",
   "Erro nos parâmetros",
   "Erro de limite",
   "Overflow de string",
   "Overflow numérico",
   "Divizao por zero",
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
   "Numero de parâmetros incorretos",
   "acesso de array",
   "array assign",
   "não é um array",
   "condicional"
};

char *hb_ErrorNatDescription( ULONG ulGenError )
{
   if( ulGenError <= sizeof(genericErrors)/sizeof(char*) )
      return genericErrors[ ulGenError ];
   else
      return genericErrors[ 0 ];
}
