/* encoding: cp850 */
#ifndef DEFINE_CUA_CH
#define DEFINE_CUA_CH

* Definiçoes de livre uso em todos os fontes da CUA

* janelas da CUA 1.0
#define _JAN_TEXTO_10      2        // janela de texto em memoria
#define _JAN_ENTRADA_10    3        // janela de entrada de dados
#define _JAN_ARQTEXTO_10   4        // janela de arquivo texto

* janelas da CUA 2.0
#define _JAN_MENU_VERT     5        // janela de menu vertical de opcoes
#define _JAN_SELE_VETO_20  6        // janela de selecao em vetor
#define _JAN_SELE_ARQ_20   7        // janela de selecao em arquivo


* Constantes para uso do Browse
#DEFINE _SELE_SIMPLES   1
#DEFINE _SELE_MULTIPLA  2
#DEFINE _SELE_EXTENDIDA 3

* Constantes do vetor de opções vertical (V_Opcoes) do browse
#DEFINE _OPCAO_TEXTO                  1
#DEFINE _OPCAO_TEXTO_TRATADO          2
#DEFINE _OPCAO_COL_DESTAQUE           3 
#DEFINE _OPCAO_TEXTO_DESTAQUE         4
#DEFINE _OPCAO_BLOCO_ACAO             5
#DEFINE _OPCAO_CDOPCAO                6
#DEFINE _OPCAO_INKEY_DESTAQUE         7
#DEFINE _OPCAO_INKEY_DESTAQUE_CASE    8
#DEFINE _OPCAO_ALIAS_MUDA             9 
#DEFINE _OPCAO_RECNO_MUDA            10 
#DEFINE _OPCAO_FILTER_MUDA           11
#DEFINE _OPCAO_ORDER_MUDA            12
#DEFINE _OPCAO_EOFOK                 13
#DEFINE _OPCAO_HANDLE_MUDA           14
#DEFINE _OPCAO_MUDADADOS             15
*
* Constantes do vetor de botoes horizontal (V_RegiaoBotoes) das janelas
#DEFINE _BOTAO_LIN_INICIAL     1
#DEFINE _BOTAO_COL_INICIAL     2
#DEFINE _BOTAO_LIN_FINAL       3
#DEFINE _BOTAO_COL_FINAL       4
#DEFINE _BOTAO_TEXTO_COMANDO   5
#DEFINE _BOTAO_TEXTO_TRATADO_1 6
#DEFINE _BOTAO_TEXTO_TRATADO_2 7
#DEFINE _BOTAO_COL_DESTAQUE    8
#DEFINE _BOTAO_TEXTO_DESTAQUE  9
#DEFINE _BOTAO_BLOCO_ACAO     10  // só usado na CUA 2.0
#DEFINE _BOTAO_AUTOCLOSE      11  // só usado na CUA 2.0
#DEFINE _BOTAO_CDBOTAO        12  // só usado na CUA 2.0
#DEFINE _BOTAO_ALIAS_MUDA     13  // só usado na CUA 2.0
#DEFINE _BOTAO_RECNO_MUDA     14  // só usado na CUA 2.0
#DEFINE _BOTAO_FILTER_MUDA    15  // só usado na CUA 2.0
#DEFINE _BOTAO_ORDER_MUDA     16  // só usado na CUA 2.0
#DEFINE _BOTAO_EOFOK          17  // só usado na CUA 2.0
#DEFINE _BOTAO_HANDLE_MUDA    18  // só usado na CUA 2.0
#DEFINE _BOTAO_INKEY_DESTAQUE 19  // só usado na CUA 1.0
#DEFINE _BOTAO_INKEY_DESTAQUE_CASE 20  // só usado na CUA 1.0
#DEFINE _BOTAO_HANDLE_PUSHBUTTON   21  // só usado na versao grafica
#DEFINE _BOTAO_MUDADADOS      22  // só usado na versao grafica

*

* Constantes do vetor de ações de teclado (V_LstAcoes) das janelas
#DEFINE _ACAO_KEYBOARD        1
#DEFINE _ACAO_KEYBOARD_CASE   2
#DEFINE _ACAO_BLOCO_ACAO      3
#DEFINE _ACAO_AUTOCLOSE       4
#DEFINE _ACAO_CDBOTAO         5
#DEFINE _ACAO_ALIAS_MUDA      6
#DEFINE _ACAO_RECNO_MUDA      7
#DEFINE _ACAO_FILTER_MUDA     8
#DEFINE _ACAO_ORDER_MUDA      9
#DEFINE _ACAO_EOFOK          10
#DEFINE _ACAO_HANDLE_MUDA    11
#DEFINE _ACAO_MUDADADOS      12
*

* Constantes do vetor de imagens (V_LstImagens) das janelas
#DEFINE _IMAGEM_ARQUIVO         1
#DEFINE _IMAGEM_LIN_INICIAL     2
#DEFINE _IMAGEM_COL_INICIAL     3
#DEFINE _IMAGEM_LIN_FINAL       4
#DEFINE _IMAGEM_COL_FINAL       5
#DEFINE _IMAGEM_BLOCO_ACAO      6
#DEFINE _IMAGEM_AUTOCLOSE       7
#DEFINE _IMAGEM_CDBOTAO         8  // só usado na CUA 2.0
#DEFINE _IMAGEM_ALIAS_MUDA      9  // só usado na CUA 2.0
#DEFINE _IMAGEM_RECNO_MUDA     10  // só usado na CUA 2.0
#DEFINE _IMAGEM_FILTER_MUDA    11  // só usado na CUA 2.0
#DEFINE _IMAGEM_ORDER_MUDA     12  // só usado na CUA 2.0
#DEFINE _IMAGEM_EOFOK          13  // só usado na CUA 2.0
#DEFINE _IMAGEM_HANDLE_MUDA    14  // só usado na CUA 2.0
#DEFINE _IMAGEM_KEYBOARD       15
#DEFINE _IMAGEM_KEYBOARD_CASE  16
#DEFINE _IMAGEM_MUDADADOS      17  // só usado na CUA 2.0
*

* Taxa de repaint quanto se estiver fora do sistema de janelamento.
* (ex: imprindo relatório, indexando arquivos, ainda criando uma janela, etc).
*
#DEFINE _REPAINT_DEFAULT           1000 // milissegundos ( 1 segundo)
*
* Taxa de repaint quanto o sistema de janelamento est  esperando o
* usu rio teclar algo. Nesta situa‡Æo o redesenho deve ser r pido.
*
#DEFINE _REPAINT_WAIT_STATE         100 // milissegundos (um d‚cimo de segundo)
*

*
* Defines usados pela MessageBox/MessageBoxTimeOut
* https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505(v=vs.85).aspx
*
#define MB_ABORTRETRYIGNORE	0x00000002
#define MB_CANCELTRYCONTINUE	0x00000006
#define MB_HELP			0x00004000
#define MB_OK			0x00000000
#define MB_OKCANCEL		0x00000001
#define MB_RETRYCANCEL		0x00000005
#define MB_YESNO		0x00000004
#define MB_YESNOCANCEL		0x00000003
*
#define MB_ICONEXCLAMATION	0x00000030
#define MB_ICONWARNING		0x00000030
#define MB_ICONINFORMATION	0x00000040
#define MB_ICONASTERISK		0x00000040
#define MB_ICONQUESTION		0x00000020
#define MB_ICONSTOP		0x00000010
#define MB_ICONERROR		0x00000010
#define MB_ICONHAND		0x00000010
*
#define MB_DEFBUTTON1		0x00000000
#define MB_DEFBUTTON2		0x00000100
#define MB_DEFBUTTON3		0x00000200
#define MB_DEFBUTTON4		0x00000300
*
#define MB_APPLMODAL		0x00000000
#define MB_SYSTEMMODAL		0x00001000
#define MB_TASKMODAL		0x00002000
*
#define MB_DEFAULT_DESKTOP_ONLY	0x00020000
#define MB_RIGHT		0x00080000
#define MB_RTLREADING		0x00100000
#define MB_SETFOREGROUND	0x00010000
#define MB_TOPMOST		0x00040000
#define MB_SERVICE_NOTIFICATION	0x00200000
*
* Define usados em outros locais
*
#define IDABORT			 3
#define IDCANCEL		 2
#define IDCONTINUE		11
#define IDIGNORE		 5
#define IDNO			 7
#define IDOK			 1
#define IDRETRY			 4
#define IDTRYAGAIN		10
#define IDYES			 6
*
* Defines utilizados pela construção de janelas
* https://msdn.microsoft.com/en-us/library/windows/desktop/ms724371(v=vs.85).aspx
*
#define COLOR_BTNFACE		15

*
#xtranslate GetProgressBar(<janela>) => <janela>\[55]
#xtranslate SetProgressBar(<janela>,<lvalue>) => <janela>\[55] := <lvalue>

* Existem teclas diferentes que retornam o mesmo código (ex:K_CTRL_C e K_PGDN).
* Para resolver esta questão, serão criados defines específicos,
* para a CUA poder fazer esta diferenciação corretamente.
#DEFINE K_CTRL_C_ARBITRADO_TECLADO    5001
#DEFINE K_CTRL_C_ARBITRADO_TOOLBAR    5002
#DEFINE K_CTRL_V_ARBITRADO_TECLADO    5003
#DEFINE K_CTRL_V_ARBITRADO_TOOLBAR    5004
#DEFINE K_CTRL_X_ARBITRADO_TECLADO    5005
#DEFINE K_CTRL_X_ARBITRADO_TOOLBAR    5006
#DEFINE K_CTRL_Z_ARBITRADO_TECLADO    5007
#DEFINE K_CTRL_Z_ARBITRADO_TOOLBAR    5008
* Os defines ARBITRADOS abaixo poderão ser removidos, após extinção do xHarbour.
#DEFINE K_CTRL_N_ARBITRADO_TECLADO    5021
#DEFINE K_CTRL_Y_ARBITRADO_TECLADO    5023
#DEFINE K_CTRL_W_ARBITRADO_TECLADO    5025

* Não dá certo fazer um HB_KeyPut() com tecla que seja a combinação de duas teclas
* (ex: K_CTRL_C), e simultaneamente tenha outra tecla com mesmo código.
* Isto impede que a AjustaTecla() use a CtrlPressed() para diferenciar uma tecla da outra,
* quando colocada no buffer via Hb_KeyPut().
* A saída para este problema é escolher outros códigos, para os casos em que se
* vá fazer um HB_KeyPut(), voltando para o código normal dentro da AjustaTecla().
* Isto será usado, por exemplo, na ToolBar.
#DEFINE K_CTRL_C_TROCADO_POR_K_CTRL_F9    K_CTRL_F9
#DEFINE K_CTRL_V_TROCADO_POR_K_CTRL_F10   K_CTRL_F10
#DEFINE K_CTRL_X_TROCADO_POR_K_CTRL_F11   K_CTRL_F11
#DEFINE K_CTRL_Z_TROCADO_POR_K_CTRL_F12   K_CTRL_F12


* Comandos repetidos (já existem no "z:\f\xx\xx.ch") para que 
* tenham efeito também quando a CUA está sendo debugada via "z:\f\cua\exemplo\".
#xcommand ENDIF ERRO             => else ; ? MEMVAR->ERRO_IF ; endif
#xcommand ENDCASE ERRO           => otherwise ; ? MEMVAR->ERRO_CASE ; endcase


#endif