/* encoding: cp850 */
#ifndef XX_CH
#define XX_CH

* Conserto de bug's na linguagem
* (EXISTE bug na FUNCAO EOF(), usada dentro de objetos TBROWSE, em modo
*  mono-usu†rio, existindo tambÇm v†rias descompassos na atualizaáÑo
*  das funáîes EOF(), LASTREC(), etc. Para solucionar parte destes problemas
*  adotou a posiáÑo mais conservadora poss°vel para testar o EOF().)
*
#translate XEOF() => (EOF() .OR. RECNO()==LASTREC()+1)

#xcommand ASSUME <xpr> [ CODIGO <c_CdMens> [DESCRICAO <c_Txt_Descricao>] ]=> ;
          IF .NOT. (<xpr>) ; ERRCTRL(<c_CdMens>,<c_Txt_Descricao>) ; ? MEMVAR->ERRO_ASSUME ; endif

****************
* Novos comandos
*
#xcommand ENDIF ERRO             => else ; ? MEMVAR->ERRO_IF ; endif
#xcommand ENDCASE ERRO           => otherwise ; ? MEMVAR->ERRO_CASE ; endcase



************
* ACENTUACAO
*
* Tabelas para suporte Ö acentuaáÑo, usa p†gina de c¢digo 850 do DOS
* (cedilha pode ser gerado por '+C mas na impressÑo deve ser ,+C).
*                              ---                           ---
#DEFINE _AC_PAR_SIM " † Ç ° ¢ £ µ ê ÷ ‡ È Ö ∑ ∆ ‰ « Â É à ì ∂ “ ‚ á Ä ß ¶ ß ¶"+;
                    ' ö Å'+" Ä á"
#DEFINE _AC_PAR_NAO "'a'e'i'o'u'A'E'I'O'U`a`A~a~o~A~O^a^e^o^A^E^O,c,C_o_a_O_A"+;
                    '"U"u'+"'C'c"
*
#DEFINE _AC_UM_SIM  "†Ç°¢£µê÷‡ÈÖ∑∆‰«ÂÉàì∂“‚öÅáÄß¶"    // Caracteres DOS
#DEFINE _AC_UM_NAO  "aeiouAEIOUaAaoAOaeoAEOUucCoa"    // sem acentos
#DEFINE _AC_UM_WIN  "·ÈÌÛ˙¡…Õ”⁄‡¿„ı√’‚ÍÙ¬ ‘‹¸Á«∫™"    // Caracteres WIN
#DEFINE _AC_UM_UNI  "e1e9edf3fac1c9cdd3dae0c0e3f5c3d5e2eaf4c2cad4fcdce7c7baaa"    // Caracteres UNICODE
*
#DEFINE _AC_MAIUS   "µê÷‡È∑«Â∂“‚öÄ"
#DEFINE _AC_MINUS   "†Ç°¢£Ö∆‰ÉàìÅá"

* Define caracteres que existem na tabela do Windows, mas nao existem na
* tabela do DOS (850). Deve ser usado quando converter um texto vindo
* do Windows para o 850. Eê uma conversao nao reversivel, ou seja,
* deve ser executada somente o Windows para a tabela do 850.

#DEFINE _ABRE_ASPAS_DUPLO  CHR(147)
#DEFINE _FECHA_ASPAS_DUPLO CHR(148)
#DEFINE _TRACO_LARGO       CHR(150)
#DEFINE _CODIGOS_WINDOWS_ORIGEM _ABRE_ASPAS_DUPLO+_FECHA_ASPAS_DUPLO+_TRACO_LARGO
#DEFINE _CODIGOS_DOS_DESTINO    '"' + '"' + '-'    // Aspas duplas comuns e traco curto comum
*
* Define caracteres gerados pala p†gina de c¢digo 850 mas que n∆o tem
* significado para o Brasil
#DEFINE _AC_NAO_EXISTE "äçïó‘ﬁ„Îå◊ñÍ"
*
*************************************
* Definicoes globais para os sistemas
*
#DEFINE   GL_ENTER    'Enter'
#DEFINE   GL_PC       "@E 999,999,999,999.99"        // picture para valores
#DEFINE   GL_TAM_PC   18     // tamanho real de um numero formatado por GL_PC
// #DEFINE   GL_TAM_DT   10     // tamanho m†ximo de uma data formatada DD/MM/AAAA
#DEFINE   GL_USUARIO  "ASPEC ltda"
#DEFINE   GL_DTVAZIA  CTOD("")           // data vazia usada em inicializaáîes
#DEFINE   _QL         CHR(013)+CHR(010)   // 2 caracteres de quebra de linha do Windows
#DEFINE   _QLSOFT     CHR(141)+CHR(010)   // 2 caracteres de quebra de linha "soft" do Harbour
#DEFINE   _LF         CHR(010)            // 1 caractere de salto de linha (quebra de linha do Linux)
#DEFINE   _CR         CHR(013)            // 1 caractere de retrocesso de carro (qual uso?)
#DEFINE   _EJECT      CHR(012)            // 1 caractere de salto de p†gina
#DEFINE   _CHAR_TABULACAO  CHR(009)       

* Definiáîes para cores

#DEFINE _COR_MENSAGEM_ERRO              1
#DEFINE _COR_MENSAGEM_ADVERTENCIA       2
#DEFINE _COR_MENSAGEM_OK                3
#DEFINE _COR_PADRAO                     4




#endif