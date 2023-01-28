/* encoding: cp850 */
/*
                               Aspec - Informatica
                           Direitos Autorais Reservado

Comandos de definição de dados

*/
#ifndef DEF_DADOS_CH

#define DEF_DADOS_CH

* Indicativo de severidade da mensagens
* NOTA: Por enquanto, será dado suporte somente a uma severidade
//#DEFINE _MENS_DICA_LEVE                       1
//#DEFINE _MENS_DICA_FORTE                   2
//#DEFINE _MENS_ADVERTENCIA_LEVE     3
//#DEFINE _MENS_ADVERTENCIA_FORTE  4 
#DEFINE _MENS_ERRO_GRAVE                   5
//#DEFINE _MENS_ERRO_GRAVISSIMO       6


*******
* CAMPO
*******
* Definição: 
*    Contém uma só informação. Deve ser definido como se nunca fosse ficar vazio.
*
*******************
* comando DEF_CAMPO
*******************
* Nota: Os parâmetros SINGULAR, PLURAL
*       devem (opcionalmente) ser utilizados somente quando o
*       campo for um substantivo ou um adjetivo, e devem indicar
*       a grafia usada no parâmetro NOME.

*         Exemplo: Se NOME for "Pessoa", usar SINGULAR
*         Exemplo: Se NOME for "Pessoas", usar PLURAL
*
* Nota: Os parâmetros MASCULINO, FEMININO
*       devem (opcionalmente) ser utilizados somente quando o
*       campo for um substantivo ou um adjetivo, e devem indicar
*       o gênero do parâmetro NOME.
*         Exemplo: Se NOME for "Adulto", usar MASCULINO
*         Exemplo: Se NOME for "Pessoa", usar FEMININO
*
* DEF_CAMPO <O_Campo> TAMANHO <N_TamanhoFisico> [PICTURE <C_Picture>] ; 
*    NOME <C_NomeCompleto> [RESUMIDO <C_NomeResumido>] ;
*    [ [SINGULAR] | [PLURAL] ] ;
*    [ [MASCULINO] | [FEMININO] ]
*
#xcommand DEF_CAMPO <O_Campo> ;
          TAMANHO <N_TamanhoFisico> ;
          [PICTURE <C_Picture>] ;
          NOME <C_NomeCompleto> ;
          [RESUMIDO <C_NomeResumido>] ;
          [<L_FlexaoNumeroSingular: SINGULAR>] [<L_FlexaoNumeroPlural: PLURAL>] ;
          [<L_FlexaoGeneroMasculino: MASCULINO>] [<L_FlexaoGeneroFeminino: FEMININO>] ;
          => <O_Campo> := DEF_CAMPO(<N_TamanhoFisico>,<C_Picture>,;
                                    <C_NomeCompleto>,<C_NomeResumido>,;
                                    <.L_FlexaoNumeroSingular.>,<.L_FlexaoNumeroPlural.>,;
                                    <.L_FlexaoGeneroMasculino.>,<.L_FlexaoGeneroFeminino.>,;
                                   -1)  // Vide texto da rotina, para explicação sobre sobre "-1"

************************************
* comando ADD_AO_CAMPO ... DESCRICAO
************************************
* ADD_AO_CAMPO <O_Campo> DESCRICAO CLIENTE ;
*   <C_DescricaoCliente>
* ADD_AO_CAMPO <O_Campo> DESCRICAO ATENDENTE ;
*    <C_DescricaoAtendente>
* ADD_AO_CAMPO <O_Campo> DESCRICAO ANALISTA ;
*    <C_DescricaoAnalista>
* ADD_AO_CAMPO <O_Campo> DESCRICAO DESENVOLVEDOR ;
*    <C_DescricaoDesenvolvedor>
*    
#xcommand ADD_AO_CAMPO <O_Campo> ;
              DESCRICAO CLIENTE <C_DescricaoCliente> ;
          => ADD_AO_CAMPO_DESCRICAO_CLIENTE(@<O_Campo>,;
                 <C_DescricaoCliente>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_CAMPO <O_Campo> ;
              DESCRICAO ATENDENTE <C_DescricaoAtendente> ;
          => ADD_AO_CAMPO_DESCRICAO_ATENDENTE(@<O_Campo>,;
                 <C_DescricaoAtendente>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_CAMPO <O_Campo> ;
              DESCRICAO ANALISTA <C_DescricaoAnalista> ;
          => ADD_AO_CAMPO_DESCRICAO_ANALISTA(@<O_Campo>,;
                 <C_DescricaoAnalista>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_CAMPO <O_Campo> ;
              DESCRICAO DESENVOLVEDOR <C_DescricaoDesenvolvedor> ;
          => ADD_AO_CAMPO_DESCRICAO_DESENVOLVEDOR(@<O_Campo>,;
                 <C_DescricaoDesenvolvedor>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

*
*********************************
* comando ESP_CAMPO ... TIPO_DATA
*********************************
* Campo data vazio ou não vazio, com restrição de final de semana
*   ESP_CAMPO <O_Campo> TIPO_DATA 
*      MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
*      MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
*      [NAO_SABADO] [NAO_DOMINGO] ;
*      [SUGERIR <D_ConteudoSugerido>] 
* Campo data vazio ou não vazio, sem restrição de final de semana
*   ESP_CAMPO <O_Campo> TIPO_DATA ;
*      MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
*      MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
*      [ [INICIO_ANO] | [FINAL_ANO] | [INICIO_SEMESTRE] | [FINAL_SEMESTRE] ;
*        [INICIO_QUADRIMESTRE] | [FINAL_QUADRIMESTRE] | [INICIO_TRIMESTRE] | [FINAL_TRIMESTRE] ;
*        [INICIO_BIMESTRE] | [FINAL_BIMESTRE] | [INICIO_MES] | [FINAL_MES] ] ;
*      [SUGERIR <D_ConteudoSugerido>] 
* Campo data sempre vazia
*   ESP_CAMPO <O_Campo> TIPO_DATA ;
*      SEMPRE_VAZIA
*
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_NaoSabado: NAO_SABADO>] ;
          [<L_NaoDomingo: NAO_DOMINGO>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;      // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                <.L_NaoSabado.>,<.L_NaoDomingo.>,;
                .F.,.F.,;  // Ano         
                .F.,.F.,;  // Semestre
                .F.,.F.,;  // Quadrimestre
                .F.,.F.,;  // Trimestre
                .F.,.F.,;  // Bimestre
                .F.,.F.,;  // Mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeAno: INICIO_ANO>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                      // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                  // Não sábado, não domingo
                <.L_InicioDeAno.>,.F.,;  // Ano         
                <.L_InicioDeAno.>,.F.,;  // Semestre - Inicio de ano é sempre início de semestre
                <.L_InicioDeAno.>,.F.,;  // Quadrimestre - Inicio de ano é sempre início de quadrimestre
                <.L_InicioDeAno.>,.F.,;  // Trimestre - Inicio de ano é sempre início de trimestre
                <.L_InicioDeAno.>,.F.,;  // Bimestre - Inicio de ano é sempre início de bimestre
                <.L_InicioDeAno.>,.F.,;  // Mês - Inicio de ano é sempre início de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
                
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeAno: FINAL_ANO>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                     // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                 // Não sábado, não domingo
                .F.,<.L_FinalDeAno.>,;  // Ano         
                .F.,<.L_FinalDeAno.>,;  // Semestre - Final de ano é sempre final de semestre
                .F.,<.L_FinalDeAno.>,;  // Quadrimestre - Final de ano é sempre final de quadrimestre
                .F.,<.L_FinalDeAno.>,;  // Trimestre - Final de ano é sempre final de trimestre
                .F.,<.L_FinalDeAno.>,;  // Bimestre - Final de ano é sempre final de bimestre
                .F.,<.L_FinalDeAno.>,;  // Mês - Final de ano é sempre final de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeSemestre: INICIO_SEMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                           // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                       // Não sábado, não domingo
                .F.,.F.,;                       // Ano         
                <.L_InicioDeSemestre.>,.F.,;  // Semestre
                .F.,.F.,;                       // Quadrimestre
                <.L_InicioDeSemestre.>,.F.,;  // Trimestre - Inicio de semestre é sempre início de trimestre
                <.L_InicioDeSemestre.>,.F.,;  // Bimestre - Inicio de semestre é sempre início de bimestre
                <.L_InicioDeSemestre.>,.F.,;  // Mês - Inicio de semestre é sempre início de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
                
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeSemestre: FINAL_SEMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                          // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                      // Não sábado, não domingo
                .F.,.F.,;                      // Ano         
                .F.,<.L_FinalDeSemestre.>,;  // Semestre
                .F.,.F.,;                      // Quadrimestre
                .F.,<.L_FinalDeSemestre.>,;  // Trimestre - Final de semestre é sempre final de trimestre
                .F.,<.L_FinalDeSemestre.>,;  // Bimestre - Final de semestre é sempre final de bimestre
                .F.,<.L_FinalDeSemestre.>,;  // Mês - Final de semestre é sempre final de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeQuadrimestre: INICIO_QUADRIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                               // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                           // Não sábado, não domingo
                .F.,.F.,;                           // Ano         
                .F.,.F.,;                           // Semestre
                <.L_InicioDeQuadrimestre.>,.F.,;  // Quadrimestre
                .F.,.F.,;                           // Trimestre
                <.L_InicioDeQuadrimestre.>,.F.,;  // Bimestre - Inicio de quadrimestre é sempre início de bimestre
                <.L_InicioDeQuadrimestre.>,.F.,;  // Mês - Inicio de quadrimestre é sempre início de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
                
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeQuadrimestre: FINAL_QUADRIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                              // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                          // Não sábado, não domingo
                .F.,.F.,;                          // Ano         
                .F.,.F.,;                          // Semestre
                .F.,<.L_FinalDeQuadrimestre.>,;  // Quadrimestre
                .F.,.F.,;                          // Trimestre
                .F.,<.L_FinalDeQuadrimestre.>,;  // Bimestre - Final de quadrimestre é sempre final de bimestre
                .F.,<.L_FinalDeQuadrimestre.>,;  // Mês - Final de quadrimestre é sempre final de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeTrimestre: INICIO_TRIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                            // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                        // Não sábado, não domingo
                .F.,.F.,;                        // Ano         
                .F.,.F.,;                        // Semestre
                .F.,.F.,;                        // Quadrimestre
                <.L_InicioDeTrimestre.>,.F.,;  // Trimestre
                .F.,.F.,;                        // Bimestre
                <.L_InicioDeTrimestre.>,.F.,;  // Mês - Inicio de trimestre é sempre início de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
                
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeTrimestre: FINAL_TRIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                           // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                       // Não sábado, não domingo
                .F.,.F.,;                       // Ano         
                .F.,.F.,;                       // Semestre
                .F.,.F.,;                       // Quadrimestre
                .F.,<.L_FinalDeTrimestre.>,;  // Trimestre
                .F.,.F.,;                       // Bimestre
                .F.,<.L_FinalDeTrimestre.>,;  // Mês - Final de trimestre é sempre final de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeBimestre: INICIO_BIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                           // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                       // Não sábado, não domingo
                .F.,.F.,;                       // Ano         
                .F.,.F.,;                       // Semestre
                .F.,.F.,;                       // Quadrimestre
                .F.,.F.,;                       // Trimestre
                <.L_InicioDeBimestre.>,.F.,;  // Bimestre
                <.L_InicioDeBimestre.>,.F.,;  // Mês - Inicio de bimestre é sempre início de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
                
#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeBimestre: FINAL_BIMESTRE>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                          // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                      // Não sábado, não domingo
                .F.,.F.,;                      // Ano         
                .F.,.F.,;                      // Semestre
                .F.,.F.,;                      // Quadrimestre
                .F.,.F.,;                      // Trimestre
                .F.,<.L_FinalDeBimestre.>,;  // Bimestre
                .F.,<.L_FinalDeBimestre.>,;  // Mês - Final de bimestre é sempre final de mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_InicioDeMes: INICIO_MES>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                      // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                  // Não sábado, não domingo
                .F.,.F.,;                  // Ano         
                .F.,.F.,;                  // Semestre
                .F.,.F.,;                  // Quadrimestre
                .F.,.F.,;                  // Trimestre
                .F.,.F.,;                  // Bimestre
                <.L_InicioDeMes.>,.F.,;  // Mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          MAIOR_OU_IGUAL_A <D_MaiorOuIgualA> ;
          MENOR_OU_IGUAL_A <D_MenorOuIgualA> ;
          [<L_FinalDeMes: FINAL_MES>] ;
          [SUGERIR <D_ConteudoSugerido>] ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .F.,;                     // Sempre vazia 
                <D_MaiorOuIgualA>,<D_MenorOuIgualA>,;
                .F.,.F.,;                 // Não sábado, não domingo
                .F.,.F.,;                 // Ano         
                .F.,.F.,;                 // Semestre
                .F.,.F.,;                 // Quadrimestre
                .F.,.F.,;                 // Trimestre
                .F.,.F.,;                 // Bimestre
                .F.,<.L_FinalDeMes.>,;  // Mês
                <D_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DATA ;
          SEMPRE_VAZIA ;
          => ESP_CAMPO_DATA(@<O_Campo>,;
                .T.,;                     // Sempre vazia
                NIL,NIL,;                 // Maior ou igual a, menor ou igual a
                .F.,.F.,;                 // Não sábado, não domingo
                .F.,.F.,;                 // Ano         
                .F.,.F.,;                 // Semestre
                .F.,.F.,;                 // Quadrimestre
                .F.,.F.,;                 // Trimestre
                .F.,.F.,;                 // Bimestre
                .F.,.F.,;                 // Mês
                NIL,;                     // Conteúdo sugerido
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

***********************************
* comando ESP_CAMPO ... TIPO_LOGICO
***********************************
* ESP_CAMPO <O_Campo> TIPO_LOGICO ;
*    [SUGERIR <L_ConteudoSugerido>]
*
#xcommand ESP_CAMPO <O_Campo> TIPO_LOGICO ;
          [SUGERIR <L_ConteudoSugerido>] ;
          => ESP_CAMPO_LOGICO(@<O_Campo>,;
               <L_ConteudoSugerido>,;
               -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"


************************************
* comando ESP_CAMPO ... TIPO_INTEIRO
************************************
* ESP_CAMPO <O_Campo> TIPO_INTEIRO ;
*    [ [MAIOR_QUE <N_MaiorQue>] | [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ] ;
*    [ [MENOR_QUE <N_MenorQue>] | [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ] ;
*    [SUGERIR <N_ConteudoSugerido>]
*
#xcommand ESP_CAMPO <O_Campo> TIPO_INTEIRO ;
          [MAIOR_QUE <N_MaiorQue>] ;
          [MENOR_QUE <N_MenorQue>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.T.,0,;
               <N_MaiorQue>,NIL,;
               <N_MenorQue>,NIL,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_INTEIRO ;
          [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ;
          [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.T.,0,;
               NIL,<N_MaiorOuIgualA>,;
               NIL,<N_MenorOuIgualA>,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_INTEIRO ;
          [MAIOR_QUE <N_MaiorQue>] ;
          [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.T.,0,;
               <N_MaiorQue>,NIL,;
               NIL          ,<N_MenorOuIgualA>,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_INTEIRO ;
          [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ;
          [MENOR_QUE <N_MenorQue>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.T.,0,;
               NIL          ,<N_MaiorOuIgualA>,;
               <N_MenorQue>,NIL,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

************************************
* comando ESP_CAMPO ... TIPO_DECIMAL
************************************
* ESP_CAMPO <O_Campo> TIPO_DECIMAL <N_QuantDecimais> ;
*    [ [MAIOR_QUE <N_MaiorQue>] | [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ] ;
*    [ [MENOR_QUE <N_MenorQue>] | [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ] ;
*    [SUGERIR <N_ConteudoSugerido>]
* 
#xcommand ESP_CAMPO <O_Campo> TIPO_DECIMAL <N_QuantDecimais> ;
          [MAIOR_QUE <N_MaiorQue>] ;
          [MENOR_QUE <N_MenorQue>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.F.,<N_QuantDecimais>,;
               <N_MaiorQue>,NIL,;
               <N_MenorQue>,NIL,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DECIMAL <N_QuantDecimais> ;
          [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ;
          [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.F.,<N_QuantDecimais>,;
               NIL,<N_MaiorOuIgualA>,;
               NIL,<N_MenorOuIgualA>,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DECIMAL <N_QuantDecimais> ;
          [MAIOR_QUE <N_MaiorQue>] ;
          [MENOR_OU_IGUAL_A <N_MenorOuIgualA>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.F.,<N_QuantDecimais>,;
               <N_MaiorQue>,NIL,;
               NIL          ,<N_MenorOuIgualA>,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ESP_CAMPO <O_Campo> TIPO_DECIMAL <N_QuantDecimais> ;
          [MAIOR_OU_IGUAL_A <N_MaiorOuIgualA>] ;
          [MENOR_QUE <N_MenorQue>] ;
          [SUGERIR <N_ConteudoSugerido>] ;
          => ESP_CAMPO_NUMERICO(@<O_Campo>,.F.,<N_QuantDecimais>,;
               NIL          ,<N_MaiorOuIgualA>,;
               <N_MenorQue>,NIL,;
               <N_ConteudoSugerido>,;
                -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

**************************************
* comando ESP_CAMPO ... TIPO_CARACTERE
**************************************
* ESP_CAMPO <O_Campo> TIPO_CARACTERE ;
*    [MINIMO <N_TamanhoMinimoInformado>] [MAXIMO <N_TamanhoMaximoInformado>] ;
*    [ [MINUSCULO] [MAIUSCULO] ];
*    [EXPRESSAO_REGULAR <C_ExprRegular> [MENSAGEM <C_MensExprRegular> ] ] ; 
*    [COM_ESPACO_INICIO] [SEM_ESPACO_MEIO] COM_ESPACO_DUPLO] ;
*    [COM_TABULACAO] [COM_QUEBRA_LINHA] [COM_QUEBRA_PAGINA] [BINARIO] ;
*    [SUGERIR <C_ConteudoSugerido>]
*
#xcommand ESP_CAMPO <O_Campo> TIPO_CARACTERE ;
          [MINIMO <N_TamanhoMinimoInformado>] [MAXIMO <N_TamanhoMaximoInformado>] ;
          [<L_CaseMinusculo: MINUSCULO>] ;
          [<L_CaseMaiusculo: MAIUSCULO>] ;
          [EXPRESSAO_REGULAR <C_ExprRegular> [MENSAGEM <C_MensExprRegular> ]] ; 
          [<L_ComEspacoInicio: COM_ESPACO_INICIO>] ;
          [<L_SemEspacoMeio: SEM_ESPACO_MEIO>] ;
          [<L_ComEspacoDuploNoMeio: COM_ESPACO_DUPLO>] ;
          [<L_ComTabulacao: COM_TABULACAO>] ;
          [<L_ComQuebraLinha: COM_QUEBRA_LINHA>] ;
          [<L_ComQuebraPagina: COM_QUEBRA_PAGINA>] ;
          [<L_Binario: BINARIO>] ;
          [SUGERIR <C_ConteudoSugerido>] ;
          => ESP_CAMPO_TEXTO(@<O_Campo>,;
               .T.,;  // É campo texto de tamanho fixo (não "memo")
               <N_TamanhoMinimoInformado>,<N_TamanhoMaximoInformado>,;
               NIL,NIL,;    // PENDENTE - Falta implementar C_Conteudo_Minimo, C_Conteudo_Maximo
               <.L_CaseMinusculo.>,<.L_CaseMaiusculo.>,;
               <C_ExprRegular>,<C_MensExprRegular>,;
               <.L_ComEspacoInicio.>,;
               <.L_SemEspacoMeio.>,;
               <.L_ComEspacoDuploNoMeio.>,; 
               .T.,;  // Com espaço no fim
               <.L_ComTabulacao.>,; 
               <.L_ComQuebraLinha.>,; 
               <.L_ComQuebraPagina.>,; 
               <.L_Binario.>,;
               <C_ConteudoSugerido>,;
               -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
*

*********************************
* comando ESP_CAMPO ... TIPO_MEMO
*********************************
* ESP_CAMPO <O_Campo> TIPO_MEMO ;
*    [MINIMO <N_TamanhoMinimoInformado>] MAXIMO <N_TamanhoMaximoInformado> ;
*    [ [MINUSCULO] [MAIUSCULO] ];
*    [EXPRESSAO_REGULAR <C_ExprRegular> [MENSAGEM <C_MensExprRegular> ] ] ; 
*    [COM_ESPACO_INICIO] [SEM_ESPACO_MEIO] COM_ESPACO_DUPLO] [COM_ESPACO_FIM] ;
*    [COM_TABULACAO] [COM_QUEBRA_LINHA] [COM_QUEBRA_PAGINA] [BINARIO] ;
*    [SUGERIR <C_ConteudoSugerido>]
*
#xcommand ESP_CAMPO <O_Campo> TIPO_MEMO ;
          [MINIMO <N_TamanhoMinimoInformado>] MAXIMO <N_TamanhoMaximoInformado> ;
          [<L_CaseMinusculo: MINUSCULO>] ;
          [<L_CaseMaiusculo: MAIUSCULO>] ;
          [EXPRESSAO_REGULAR <C_ExprRegular> [MENSAGEM <C_MensExprRegular> ]] ; 
          [<L_ComEspacoInicio: COM_ESPACO_INICIO>] ;
          [<L_SemEspacoMeio: SEM_ESPACO_MEIO>] ;
          [<L_ComEspacoDuploNoMeio: COM_ESPACO_DUPLO>] ;
          [<L_ComEspacoFim: COM_ESPACO_FIM>] ;
          [<L_ComTabulacao: COM_TABULACAO>] ;
          [<L_ComQuebraLinha: COM_QUEBRA_LINHA>] ;
          [<L_ComQuebraPagina: COM_QUEBRA_PAGINA>] ;
          [<L_Binario: BINARIO>] ;
          [SUGERIR <C_ConteudoSugerido>] ;
          => ESP_CAMPO_TEXTO(@<O_Campo>,;
               .F.,;  // É campo texto de tamanho variável ("memo")
               <N_TamanhoMinimoInformado>,<N_TamanhoMaximoInformado>,;
               NIL,NIL,;  // C_Conteudo_Minimo, C_Conteudo_Maximo sempre NIL em campo MEMO
               <.L_CaseMinusculo.>,<.L_CaseMaiusculo.>,;
               <C_ExprRegular>,<C_MensExprRegular>,;
               <.L_ComEspacoInicio.>,;
               <.L_SemEspacoMeio.>,;
               <.L_ComEspacoDuploNoMeio.>,; 
               <.L_ComEspacoFim.>,;
               <.L_ComTabulacao.>,; 
               <.L_ComQuebraLinha.>,; 
               <.L_ComQuebraPagina.>,; 
               <.L_Binario.>,;
               <C_ConteudoSugerido>,;
               -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"
*

************************************
* comando ADD_AO_CAMPO ... VALIDACAO
************************************
* ADD_AO_CAMPO <O_Campo> VALIDACAO <C_NomeRotinaDeValidacao>
*    
*  NOTA: O nome da rotina deve ser informado sem parênteses, nem parâmetro.
*
#xcommand ADD_AO_CAMPO <O_Campo> ;
              VALIDACAO <C_NomeRotinaDeValidacao> ;
          => ADD_AO_CAMPO_VALIDACAO(@<O_Campo>,;
                 {|X_Conteudo| <C_NomeRotinaDeValidacao>(X_Conteudo)},<"C_NomeRotinaDeValidacao">,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

*********************
* comando DEF_DOMINIO   // Define um domínio sem associá-lo a nenhum campo ainda
*********************
* DEF_DOMINIO <O_Dominio> ; 
*  NOME <C_NomeCompleto> ;
*  [RESUMIDO <C_NomeResumido>] ;
*  [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
*     SE <X_Conteudo_1> TITULO <C_Titulo_1>  [SUFIXO <C_SufixoDoDefine_1>],;
*     SE <X_Conteudo_N> TITULO <C_Titulo_N>  [SUFIXO <C_SufixoDoDefine_N>]
*
* - O parâmetro opcional repetitivo (na parte recursiva) é "SUFIXO".
* - Em testes práticos, notou-se que o pré-processador erra a transcrição
*   (sem dar erro de compilação) quando existe parâmetro opcional dentro
*   do trecho recursivo (repetitivo) do comando.
* - Para resolver esta questão, criou-se novos comandos, sem parâmetro
*   opcional na parte recursiva, para simular todas as situações (2 combinações possíveis).
*
* - Nos comandos recursivos, pode ocorrer do programador não escrever a parte recursiva 
*   ou escrever somente uma linha na parte recursiva.
*   Neste caso, SEM QUE SAIBAMOS O MOTIVO, o pré-processador está fazendo uma chamada 
*   adicional à rotina recursiva, com todos os parâmetros recursivos passados sem contéudo.
*      Exemplo: Fonte PRG: 
*                   DEF_DOMINIO O_Dominio ;
*                       NOME "Tipo de pessoa" ;
*                       SE _PESSOA_FISICA TITULO "Pessoa física"
*               Arquivo PPO: 
*                   O_Dominio := DEF_DOMINIO("Tipo de pessoa",,,-1)
*                   ADD_AO_DOMINIO_LINHA(@O_Dominio,_PESSOA_FISICA,"Pessoa física",NIL,-1)
*                   ADD_AO_DOMINIO_LINHA(@O_Dominio,,,NIL,-1)     // ESTA LINHA NÃO ERA PARA EXISTIR
*   Tentou-se todas as combinações e posições possíveis para os caracteres "[" e "]", tanto na
*   parte anterior do comando (antes do "=>"), como na parte posterior (depois do "=>"), sem sucesso.
*   A "solução" tratar isto dentro do PRG chamado (se chamado com tudo NIL, desconsiderar a chamada).
* 

* 1.1) Com o parâmetro opcional "SUFIXO" constando como obrigatório, na parte recursiva.
#xcommand DEF_DOMINIO <O_Dominio> ;
   NOME <C_NomeCompleto> ;
   [RESUMIDO <C_NomeResumido>] ;
   [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
   [ SE <X_Conteudo_1> TITULO <C_Titulo_1> SUFIXO <C_SufixoDoDefine_1>  ;
     [, SE <X_Conteudo_N> TITULO <C_Titulo_N> SUFIXO <C_SufixoDoDefine_N> ] ] => ;
     <O_Dominio> := DEF_DOMINIO(<C_NomeCompleto>,<C_NomeResumido>,<C_ArquivoCH>,<C_PrefixoDoDefine>,-1) ;;
     [ ADD_AO_DOMINIO_LINHA(@<O_Dominio>,<X_Conteudo_1>,<C_Titulo_1>,<C_SufixoDoDefine_1>,-1) ] ;
     [ ; ADD_AO_DOMINIO_LINHA(@<O_Dominio>,<X_Conteudo_N>,<C_Titulo_N>,<C_SufixoDoDefine_N>,-1) ] ;;
     DEF_DOMINIO_FINALIZACAO(<O_Dominio>,-1)  

* 1.2) Sem parâmetro opcional, na parte recursiva.
#xcommand DEF_DOMINIO <O_Dominio> ;
   NOME <C_NomeCompleto> ;
   [RESUMIDO <C_NomeResumido>] ;
   [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
   [ SE <X_Conteudo_1> TITULO <C_Titulo_1> ;
     [, SE <X_Conteudo_N> TITULO <C_Titulo_N> ] ] => ;
     <O_Dominio> := DEF_DOMINIO(<C_NomeCompleto>,<C_NomeResumido>,<C_ArquivoCH>,<C_PrefixoDoDefine>,-1) ;;
     [ ADD_AO_DOMINIO_LINHA(@<O_Dominio>,<X_Conteudo_1>,<C_Titulo_1>,NIL,-1) ] ;
     [ ; ADD_AO_DOMINIO_LINHA(@<O_Dominio>,<X_Conteudo_N>,<C_Titulo_N>,NIL,-1) ] ;;
     DEF_DOMINIO_FINALIZACAO(<O_Dominio>,-1)  

**********************************
* comando ADD_AO_CAMPO ... DOMINIO    // Adiciona um domínio já previamente existente a um campo
**********************************
* ADD_AO_CAMPO <O_Campo> DOMINIO <O_Dominio>
*

#xcommand ADD_AO_CAMPO <O_Campo> DOMINIO <O_Dominio> => ;
     ADD_AO_CAMPO_DOMINIO(@<O_Campo>,<O_Dominio>,-1)

**********************************
* comando DEF_NO_CAMPO ... DOMINIO   // Define um domínio associando-o diretamente a um campo
**********************************
* DEF_NO_CAMPO <O_Campo> DOMINIO [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
*   SE <X_Conteudo_1> TITULO <C_Titulo_1>  [SUFIXO <C_SufixoDoDefine_1>],;
*   SE <X_Conteudo_N> TITULO <C_Titulo_N>  [SUFIXO <C_SufixoDoDefine_N>]
*
* - O parâmetro opcional repetitivo (na parte recursiva) é "SUFIXO".
* - Em testes práticos, notou-se que o pré-processador erra a transcrição
*   (sem dar erro de compilação) quando existe parâmetro opcional dentro
*   do trecho recursivo (repetitivo) do comando.
* - Para resolver esta questão, criou-se novos comandos, sem parâmetros
*   opcionais na parte recursiva, para simular todas as situações (2 combinações possíveis).
*
* - Nos comandos recursivos, pode ocorrer do programador não escrever a parte recursiva 
*   ou escrever somente uma linha na parte recursiva.
*   Neste caso, SEM QUE SAIBAMOS O MOTIVO, o pré-processador está fazendo uma chamada 
*   adicional à rotina recursiva ADD_NO_CAMPO_LINHA_DE_DOMINIO(),
*   com todos os parâmetros recursivos passados sem contéudo (vide exemplo na DEF_DOMINIO, acima).
* 
* 1.1) Com o parâmetro opcional "SUFIXO" constando como obrigatório, na parte recursiva.
#xcommand DEF_NO_CAMPO <O_Campo> DOMINIO ;
   [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
   [ SE <X_Conteudo_1> TITULO <C_Titulo_1> SUFIXO <C_SufixoDoDefine_1>  ;
     [, SE <X_Conteudo_N> TITULO <C_Titulo_N> SUFIXO <C_SufixoDoDefine_N> ] ] => ;
     DEF_NO_CAMPO_DOMINIO(@<O_Campo>,<C_ArquivoCH>,<C_PrefixoDoDefine>,-1) ;;
     [ ADD_NO_CAMPO_LINHA_DE_DOMINIO(@<O_Campo>,<X_Conteudo_1>,<C_Titulo_1>,<C_SufixoDoDefine_1>,-1) ] ;
     [ ; ADD_NO_CAMPO_LINHA_DE_DOMINIO(@<O_Campo>,<X_Conteudo_N>,<C_Titulo_N>,<C_SufixoDoDefine_N>,-1) ] ;;
     DEF_NO_CAMPO_DOMINIO_FINALIZACAO(<O_Campo>,-1) 

* 1.2) Sem parâmetro opcional, na parte recursiva.
#xcommand DEF_NO_CAMPO <O_Campo> DOMINIO ;
   [ARQUIVO_CH <C_ArquivoCH> [PREFIXO <C_PrefixoDoDefine>]] ;
   [ SE <X_Conteudo_1> TITULO <C_Titulo_1> ;
     [, SE <X_Conteudo_N> TITULO <C_Titulo_N> ] ] => ;
     DEF_NO_CAMPO_DOMINIO(@<O_Campo>,<C_ArquivoCH>,<C_PrefixoDoDefine>,-1) ;;
     [ ADD_NO_CAMPO_LINHA_DE_DOMINIO(@<O_Campo>,<X_Conteudo_1>,<C_Titulo_1>,NIL,-1) ] ;
     [ ; ADD_NO_CAMPO_LINHA_DE_DOMINIO(@<O_Campo>,<X_Conteudo_N>,<C_Titulo_N>,NIL,-1) ] ;;
     DEF_NO_CAMPO_DOMINIO_FINALIZACAO(<O_Campo>,-1)

**********************
* comando DEF_REGISTRO
**********************
* Nota: Por ser a definição de UM registro, o nome deve ser grafado sempre no SINGULAR.
*       Mas caso não exista grafia no singular, usar o parâmetro SO_PLURAL.
*         Exemplo: Se NOME for "Arroz", escrever no singular e não usar nada
*         Exemplo: Se NOME for "Férias", escrever no plural e usar SO_PLURAL
*
* Nota: Os parâmetros MASCULINO, FEMININO
*       devem ser utilizados pois um registro é sempre um substantivo,
*       devendo-se indicar o gênero do parâmetro NOME.
*         Exemplo: Se NOME for "Adulto", usar MASCULINO
*         Exemplo: Se NOME for "Pessoa", usar FEMININO
*
* DEF_REGISTRO <O_Registro> ; 
*    NOME <C_NomeCompletoSingular> [RESUMIDO <C_NomeResumidoSingular>] ;
*    [ SO_PLURAL ] ;
*    MASCULINO | FEMININO ;
*
#xcommand DEF_REGISTRO <O_Registro> ;
          NOME <C_NomeCompletoSingular> ;
          [RESUMIDO <C_NomeResumidoSingular>] ;
          [<L_FlexaoNumeroSoPlural: SO_PLURAL>] ;
          MASCULINO ;
          => <O_Registro> := DEF_REGISTRO(<C_NomeCompletoSingular>,<C_NomeResumidoSingular>,;
                                    <.L_FlexaoNumeroSoPlural.>,;
                                    .T.,.F.,;
                                   -1)  // Vide texto da rotina, para explicação sobre sobre "-1"
*
#xcommand DEF_REGISTRO <O_Registro> ;
          NOME <C_NomeCompletoSingular> ;
          [RESUMIDO <C_NomeResumidoSingular>] ;
          [<L_FlexaoNumeroSoPlural: SO_PLURAL>] ;
          FEMININO ;
          => <O_Registro> := DEF_REGISTRO(<C_NomeCompletoSingular>,<C_NomeResumidoSingular>,;
                                    <.L_FlexaoNumeroSoPlural.>,;
                                    .F.,.T.,;
                                   -1)  // Vide texto da rotina, para explicação sobre sobre "-1"
*

***************************************
* comando ADD_AO_REGISTRO ... DESCRICAO
***************************************
* ADD_AO_REGISTRO <O_Registro> DESCRICAO CLIENTE ;
*   <C_DescricaoCliente>
* ADD_AO_REGISTRO <O_Registro> DESCRICAO ATENDENTE ;
*    <C_DescricaoAtendente>
* ADD_AO_REGISTRO <O_Registro> DESCRICAO ANALISTA ;
*    <C_DescricaoAnalista>
* ADD_AO_REGISTRO <O_Registro> DESCRICAO DESENVOLVEDOR ;
*    <C_DescricaoDesenvolvedor>
*    
#xcommand ADD_AO_REGISTRO <O_Registro> ;
              DESCRICAO CLIENTE <C_DescricaoCliente> ;
          => ADD_AO_REGISTRO_DESCRICAO_CLIENTE(@<O_Registro>,;
                 <C_DescricaoCliente>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_REGISTRO <O_Registro> ;
              DESCRICAO ATENDENTE <C_DescricaoAtendente> ;
          => ADD_AO_REGISTRO_DESCRICAO_ATENDENTE(@<O_Registro>,;
                 <C_DescricaoAtendente>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_REGISTRO <O_Registro> ;
              DESCRICAO ANALISTA <C_DescricaoAnalista> ;
          => ADD_AO_REGISTRO_DESCRICAO_ANALISTA(@<O_Registro>,;
                 <C_DescricaoAnalista>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

#xcommand ADD_AO_REGISTRO <O_Registro> ;
              DESCRICAO DESENVOLVEDOR <C_DescricaoDesenvolvedor> ;
          => ADD_AO_REGISTRO_DESCRICAO_DESENVOLVEDOR(@<O_Registro>,;
                 <C_DescricaoDesenvolvedor>,;
                 -1)  // Vide texto da rotina, para explicação sobre sobre o "-1"

***********************************
* comando ADD_AO_REGISTRO ... CAMPO   // Adiciona um campo já previamente existente a um registro
***********************************
* ADD_AO_REGISTRO <O_Registro> CAMPO <O_Campo> ;
*   ID <C_IdCompletoDoCampo> ;
*   IDCURTO <C_IdCurtoDoCampo> ;
*   [ OPCIONAL ] ;
*   [ NOME <C_NomeCompletoSingularAlternativo> ] ;
*   [ RESUMIDO <C_NomeResumidoSingularAlternativo> ]
*
#xcommand ADD_AO_REGISTRO <O_Registro> CAMPO <O_Campo> ;
     ID <C_IdCompletoDoCampo> ;
     IDCURTO <C_IdCurtoDoCampo> ;
     [<L_CampoOpcional: OPCIONAL>] ;
     [NOME <C_NomeCompletoSingularAlternativo>] ;
     [RESUMIDO <C_NomeResumidoSingularAlternativo>] => ;
     ADD_AO_REGISTRO_CAMPO(@<O_Registro>,<O_Campo>,;
                          <C_IdCompletoDoCampo>,;  
                          <C_IdCurtoDoCampo>,;  
                          <.L_CampoOpcional.>,;
                          .F.,;
                          <C_NomeCompletoSingularAlternativo>,; 
                          <C_NomeResumidoSingularAlternativo>,; 
                          -1)  // Vide texto da rotina, para explicação sobre sobre "-1"


******************************************************
* comando ADD_AO_REGISTRO ... TABELA_DECISAO ... SOBRE    // Adiciona ao registro uma tabela de decisão
******************************************************
* ADD_AO_REGISTRO <O_Registro> ;
*   TABELA_DECISAO <V_Lst_IdCurtoDoCampoAnterior> ;
*   SOBRE <C_IdCurtoDoCampoPosterior> [ IDCURTO ]
*
* Nota: Futuramente o parâmetro IDCURTO será opcional,
*       devendo o comando abaixo ser modificado para:
* #xcommand ADD_AO_REGISTRO <O_Registro> ;
*             TABELA_DECISAO <V_Lst_IdCurtoDoCampoAnterior> ;
*             SOBRE <C_IdCurtoDoCampoPosterior> [<L_UsaIdCurto: IDCURTO>] => ;
*           ADD_AO_REGISTRO_TABELA_DECISAO(@<O_Registro>,NIL,;
*                                          <V_Lst_IdCurtoDoCampoAnterior>,;
*                                          <C_IdCurtoDoCampoPosterior>,<.L_UsaIdCurto.>,-1)
*
#xcommand ADD_AO_REGISTRO <O_Registro> ;
            TABELA_DECISAO <V_Lst_IdCurtoDoCampoAnterior> ;
            SOBRE <C_IdCurtoDoCampoPosterior> IDCURTO => ;
          ADD_AO_REGISTRO_TABELA_DECISAO(@<O_Registro>,NIL,;
                                         <V_Lst_IdCurtoDoCampoAnterior>,;
                                         <C_IdCurtoDoCampoPosterior>,.T.,-1)

****************************************************
* comando ADD_AO_REGISTRO ... SOBRE ... SE ... ENTAO     // Adiciona ao registro uma linha na tabela de decisão
****************************************************
* ADD_AO_REGISTRO <O_Registro> ;
*   SOBRE <C_IdCurtoDoCampoPosterior> ;
*   SE <V_Lst_X_ConteudosDosCamposAnteriores_1> ENTAO <V_Lst_X_ConteudosDoCampoPosterior_1>,; 
*   SE <V_Lst_X_ConteudosDosCamposAnteriores_N> ENTAO <V_Lst_X_ConteudosDoCampoPosterior_N>
*
* - Nos comandos recursivos, pode ocorrer do programador não escrever a parte recursiva 
*   ou escrever somente uma linha na parte recursiva.
*   Neste caso, SEM QUE SAIBAMOS O MOTIVO, o pré-processador está fazendo uma chamada 
*   adicional à rotina recursiva ADD_NO_CAMPO_LINHA_DE_DOMINIO(),
*   com todos os parâmetros recursivos passados sem contéudo (vide exemplo na DEF_DOMINIO, acima).
* 

#xcommand ADD_AO_REGISTRO <O_Registro> SOBRE <C_IdCurtoDoCampoPosterior> ;
           [ SE <V_Lst_X_ConteudosDosCamposAnteriores_1> ENTAO <V_Lst_X_ConteudosDoCampoPosterior_1> ;
             [, SE <V_Lst_X_ConteudosDosCamposAnteriores_N> ENTAO <V_Lst_X_ConteudosDoCampoPosterior_N> ] ] => ;
          ADD_AO_REGISTRO_TABELA_DECISAO_SE(@<O_Registro>,<C_IdCurtoDoCampoPosterior>,;
                                            <V_Lst_X_ConteudosDosCamposAnteriores_1>,;
                                            <V_Lst_X_ConteudosDoCampoPosterior_1>,-1) ;
          [ ; ADD_AO_REGISTRO_TABELA_DECISAO_SE(@<O_Registro>,<C_IdCurtoDoCampoPosterior>,;
                                               <V_Lst_X_ConteudosDosCamposAnteriores_N>,;
                                               <V_Lst_X_ConteudosDoCampoPosterior_N>,-1) ]

*********************************************
* comando ADD_AO_REGISTRO ... SOBRE ... SENAO     // Adiciona ao registro uma linha de alternativa na tabela de decisão
*********************************************
* ADD_AO_REGISTRO <O_Registro> ;
*   SOBRE <C_IdCurtoDoCampoPosterior> ;
*   SENAO <V_Lst_X_ConteudosSenaoDoCampoPosterior> 
*
#xcommand ADD_AO_REGISTRO <O_Registro> SOBRE <C_IdCurtoDoCampoPosterior> ;
            SENAO <V_Lst_X_ConteudosSenaoDoCampoPosterior> => ;
            ADD_AO_REGISTRO_TABELA_DECISAO_SENAO(@<O_Registro>,<C_IdCurtoDoCampoPosterior>,;
                                                 <V_Lst_X_ConteudosSenaoDoCampoPosterior>,-1)

*********************************************
* comando ADD_REGISTRO ... TABELA_PERSISTENTE
*********************************************
* ADD_REGISTRO <O_Registro> TABELA_PERSISTENTE <C_NomeTabelaPersistente> ;
*    NOME <C_NomeCompletoPlural> [RESUMIDO <C_NomeResumidoPlural>]
*

#xcommand ADD_REGISTRO <O_Registro> TABELA_PERSISTENTE <C_NomeTabelaPersistente> ;
          NOME <C_NomeCompletoPlural> ;
          [RESUMIDO <C_NomeResumidoPlural>] ;
          => ADD_REGISTRO_TABELA_PERSISTENTE(@<O_Registro>,;
                                   <C_NomeTabelaPersistente>,; 
                                   <C_NomeCompletoPlural>,<C_NomeResumidoPlural>,;
                                   -1)  // Vide texto da rotina, para explicação sobre sobre "-1"



#endif    /* DEF_DADOS_CH */