// Temp-table ---
DEFINE TEMP-TABLE tt-usuar-grp-usuar NO-UNDO
    FIELD cod-usuario   AS CHARACTER
    FIELD cod-grp-usuar AS CHARACTER.

DEFINE TEMP-TABLE tt-prog-dtsul-segur NO-UNDO
    FIELD cod-programa AS CHARACTER
    FIELD cod-grupo    AS CHARACTER.

DEFINE TEMP-TABLE tt-de-para-grupo NO-UNDO
    FIELD cod-grupo AS CHARACTER
    FIELD nom-grupo AS CHARACTER.

// Funá‰es ---
FUNCTION retornarUltimoProgDtsulSegur RETURNS INTEGER
  ( )  FORWARD.

// Buffers ---
DEFINE BUFFER b_prog_dtsul_segur_last FOR prog_dtsul_segur.

// Variaveis ---
DEFINE VARIABLE c-arquivo           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-linha             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iUltimoIDProgDtsul  AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-grp-usuar     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-teste             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE h-acomp             AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-cont1             AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont2             AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont3             AS INTEGER     NO-UNDO.

ASSIGN l-teste = NO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Carregando *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

// Limpa Tamp-Table ---
EMPTY TEMP-TABLE tt-usuar-grp-usuar.
EMPTY TEMP-TABLE tt-prog-dtsul-segur.
EMPTY TEMP-TABLE tt-de-para-grupo.

// Carrega DE-PARA grupo
RUN pi-carrega-de-para.

// Importa CSV ---
ASSIGN c-arquivo = "D:\Users\jgargavisa2\Desktop\tst\importar.csv".
FILE-INFO:FILE-NAME = SEARCH(c-arquivo).

INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) CONVERT TARGET "ibm850".
    REPEAT:

        ASSIGN i-cont1 = i-cont1 + 1.

        RUN pi-acompanhar IN h-acomp (INPUT "Linha : " + STRING(i-cont1)).

        IMPORT UNFORMATTED c-linha.

        ASSIGN c-cod-grp-usuar = ENTRY(1,c-linha,";").

        FIND FIRST tt-de-para-grupo NO-LOCK
             WHERE tt-de-para-grupo.nom-grupo = ENTRY(1,c-linha,";") NO-ERROR.
        IF AVAIL tt-de-para-grupo THEN
            ASSIGN c-cod-grp-usuar = tt-de-para-grupo.cod-grupo.
            
        // Grupo x Usu†rio ---
        FIND FIRST tt-usuar-grp-usuar
             WHERE tt-usuar-grp-usuar.cod-usuario   = ENTRY(2,c-linha,";")
               AND tt-usuar-grp-usuar.cod-grp-usuar = c-cod-grp-usuar NO-ERROR.
        IF NOT AVAIL tt-usuar-grp-usuar THEN DO:
            CREATE tt-usuar-grp-usuar.
            ASSIGN tt-usuar-grp-usuar.cod-usuario   = ENTRY(2,c-linha,";")
                   tt-usuar-grp-usuar.cod-grp-usuar = c-cod-grp-usuar.

            ASSIGN i-cont2 = i-cont2 + 1.
        END.

        // Grupo x Programa ---
        FIND FIRST tt-prog-dtsul-segur
             WHERE tt-prog-dtsul-segur.cod-programa = ENTRY(3,c-linha,";")
               AND tt-prog-dtsul-segur.cod-grupo    = c-cod-grp-usuar NO-ERROR.
        IF NOT AVAIL tt-prog-dtsul-segur THEN DO:
            CREATE tt-prog-dtsul-segur.
            ASSIGN tt-prog-dtsul-segur.cod-programa = ENTRY(3,c-linha,";")
                   tt-prog-dtsul-segur.cod-grupo    = c-cod-grp-usuar.

            ASSIGN i-cont3 = i-cont3 + 1.
        END.
    END.

INPUT CLOSE.
ASSIGN i-cont1 = 0.
// Vincula usu†rio ao grupo de seguranáa ---
OUTPUT TO VALUE ("D:\Users\jgargavisa2\Desktop\tst\log-usuar-grp-usuar.txt") NO-ECHO NO-CONVERT.
PUT UNFORMATTED "Usu†rio"  CHR(9)
                "Grupo"    CHR(9)
                "Status"   SKIP.

FOR EACH tt-usuar-grp-usuar NO-LOCK:

    ASSIGN i-cont1 = i-cont1 + 1.
    RUN pi-acompanhar IN h-acomp (INPUT "Grp x Usr : " + STRING(i-cont1) + " / " + STRING(i-cont2)).

    PUT UNFORMATTED tt-usuar-grp-usuar.cod-usuario   CHR(9)
                    tt-usuar-grp-usuar.cod-grp-usuar CHR(9).

    FIND FIRST usuar_mestre NO-LOCK
         WHERE usuar_mestre.cod_usuar = tt-usuar-grp-usuar.cod-usuario NO-ERROR.
    IF NOT AVAIL usuar_mestre THEN DO:
        PUT UNFORMATTED "Usu†rio n∆o encontrado." SKIP.
        NEXT.
    END.

    FIND FIRST grp_usuar NO-LOCK
         WHERE grp_usuar.cod_grp_usuar = tt-prog-dtsul-segur.cod-grupo NO-ERROR.
    IF NOT AVAIL grp_usuar THEN DO:
        PUT UNFORMATTED "Grupo de Usu†rio n∆o encontrado." SKIP.
        NEXT.
    END.

    FIND FIRST usuar_grp_usuar
         WHERE usuar_grp_usuar.cod_usuario   = tt-usuar-grp-usuar.cod-usuario
           AND usuar_grp_usuar.cod_grp_usuar = tt-usuar-grp-usuar.cod-grp-usuar  NO-ERROR.
    IF NOT AVAIL usuar_grp_usuar THEN DO:

        IF NOT l-teste THEN DO:
            CREATE usuar_grp_usuar.
            ASSIGN usuar_grp_usuar.cod_grp_usuar = tt-usuar-grp-usuar.cod-grp-usuar
                   usuar_grp_usuar.cod_usuario   = tt-usuar-grp-usuar.cod-usuario.
        END.


        PUT UNFORMATTED "Relacionamento criado com sucesso." SKIP.
    END.
    ELSE
        PUT UNFORMATTED "Relacionamento j† cadastrado." SKIP.

END.
OUTPUT CLOSE.
ASSIGN i-cont1 = 0.
// Vincula grupo de seguranáa ao programa ---
OUTPUT TO VALUE ("D:\Users\jgargavisa2\Desktop\tst\log-prog-dtsul-segur.txt") NO-ECHO NO-CONVERT.
PUT UNFORMATTED "Programa" CHR(9)
                "Grupo"    CHR(9)
                "Status"   SKIP.

FOR EACH tt-prog-dtsul-segur:
        
    ASSIGN i-cont1 = i-cont1 + 1.    
    RUN pi-acompanhar IN h-acomp (INPUT "Grp x Pro : " + STRING(i-cont1) + " / " + STRING(i-cont3)).

    PUT UNFORMATTED tt-prog-dtsul-segur.cod-programa CHR(9)
                    tt-prog-dtsul-segur.cod-grupo    CHR(9).

    FIND FIRST prog_dtsul NO-LOCK
         WHERE prog_dtsul.cod_prog_dtsul = tt-prog-dtsul-segur.cod-programa NO-ERROR.
    IF NOT AVAIL prog_dtsul THEN DO:
        PUT UNFORMATTED "Programa n∆o encontrado." SKIP.
        NEXT.
    END.

    FIND FIRST grp_usuar NO-LOCK
         WHERE grp_usuar.cod_grp_usuar = tt-prog-dtsul-segur.cod-grupo NO-ERROR.
    IF NOT AVAIL grp_usuar THEN DO:
        PUT UNFORMATTED "Grupo de Usu†rio n∆o encontrado." SKIP.
        NEXT.
    END.

    FIND FIRST prog_dtsul_segur
         WHERE prog_dtsul_segur.idi_dtsul_grp_usuar  = grp_usuar.idi_dtsul
           AND prog_dtsul_segur.idi_dtsul_prog_dtsul = prog_dtsul.idi_dtsul NO-ERROR.
    IF NOT AVAIL prog_dtsul_segur THEN DO:
        
        ASSIGN iUltimoIDProgDtsul = retornarUltimoProgDtsulSegur().
        IF NOT l-teste THEN DO:
            CREATE prog_dtsul_segur.
            ASSIGN prog_dtsul_segur.cod_grp_usuar        = tt-prog-dtsul-segur.cod-grupo
                   prog_dtsul_segur.cod_prog_dtsul       = tt-prog-dtsul-segur.cod-programa
                   prog_dtsul_segur.idi_dtsul            = iUltimoIDProgDtsul
                   prog_dtsul_segur.idi_dtsul_grp_usuar  = grp_usuar.idi_dtsul
                   prog_dtsul_segur.idi_dtsul_prog_dtsul = prog_dtsul.idi_dtsul.
        END.
        PUT UNFORMATTED "Relacionamento criado com sucesso." SKIP.
    END.
    ELSE
        PUT UNFORMATTED "Relacionamento j† cadastrado." SKIP.
END.
OUTPUT CLOSE.

RUN pi-finalizar IN h-acomp.

RETURN "OK".



// Funá‰es ---
FUNCTION retornarUltimoProgDtsulSegur RETURNS INTEGER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  retornar o ultimo registro da prog_dtsul_segur e devolver o idi_dtsul 
    Notes:  
------------------------------------------------------------------------------*/
FOR LAST b_prog_dtsul_segur_last
    FIELDS( b_prog_dtsul_segur_last.idi_dtsul 
            b_prog_dtsul_segur_last.idi_dtsul_grp_usuar 
            b_prog_dtsul_segur_last.idi_dtsul_prog_dtsul 
            b_prog_dtsul_segur_last.cod_grp_usuar 
            b_prog_dtsul_segur_last.cod_prog_dtsul )
    NO-LOCK 
    USE-INDEX prgdtsls_frwk:

END.

IF  AVAIL b_prog_dtsul_segur_last THEN
    RETURN b_prog_dtsul_segur_last.idi_dtsul + 1.
ELSE
    RETURN 1.   /* Function return value. */

END FUNCTION.


// Procedure ---

PROCEDURE pi-carrega-de-para:
    
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AAC" tt-de-para-grupo.nom-grupo = "Analista Adm Contas Receber e Contas Receber". 
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AAV" tt-de-para-grupo.nom-grupo = "Analista Administraá∆o Vendas".                
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ACC" tt-de-para-grupo.nom-grupo = "Analista Cadastro CliFor".                     
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CPL" tt-de-para-grupo.nom-grupo = "Analista Cont†bil Pleno".                      
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ACS" tt-de-para-grupo.nom-grupo = "Analista Cont†bil Sànior".                     
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ACP" tt-de-para-grupo.nom-grupo = "Analista Contas Pagar".                        
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ACR" tt-de-para-grupo.nom-grupo = "Analista Contas Receber".                      
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CCR" tt-de-para-grupo.nom-grupo = "Analista CrÇdito e Contas Receber".            
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ANF" tt-de-para-grupo.nom-grupo = "Analista Faturamento".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AFI" tt-de-para-grupo.nom-grupo = "Analista Fiscal".                              
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AFE" tt-de-para-grupo.nom-grupo = "Analista Fiscal Externo".                      
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AIE" tt-de-para-grupo.nom-grupo = "Analista Importaá∆o e Exportaá∆o".             
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "APF" tt-de-para-grupo.nom-grupo = "Analista Planejamento Financeiro".             
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "APR" tt-de-para-grupo.nom-grupo = "Analista Projetos".                            
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ATE" tt-de-para-grupo.nom-grupo = "Analista Tesouraria".                          
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ADV" tt-de-para-grupo.nom-grupo = "Assistente Administraá∆o Vendas".              
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ASA" tt-de-para-grupo.nom-grupo = "Assistente Administrativo".                    
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AAL" tt-de-para-grupo.nom-grupo = "Assistente Almoxarifado".                      
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AEX" tt-de-para-grupo.nom-grupo = "Assistente Expediá∆o".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "AQA" tt-de-para-grupo.nom-grupo = "Assistente Qualidade".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "ARE" tt-de-para-grupo.nom-grupo = "Assistente Recebimento".                       
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CSE" tt-de-para-grupo.nom-grupo = "Compradora Sànior".                            
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "COC" tt-de-para-grupo.nom-grupo = "Coordenador Controladoria".                    
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "COL" tt-de-para-grupo.nom-grupo = "Coordenador Log°stica".                        
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "COQ" tt-de-para-grupo.nom-grupo = "Coordenador Qualidade".                        
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CCO" tt-de-para-grupo.nom-grupo = "Coordenadora Compras".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CIE" tt-de-para-grupo.nom-grupo = "Coordenadora Importaá∆o e Exportaá∆o".         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "CVD" tt-de-para-grupo.nom-grupo = "Coordenadora Vendas Diretas".                  
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "EEL" tt-de-para-grupo.nom-grupo = "Engenheiro Eletronico".                        
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "PLM" tt-de-para-grupo.nom-grupo = "Planejador Materiais".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "REC" tt-de-para-grupo.nom-grupo = "Requisitante Compras".                         
    CREATE tt-de-para-grupo. ASSIGN tt-de-para-grupo.cod-grupo = "SST" tt-de-para-grupo.nom-grupo = "Supervisor Sup. Tec.".                         

END PROCEDURE.

