/*---------------------------------------------------------
 Procedures universais
 ---------------------------------------------------------*/
PROCEDURE piValidUserAccount:
/*---------------------------------------------------------------
 Purpose: verifica se o usuario enviado existe na base
---------------------------------------------------------------*/
    DEFINE INPUT  PARAM pUser         AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAM pValidateUser AS LOG  NO-UNDO.

    ASSIGN pValidateUser = CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = pUser NO-LOCK).

END PROCEDURE.



PROCEDURE piGetUser:
/*---------------------------------------------------------------
 Purpose: retorna os dados do usuario
---------------------------------------------------------------*/
    DEFINE INPUT PARAM pUser AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAM TABLE FOR ttUser.
    DEFINE OUTPUT PARAM TABLE FOR ttGroups.

    //definicao de variaveis locais
    DEFINE VARIABLE i-pos-first AS INTEGER INITIAL 1  NO-UNDO.
    DEFINE VARIABLE i-pos-last  AS INTEGER            NO-UNDO.
    DEFINE VARIABLE i-length    AS INTEGER            NO-UNDO.
    DEFINE VARIABLE c-firstname AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE c-lastname  AS CHARACTER          NO-UNDO.


    FOR FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = pUser NO-LOCK:

        //verifica o tamanho do nome do usuario    
        ASSIGN i-length    = LENGTH(usuar_mestre.nom_usuario) 
               i-pos-last  = INDEX(usuar_mestre.nom_usuario, ' ').  

        //separa a primeira parte do nome e o que sobrar envia como sobrenome
        ASSIGN c-firstname = SUBSTRING(usuar_mestre.nom_usuario,i-pos-first,i-pos-last - 1, "CHAR")
               c-lastname  = SUBSTRING(usuar_mestre.nom_usuario,i-pos-last,i-length, "CHAR").
            
        CREATE ttUser.
        ASSIGN ttUser.accountId   = usuar_mestre.cod_usuario
               ttUser.firstname   = c-firstname
               ttUser.lastname    = c-lastname
               ttUser.email       = usuar_mestre.cod_e_mail_local  
               ttUser.department  = "".

        //cria temp-table de grupo de usuarios
        FOR EACH usuar_grp_usuar NO-LOCK
           WHERE usuar_grp_usuar.cod_usuario = usuar_mestre.cod_usuario:
            CREATE ttGroups.
            ASSIGN ttGroups.groupCode = usuar_grp_usuar.cod_grp_usuar
                   .
        END.
    END.

END PROCEDURE.
