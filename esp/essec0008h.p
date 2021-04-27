/*------------------------------------------------------------------------
    File        : getUser.p
    Description : Metodo para retornar informa‡äes do usuario
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i getUser 2.00.00.001}
{METHOD/dbotterr.i}

{include/getUser.i}
{include/userData.i}
{include/userRoles.i "'Roles'" "'Role'"}

/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE lRetOK AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttAttIDM NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "GetUserAtribute"
    FIELD id AS CHARACTER INITIAL "urn:webservice.idm.bosch.com:types" XML-NODE-TYPE "ELEMENT".


/* dataset definitions */
DEFINE DATASET dsGetUser  
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    SERIALIZE-HIDDEN
    FOR ttAttIDM, ttUser
    DATA-RELATION relUserId FOR ttAttIDM, ttUser RELATION-FIELDS(id, accountId) NESTED.


DEFINE DATASET dsRetUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "UserData" SERIALIZE-HIDDEN FOR ttUserData, ttRoles, ttUserRoles,ttErrorMessage
    DATA-RELATION drRoles   FOR ttUserData, ttRoles   RELATION-FIELDS(accountId, accountId) NESTED
    DATA-RELATION UserRoles FOR ttRoles, ttUserRoles  RELATION-FIELDS(accountId, accountId) NESTED.
    



/* ***************************  Main Block  ************************** */
DEFINE INPUT PARAMETER  accountId AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetUser.



/* run procedure piGetUser */
RUN piGetUser(INPUT accountId, INPUT TABLE ttUser, OUTPUT TABLE ttUserData, OUTPUT TABLE ttUserRoles, OUTPUT TABLE ttErrorMessage).




PROCEDURE piGetUser:
/*---------------------------------------------------------------
 Purpose: Return user information
---------------------------------------------------------------*/
    DEFINE INPUT  PARAM pUser AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAM TABLE FOR ttUser.
    DEFINE OUTPUT PARAM TABLE FOR ttUserData.
    DEFINE OUTPUT PARAM TABLE FOR ttUserRoles.
    DEFINE OUTPUT PARAM TABLE FOR ttErrorMessage.

    //definicao de variaveis locais
    DEFINE VARIABLE i-pos-first   AS INTEGER INITIAL 1  NO-UNDO.
    DEFINE VARIABLE i-pos-last    AS INTEGER            NO-UNDO.
    DEFINE VARIABLE i-length      AS INTEGER            NO-UNDO.
    DEFINE VARIABLE c-firstname   AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE c-lastname    AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE h-buffer-xml  AS HANDLE             NO-UNDO.
    
    EMPTY TEMP-TABLE ttUserData.
    EMPTY TEMP-TABLE ttUserRoles.
    
    IF accountId = "" THEN
    DO:
    
        CREATE ttErrorMessage.
        ASSIGN ttErrorMessage.ErrorCode        = "INVALID_PARAMETER"
               ttErrorMessage.ErrorDescription = "Field cannot be null".

        RETURN "NOK":U.

    END.


    FOR FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = pUser NO-LOCK:

        //verifica o tamanho do nome do usuario    
        ASSIGN i-length    = LENGTH(usuar_mestre.nom_usuario) 
               i-pos-last  = INDEX(usuar_mestre.nom_usuario, ' ').  

        //separa a primeira parte do nome e o que sobrar envia como sobrenome
        ASSIGN c-firstname = SUBSTRING(usuar_mestre.nom_usuario,i-pos-first,i-pos-last - 1, "CHAR")
               c-lastname  = SUBSTRING(usuar_mestre.nom_usuario,i-pos-last,i-length, "CHAR").
            
        CREATE ttUserData.
        ASSIGN ttUserData.accountId   = usuar_mestre.cod_usuario
               ttUserData.firstname   = c-firstname
               ttUserData.lastname    = c-lastname
               ttUserData.email       = usuar_mestre.cod_e_mail_local  
               ttUserData.department  = "".


        CREATE ttRoles.
        ASSIGN ttRoles.accountId = usuar_mestre.cod_usuario.


        //cria temp-table de grupo de usuarios
        FOR EACH usuar_grp_usuar NO-LOCK
           WHERE usuar_grp_usuar.cod_usuario = usuar_mestre.cod_usuario:
            CREATE ttUserRoles.
            ASSIGN ttUserRoles.accountId = usuar_grp_usuar.cod_usuario
                   ttUserRoles.groupCode = usuar_grp_usuar.cod_grp_usuar.
            
        END.
    END.
    
    IF NOT CAN-FIND(FIRST ttUserData NO-LOCK) THEN
    DO:
        CREATE ttErrorMessage.
        ASSIGN ttErrorMessage.ErrorCode        = "OBJECT_NOT_FOUND"
               ttErrorMessage.ErrorDescription = SUBSTITUTE("Search criteria did not match any object(s) [&1]",pUser).
        
    END.

END PROCEDURE.
