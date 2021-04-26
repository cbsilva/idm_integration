/*------------------------------------------------------------------------
    File        : checkUpdateAccount.p
    Description : Programa para checar as informa‡äes do usuario
    Author(s)   : Cleberson Silva - 4Make
    Created     : 21/04/2021
    Notes       :
 ------------------------------------------------------------------------*/
 
 /* ***************************  Definitions  ************************** */
 
{include/i-prgvrs.i getUser 2.00.00.001}
{METHOD/dbotterr.i}

{include/getUser.i}
{include/userData.i}
{include/userRoles.i "'Roles'" "'RoleData'"}
{include/ttResponse.i "'checkUpdateAccountResponse'"}

/* global variable definitions */

/* local variable definitions */
DEFINE VARIABLE lRetOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lRetorno AS LOGICAL INITIAL FALSE NO-UNDO.


/*-- dataset definitions --*
DEFINE DATASET dsUser 
    NAMESPACE-URI "webservice.idm.bosch.com:types" 
    XML-NODE-NAME "checkCreateAccount"
    FOR ttUserData.
*/

DEFINE TEMP-TABLE ttUsuario NO-UNDO
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "UserData"
    FIELD accountId AS CHAR.


DEFINE DATASET dsUserData 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "checkUpdateAccount" 
    FOR ttUsuario, ttRoles, ttUserRoles
    DATA-RELATION drRoles     FOR ttUsuario, ttRoles   RELATION-FIELDS(accountId, accountId) NESTED
    DATA-RELATION drUserRoles FOR ttRoles, ttUserRoles RELATION-FIELDS(accountId, accountId) NESTED.


DEFINE DATASET dsRetorno
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "checkUpdateAccountResponse"
    XML-NODE-TYPE "HIDDEN"
    SERIALIZE-HIDDEN
    FOR ttRetorno.                                


/* ***************************  Main Block  ************************** */
DEFINE INPUT  PARAMETER DATASET FOR dsUserData.
DEFINE OUTPUT PARAMETER DATASET FOR dsRetorno.



//RUN piUpdateAccount(INPUT accountId, INPUT TABLE ttUserRoles).

LOG-MANAGER:WRITE-MESSAGE(SUBSTITUTE(">>> VALOR DE RETURN-VALUE &1",RETURN-VALUE)) NO-ERROR.


CREATE ttRetorno.
ASSIGN ttRetorno.logUser = IF RETURN-VALUE = "OK":U THEN TRUE ELSE FALSE.



PROCEDURE piValidUserAccount:
/*---------------------------------------------------------------
 Purpose: Checks if the user sent exists in the database
---------------------------------------------------------------*/
    DEFINE INPUT PARAM pUser AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAM TABLE FOR  ttUserData.


    FIND FIRST ttUserData NO-LOCK NO-ERROR.
    IF NOT AVAIL ttUserData THEN
    DO:
        RETURN "NOK":U.
    END.
    ELSE
    DO:
        /*******************************************
          As validacoes foram separadas, por que 
          o AppServer, estava dando falso positivo
          de forma intermitente - CPAS
        ********************************************/

        IF ttUserData.accountId = "?"  OR
           ttUserData.firstName = "?"  OR
           ttUserData.email     = "?"  THEN
            RETURN "NOK":U.


        IF ttUserData.accountId = ""  OR
           ttUserData.firstName = ""  OR
           ttUserData.email     = ""  THEN
            RETURN "NOK":U.


        IF CAN-FIND(FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = ttUserData.accountId NO-LOCK) THEN
            RETURN "NOK":U.


        RETURN "OK":U.
    END.

END PROCEDURE.







  
