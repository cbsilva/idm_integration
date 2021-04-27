 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUserData NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "UserData"
    FIELD accountId  AS CHARACTER FORMAT "x(12)"
    FIELD firstName  AS CHARACTER FORMAT "x(50)"
    FIELD lastName   AS CHARACTER FORMAT "x(50)"
    FIELD email      AS CHARACTER FORMAT "x(50)"
    FIELD department AS CHARACTER FORMAT "x(30)"
    INDEX idxUser IS PRIMARY accountId.




DEFINE TEMP-TABLE ttUserUpdate NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME "UserData"
    FIELD accountId  AS CHARACTER FORMAT "x(12)"
    INDEX idxUser IS PRIMARY accountId.
