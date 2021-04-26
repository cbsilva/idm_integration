 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttRoles NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    SERIALIZE-NAME {1} 
    FIELD accountId AS CHARACTER SERIALIZE-HIDDEN.

DEFINE TEMP-TABLE ttUserRoles NO-UNDO 
    NAMESPACE-URI "webservice.idm.bosch.com:types"
    XML-NODE-NAME {2}
    FIELD accountId AS CHAR XML-NODE-TYPE 'hidden'
    FIELD groupCode AS CHAR XML-NODE-NAME 'name'
    INDEX idxGroupCode IS PRIMARY accountId groupCode.
