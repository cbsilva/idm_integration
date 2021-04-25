 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUserData NO-UNDO XML-NODE-NAME "UserData"
    FIELD accountId  AS CHARACTER 
    FIELD firstName  AS CHARACTER 
    FIELD lastName   AS CHARACTER 
    FIELD email      AS CHARACTER 
    FIELD department AS CHARACTER 
    INDEX idxUser IS PRIMARY accountId.
