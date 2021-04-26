 /**********************************************************
    TEMP-TABLES - WEBSERVICE INTEGRATION IDM X TOTVS
 ***********************************************************/

DEFINE TEMP-TABLE ttUserData NO-UNDO XML-NODE-NAME "UserData"
    FIELD accountId  AS CHARACTER FORMAT "x(12)"
    FIELD firstName  AS CHARACTER FORMAT "x(50)"
    FIELD lastName   AS CHARACTER FORMAT "x(50)"
    FIELD email      AS CHARACTER FORMAT "x(50)"
    FIELD department AS CHARACTER FORMAT "x(30)"
    INDEX idxUser IS PRIMARY accountId.
