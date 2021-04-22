DEFINE VARIABLE i-position     AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-length       AS INTEGER     NO-UNDO.
ASSIGN c-group = "DCBR_LOG_CUA058_W_UH1".


ASSIGN i-length   = LENGTH(c-group)
       i-position = i-length - 2.


MESSAGE i-length SKIP i-position SKIP SUBSTRING(c-group,i-position,i-length, "CHAR")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*


DISP LENGTH(c-group) 
    SKIP 
    LENGTH(c-group2) SKIP
       SUBSTRING(c-group,19,21, "CHAR") SKIP
       SUBSTRING(c-group2,1,3, "CHAR")
    */

.


