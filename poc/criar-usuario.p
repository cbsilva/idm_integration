{METHOD/dbotterr.i}

DEFINE VARIABLE h-bofn017 AS HANDLE      NO-UNDO.

//DEFINE INPUT PARAM p-usuario AS CHARACTER    NO-UNDO.
//DEFINE INPUT PARAM p-nome    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE p-usuario AS CHARACTER INITIAL "testeint"  NO-UNDO.

DEFINE TEMP-TABLE tt-usuar_mestre NO-UNDO LIKE usuar_mestre
    FIELD r-Rowid AS ROWID.
    
DEFINE VARIABLE i-id AS INTEGER     NO-UNDO.

FOR LAST usuar_mestre BREAK BY usuar_mestre.idi_dtsul.
    ASSIGN i-id = usuar_mestre.idi_dtsul + 1.
END.
        
EMPTY TEMP-TABLE tt-usuar_mestre.
FIND FIRST usuar_mestre NO-LOCK
     WHERE usuar_mestre.cod_usuario = p-usuario NO-ERROR.

CREATE tt-usuar_mestre.
BUFFER-COPY usuar_mestre TO tt-usuar_mestre.
ASSIGN tt-usuar_mestre.cod_usuario          = p-usuario
       tt-usuar_mestre.nom_usuario          = "teste usuario"
       tt-usuar_mestre.dat_inic_valid       = 01/01/0001
       tt-usuar_mestre.dat_fim_valid        = 12/31/9999
       tt-usuar_mestre.dat_valid_senha      = 10/08/2018
       tt-usuar_mestre.num_dias_valid_senha = 90
       tt-usuar_mestre.idi_dtsul            = i-id.

IF NOT VALID-HANDLE(h-bofn017) THEN
    RUN fnbo/bofn017.p PERSISTENT SET h-bofn017.

RUN emptyRowErrors IN h-bofn017.
RUN openQueryStatic IN h-bofn017 (INPUT "Main":U) NO-ERROR.
RUN setRecord IN h-bofn017(INPUT TABLE tt-usuar_mestre).
RUN createRecord IN h-bofn017.
RUN getRowErrors IN h-bofn017(OUTPUT TABLE RowErrors).

FOR EACH RowErrors:
    MESSAGE RowErrors.errorDescription
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

IF NOT CAN-FIND(FIRST RowErrors NO-LOCK) THEN DO:

    CREATE segur_empres_usuar.
    ASSIGN segur_empres_usuar.cod_usuario = p-usuario
           segur_empres_usuar.cod_empresa = "001".

/*     CREATE usuar_grp_usuar.                          */
/*     ASSIGN usuar_grp_usuar.cod_grp_usuar = p-usuario */
/*            usuar_grp_usuar.cod_usuario   = "*".      */

END.

