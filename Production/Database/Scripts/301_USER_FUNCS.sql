/*==============================================================*/
/* NRD Schema: User Functions                                   */
/*==============================================================*/

SET SEARCH_PATH = nrd;

/*==============================================================*/
/* Function: uf_array_intersect()                               */
/*==============================================================*/

CREATE OR REPLACE FUNCTION uf_array_intersect(
    anyarray,
    anyarray)
RETURNS anyarray AS
$BODY$
    SELECT ARRAY(
        SELECT UNNEST($1)
        INTERSECT
        SELECT UNNEST($2)
    );
$BODY$
    LANGUAGE sql VOLATILE
    COST 100;

/*==============================================================*/
/* Function: uf_gompertz()                                      */
/*==============================================================*/

CREATE OR REPLACE FUNCTION uf_gompertz(
    v numeric,
    a1 numeric,
    a2 numeric,
    a3 numeric)
RETURNS numeric AS
$BODY$
    SELECT TRUNC (a1 * EXP (TRUNC (LEAST (GREATEST (-a2 * EXP (TRUNC (LEAST (GREATEST (-a3 * v , -100), 100), 10)), -100), 100), 10)), 2) ;
$BODY$
    LANGUAGE sql VOLATILE
    COST 100;
 