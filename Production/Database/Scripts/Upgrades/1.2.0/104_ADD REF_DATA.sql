/*==============================================================*/
/* REF Schema: Reference Table Data                             */
/*==============================================================*/

/*==============================================================*/
/* Table: lkup_tabl                                             */
/*==============================================================*/

insert into LKUP_TABL (LKUP_TABL_ID, LKUP_TABL_NAME, LKUP_TABL_DESC, CRT_USER_ID, CRT_TMSTMP) values (18, 'IP Connection Type Code', 'The codes for the type of connection of an IP address.', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_TABL (LKUP_TABL_ID, LKUP_TABL_NAME, LKUP_TABL_DESC, CRT_USER_ID, CRT_TMSTMP) values (19, 'IP Subscriber Type Code', 'The codes for the type of subscriber of an IP address.', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');

/*==============================================================*/
/* Table: lkup_cd                                               */
/*==============================================================*/

insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (18, 'FIX', 'Fixed Line', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (18, 'CEL', 'Cellular', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (18, 'WIR', 'Wireless', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (18, 'SAT', 'Satellite', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'RES', 'Residential', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'BUS', 'Business', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'GOV', 'Government', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'MIL', 'Military', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'EDU', 'Education', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'ISP', 'ISP', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'LIB', 'Library', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
insert into LKUP_CD (LKUP_TABL_ID, LKUP_CD, LKUP_VAL, CRT_USER_ID, CRT_TMSTMP) values (19, 'ICA', 'Internet Cafe', 'bmorrison@ntelx.com', '2016-08-26 16:11:10');
