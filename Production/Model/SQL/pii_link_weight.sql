/* This script will create the links scores for each claimant based on phone, email, and passwords
The query will use subqueries on the on the claimant attribute and aggregated claimant tables for each piece of pii that will be used. 
These sub-queries will  be joined.
*/



INSERT INTO pii_link_scores (phone, phone_count, phone_array, email, email_count, email_array, passwd, passwd_count, passwd_array,
			     em_ph_array, em_ph_count, em_pw_array, em_pw_count, ph_pw_array, ph_pw_count, em_ph_pw_array, em_ph_pw_count,
			     em_link_weight, ph_link_weight, pw_link_weight, em_ph_link_weight, ph_pw_link_weight, em_pw_link_weight, em_ph_pw_link_weight) (
SELECT 
	COALESCE(ph_em.claimt_id,passwd.claimt_id) AS claimt_id,
	--links based on single shared piece of pii	
	phone,
	phone_count,
	phone_array,
	
	email,
	email_count,
	email_array,
	
	passwd,
	passwd_count,
	passwd_array,
	
	--links based on multiple shared pii
	uf_array_intersect(email_array, phone_array) 		  	AS em_ph_array ,
	array_length(uf_array_intersect(email_array, phone_array),1) 	AS em_ph_count,
	
	uf_array_intersect(email_array, passwd_array) 			AS em_pw_array,
	array_length(uf_array_intersect(email_array, passwd_array),1)   	AS em_pw_count,

	uf_array_intersect(phone_array, passwd_array) 			AS ph_pw_array,
	array_length(uf_array_intersect(phone_array, passwd_array),1) 	AS ph_pw_count,

	uf_array_intersect(uf_array_intersect(email_array, phone_array),passwd_array) 		  	AS em_ph_pw_array,
	array_length(uf_array_intersect(uf_array_intersect(email_array, phone_array),passwd_array),1) 	AS em_ph_pw_count,

	--calculate scores
	CASE WHEN email_count > 1 THEN LOG(em.count) - LOG(email_count) ELSE 0 END  AS em_link_weight,
	CASE WHEN phone_count > 1 THEN LOG(ph.count) - LOG(phone_count) ELSE 0 END  AS ph_link_weight,
	CASE WHEN passwd_count > 1 THEN LOG(pw.count) - LOG(passwd_count) ELSE 0 END AS pw_link_weight,

	CASE WHEN array_length(uf_array_intersect(email_array, phone_array),1) > 1 THEN (LOG(em.count) + LOG(ph.count) - LOG(email_count) - LOG(phone_count)) ELSE 0 END 		AS em_ph_link_weight,
	CASE WHEN array_length(uf_array_intersect(passwd_array, phone_array),1) > 1 THEN (LOG(pw.count) + LOG(ph.count) - LOG(passwd_count) - LOG(phone_count)) ELSE 0 END 	AS ph_pw_link_weight,
	CASE WHEN array_length(uf_array_intersect(email_array, passwd_array),1) > 1 THEN (LOG(em.count) + LOG(pw.count) - LOG(email_count) - LOG(passwd_count)) ELSE 0 END 	AS em_pw_link_weight,

	CASE WHEN array_length(uf_array_intersect(uf_array_intersect(email_array, phone_array),passwd_array),1)>1 THEN (LOG(em.count)+LOG(pw.count)+LOG(ph.count)-LOG(email_count)-LOG(phone_count)-LOG(passwd_count)) ELSE 0 END 	AS em_ph_pw_link_weight
	
FROM
	-- Sub-queries for phone, email, and pw totals, these are referenced as em.count, ph.count, pw.count
	(SELECT COUNT(*) FROM claimt_attr WHERE claimt_attr_type_cd = 'PHON') as ph,
	(SELECT COUNT(*) FROM claimt_attr WHERE claimt_attr_type_cd = 'EMAIL') as em,
	(SELECT COUNT(*) FROM claimt_attr WHERE claimt_attr_sub_type_cd = 'PASSWD') as pw,
	
	--Phone & Email Subquery
	(SELECT 
	COALESCE(phone.claimt_id,email.claimt_id) as claimt_id
	,phone
	,phone_count
	,phone_array
	,email
	,email_count
	,email_array
	FROM

	(SELECT  a.claimt_id
		,a.claimt_attr_val AS phone
		,b.claimt_cnt AS phone_count
		,b.claimt_id_array phone_array	
	FROM    claimt_attr			  AS a
		LEFT JOIN aggr_shared_claimt_attr AS b ON 
			a.claimt_attr_type_cd = b.claimt_attr_type_cd AND
			a.claimt_attr_val = b.claimt_attr_val
	WHERE a.claimt_attr_type_cd = 'PHON' AND claimt_cnt >1) AS phone

	FULL OUTER JOIN
	
	(SELECT  a.claimt_id
		,a.claimt_attr_val AS email
		,b.claimt_cnt AS email_count
		,b.claimt_id_array email_array
		

	FROM    claimt_attr			  AS a
		LEFT JOIN aggr_shared_claimt_attr AS b ON 
			a.claimt_attr_type_cd = b.claimt_attr_type_cd AND
			a.claimt_attr_val = b.claimt_attr_val
	WHERE a.claimt_attr_type_cd = 'EMAIL' AND claimt_cnt >1) AS email ON
	phone.claimt_id = email.claimt_id) AS ph_em
	
	
	FULL OUTER JOIN
	--PASSWD Sub-query
	(SELECT  a.claimt_id
		,a.claimt_attr_val AS passwd
		,b.claimt_cnt AS passwd_count
		,b.claimt_id_array AS passwd_array
		

	FROM    claimt_attr			  AS a
		LEFT JOIN aggr_shared_claimt_attr AS b ON 
			a.claimt_attr_sub_type_cd = b.claimt_attr_type_cd AND
			a.claimt_attr_val = b.claimt_attr_val
	WHERE b.claimt_attr_type_cd = 'PASSWD' AND claimt_cnt >1) AS passwd ON
	passwd.claimt_id = ph_em.claimt_id
);

SELECT *
FROM claimt_attr WHERE claimt_id= '747554';