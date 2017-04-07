/*

Email, Phone, and Password Links Description:
1. Calculates link weights for claimants who share personally identifiable information (PII)
2. Link weights are calculated as the log of the inverse of the probability of the two accounts sharing the PII
3. One temporary table is created for each type of PII used for linking claimants, which are then joined to create the full edgelist

Assumptions:
For calculating the probabilities we assume P(sharing PII) = (frequency of PII)/(Total claimants with valid PII)
For example two claimants that share the phone number 9999999 the link weight would be 
log(1/P(sharing phone number 9999999)) = log(# of claimants with valid phone numbers) - log(# of claimants with phone 9999999)

*/
-- make Temp Phone table
DROP TABLE IF EXISTS tmp.ph_edges;
CREATE TABLE tmp.ph_edges AS
SELECT  a.claimt_id AS from_id
	,unnest(b.claimt_id_array) AS to_id
	,a.claimt_attr_val AS phone
	,COALESCE(LOG(ph.count) - LOG(b.claimt_cnt),0) AS ph_wgt
		
FROM    (SELECT COUNT(1) FROM nrd.claimt_attr WHERE claimt_attr_type_cd = 'PHON') as ph,
	nrd.claimt_attr			  AS a
	LEFT JOIN nrd.aggr_shared_claimt_attr AS b ON 
		a.claimt_attr_type_cd = b.claimt_attr_type_cd AND
		a.claimt_attr_val = b.claimt_attr_val
			
WHERE b.claimt_attr_type_cd = 'PHON' AND claimt_cnt >1;

-- make Temp Email table
DROP TABLE IF EXISTS tmp.em_edges;
CREATE TABLE tmp.em_edges AS
SELECT  a.claimt_id AS from_id
	,unnest(b.claimt_id_array) AS to_id
	,a.claimt_attr_val AS email
	,COALESCE(LOG(em.count) - LOG(b.claimt_cnt),0) AS em_wgt
		

FROM    (SELECT COUNT(1) FROM nrd.claimt_attr WHERE claimt_attr_type_cd = 'EMAIL') as em,
	nrd.claimt_attr			  AS a
	LEFT JOIN nrd.aggr_shared_claimt_attr AS b ON 
		a.claimt_attr_type_cd = b.claimt_attr_type_cd AND
		a.claimt_attr_val = b.claimt_attr_val
			
WHERE b.claimt_attr_type_cd = 'EMAIL' AND claimt_cnt > 1;


-- make Temp PW table
DROP TABLE IF EXISTS tmp.pw_edges;
CREATE TABLE tmp.pw_edges AS
SELECT  a.claimt_id AS from_id
	,unnest(b.claimt_id_array) AS to_id
	,a.claimt_attr_val AS passwd
	,COALESCE(LOG(pw.count) - LOG(b.claimt_cnt),0) AS pw_wgt
		

FROM    (SELECT COUNT(1) FROM nrd.claimt_attr WHERE claimt_attr_type_cd = 'PASSWD') as pw,
	nrd.claimt_attr			  AS a
	LEFT JOIN nrd.aggr_shared_claimt_attr AS b ON 
		a.claimt_attr_type_cd = b.claimt_attr_type_cd AND
		a.claimt_attr_val = b.claimt_attr_val
			
WHERE b.claimt_attr_type_cd = 'PASSWD' AND claimt_cnt >1;

--BRING EM ALL TOGETHER
-- First bring together just phone and email edge lists
DROP TABLE IF EXISTS tmp.em_ph;
CREATE TABLE tmp.em_ph AS
SELECT
	COALESCE(em.from_id, ph.from_id) AS from_id,
	COALESCE(em.to_id, ph.to_id) 	 AS to_id,
	phone,
	COALESCE(ph_wgt,0) AS ph_wgt,
	email,
	COALESCE(em_wgt,0) AS em_wgt
FROM
	(SELECT * FROM tmp.em_edges WHERE from_id < to_id) em	
	FULL OUTER JOIN 
	(SELECT * FROM tmp.ph_edges WHERE from_id < to_id) ph 
	ON 
	em.from_id = ph.from_id AND em.to_id = ph.to_id;
	
-- merge email and phone edgelist with password edgelist to make complete edgelist
DROP TABLE IF EXISTS tmp.aggr_claimt_shared_attr_edges;
CREATE TABLE tmp.aggr_claimt_shared_attr_edges AS
SELECT 
	COALESCE(em_ph.from_id, pw.from_id) AS from_id,
	COALESCE(em_ph.to_id, pw.to_id)     AS to_id,
	phone,
	COALESCE(ph_wgt,0) AS ph_wgt,
	email,
	COALESCE(em_wgt,0) AS em_wgt,
	passwd,
	COALESCE(pw_wgt,0) AS pw_wgt
FROM
	(SELECT * FROM tmp.pw_edges WHERE from_id < to_id) pw
	FULL OUTER JOIN
	tmp.em_ph
	ON
	em_ph.from_id = pw.from_id AND em_ph.to_id = pw.to_id;


/*IP Link Description:
1. Calculates all unique pairs of people who have shared the same ip from the entire nrd.aggr_claimt_sessn table.
2. Subsets that group down to only those people who have shared an IP within a week of one another.
4. Calculates the inverse log probability of sharing that specific IP within that one week period.
5. Keeps the maximum resulting score for each unique pair of individuals (as individuals may have shared IP addresses multiple
   times over their claiming history).
   
Assumptions and Future Work:
1. The entire history is used to create links that will occur in the graph structure (even relatively old historical data). This
   was chosen because (1) the computation time for this script is relatively fast (~45 seconds on ~30k records; in other states
   the number of records may be much longer, and a subset of the data may need to be fed into this script) and (2) fraud often
   isn't caught until months after it occurred. Therefore, maintaining these links for months into the past is important to
   preserve connections to fraudulent actors (from which risk will be propagated)
2. At this point, the maximum of all "weights" calculated between two individuals is used as their final edge weight in the graph.
   The downside to this methodology is that it does not increase the weight between two individuals based on the number of times
   they've shared ip addresses. For example, two individuals may have shared IP addresses for 10 weeks; the maximum of their inverse
   log probability will be used and information that they shared 10x will be lost. This should be revisited and handled in a
   more robust manner in the next iteration of this script.
3. Because IP addresses change, we only consider sharing IP addresses within the same week to be indicative of a connection.
   In other words, two claimants must have been claiming unemployment insurance at the same time from the same IP address in
   order to be connected with this methodology.
*/

--Get all unique pairwise comparisons for all of the data
--Question: Do we want to allow this to do incremental updates? Would be tougher...
DROP TABLE IF EXISTS tmp.pairwise_subset;
CREATE TABLE tmp.pairwise_subset AS
SELECT t1.claimt_id AS from,
       t2.claimt_id AS to,
       t1.claimt_sessn_tmstmp AS from_ts,
       t2.claimt_sessn_tmstmp AS to_ts,
       t1.orig_ip_addr AS ip_address
FROM nrd.aggr_claimt_sessn t1
JOIN nrd.aggr_claimt_sessn t2
ON t1.orig_ip_addr = t2.orig_ip_addr
   AND
   t1.claimt_sessn_tmstmp > t2.claimt_sessn_tmstmp
   AND
   t1.claimt_id <> t2.claimt_id
WHERE (t1.claimt_sessn_tmstmp - t2.claimt_sessn_tmstmp) < '7 00:00:00';

CREATE INDEX pairwise_subset_idx
  ON tmp.pairwise_subset
  USING btree
  ("from", from_ts, ip_address);

--Calculate the number of people within a week of the "from" that shared that same IP address
DROP TABLE IF EXISTS tmp.pairwise_w_counts;
CREATE TABLE tmp.pairwise_w_counts AS
SELECT t1.*,
       count_t.share_count
FROM tmp.pairwise_subset AS t1
JOIN
	(SELECT t1.from,
	       t1.from_ts,
	       t1.ip_address,
	       COUNT(DISTINCT t1.to) AS share_count
	FROM tmp.pairwise_subset t1
	GROUP BY t1.from,
		 t1.from_ts,
		 t1.ip_address) AS count_t
ON t1.from = count_t.from
AND t1.from_ts = count_t.from_ts
AND t1.ip_address = count_t.ip_address;

DROP TABLE IF EXISTS tmp.pairwise_subset_distinct_from;
CREATE TABLE tmp.pairwise_subset_distinct_from AS
SELECT claimt_id, claimt_sessn_tmstmp from nrd.aggr_claimt_sessn
WHERE claimt_id in (SELECT DISTINCT "from" FROM tmp.pairwise_subset);
ALTER TABLE tmp.pairwise_subset_distinct_from
  ADD CONSTRAINT pairwise_subset_distinct_from_pk PRIMARY KEY(claimt_id, claimt_sessn_tmstmp);
CREATE INDEX pairwise_subset_distinct_from_idx1 on tmp.pairwise_subset_distinct_from (claimt_id);
CREATE INDEX pairwise_subset_distinct_from_idx2 on tmp.pairwise_subset_distinct_from (claimt_sessn_tmstmp);

--Calculate the number of claimants in the week prior to the timestamp for the claimants we're interested in
DROP TABLE IF EXISTS tmp.pairwise_w_tot_counts;
CREATE TABLE tmp.pairwise_w_tot_counts AS
SELECT 
	pairwise.*,
	tot_counts.tot_count,
	LOG(tot_counts.tot_count / pairwise.share_count) as logged_inv_prob
FROM tmp.pairwise_w_counts AS pairwise
JOIN 
	--Get the number of claimants that came into the system via IP in the preceeding weeks
	(with perd as (
 select count(distinct s2.claimt_id) AS tot_count, s1.claimt_sessn_dt
 from (
  select distinct
   claimt_sessn_tmstmp::date as claimt_sessn_dt,
   (claimt_sessn_tmstmp::date) + interval '1 day' as claimt_sessn_dt_end,
   (claimt_sessn_tmstmp::date) - interval '6 days' as claimt_sessn_dt_start
  from nrd.aggr_claimt_sessn
 ) as s1
 join nrd.aggr_claimt_sessn s2
  on s1.claimt_sessn_dt_start <= s2.claimt_sessn_tmstmp
  and s1.claimt_sessn_dt_end > s2.claimt_sessn_tmstmp
 group by s1.claimt_sessn_dt_start, s1.claimt_sessn_dt
)
SELECT t1.claimt_id AS from_temp,
		t1.claimt_sessn_tmstmp AS from_ts,
		perd.tot_count - 1 as tot_count
	FROM tmp.pairwise_subset_distinct_from t1
	JOIN perd
	ON t1.claimt_sessn_tmstmp::date = perd.claimt_sessn_dt) AS tot_counts
ON
	pairwise.from = tot_counts.from_temp
	AND
	pairwise.from_ts = tot_counts.from_ts;

/*Roll-up such that the from/to pair is unique. Taking the max of the final score and then obtaining the
IP address that connected the two claimants.*/
DROP TABLE IF EXISTS tmp.final_ip_edges;
CREATE TABLE tmp.final_ip_edges AS
SELECT DISTINCT t1.from as from_id,
		t1.to as to_id,
		t1.weight as ip_wgt,
		t2.ip_address as ip
FROM
	(SELECT t1.from,
	       t1.to,
	       MAX(logged_inv_prob) as weight
	FROM tmp.pairwise_w_tot_counts t1
	GROUP BY t1.from,
		 t1.to) t1
JOIN pairwise_w_tot_counts t2
ON t1.from = t2.from
   AND
   t1.to = t2.to
   AND
   t1.weight = t2.logged_inv_prob;

--Join the IP table onto the table that already contains phone, email, and password weights
DROP TABLE IF EXISTS tmp.aggr_claimt_shared_attr_edges_final;
CREATE TABLE tmp.aggr_claimt_shared_attr_edges_final AS
SELECT 
	COALESCE(other_edges.from_id, ip_table.from_id) AS from_id,
	COALESCE(other_edges.to_id, ip_table.to_id)     AS to_id,
	other_edges.phone,
	COALESCE(other_edges.ph_wgt,0) AS ph_wgt,
	other_edges.email,
	COALESCE(other_edges.em_wgt,0) AS em_wgt,
	other_edges.passwd,
	COALESCE(other_edges.pw_wgt,0) AS pw_wgt,
	ip_table.ip,
	COALESCE(ip_table.ip_wgt,0) AS ip_wgt
FROM	tmp.final_ip_edges AS ip_table
	FULL OUTER JOIN
	tmp.aggr_claimt_shared_attr_edges other_edges
	ON
	other_edges.from_id = ip_table.from_id AND other_edges.to_id = ip_table.to_id;

--Ensure that there is only one row per pair of claimt_ids (summing scores)
DROP TABLE IF EXISTS tmp.edge_list_final;
CREATE TABLE tmp.edge_list_final AS
SELECT 
	from_id,
	to_id,
	ARRAY_AGG(phone) AS phone,
	SUM(ph_wgt) AS ph_wgt,
	ARRAY_AGG(email) AS email,
	SUM(em_wgt) AS em_wgt,
	ARRAY_AGG(passwd) AS passwd,
	SUM(pw_wgt) AS pw_wgt,
	ARRAY_AGG(ip) AS ip,
	SUM(ip_wgt) AS ip_wgt
FROM tmp.aggr_claimt_shared_attr_edges_final
GROUP BY 
	from_id,
	to_id;

--Drop records from previous version of nrd.aggr_claimt_share
DELETE FROM nrd.aggr_claimt_shared_attr_edges;

--Insert the new data into the table
INSERT INTO nrd.aggr_claimt_shared_attr_edges (from_claimt_id, to_claimt_id, email_addr_array, email_link_wgt_val,
					       phon_num_array, phon_link_wgt_val, passwd_val_array, passwd_link_wgt_val,
					       orig_ip_addr_array, orig_ip_link_wgt_val, crt_tmstmp)
SELECT	from_id,
	to_id,
	email,
	em_wgt,
	phone,
	ph_wgt,
	passwd,
	pw_wgt,
	ip,
	ip_wgt,
	CURRENT_TIMESTAMP
FROM tmp.edge_list_final;
	
--Drop temporary tables
DROP TABLE IF EXISTS tmp.ph_edges,
		     tmp.em_edges,
		     tmp.pw_edges,
		     tmp.em_ph,
		     tmp.aggr_claimt_shared_attr_edges,
		     tmp.pairwise_subset,
		     tmp.pairwise_w_counts,
		     tmp.pairwise_subset_distinct_from,
		     tmp.pairwise_w_tot_counts,
		     tmp.final_ip_edges,
		     tmp.aggr_claimt_shared_attr_edges_final,
		     tmp.edge_list_final;