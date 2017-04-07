ERI APM SQL Files
=================

These SQL files are **for reference only**!

The SQL-based models are wrapped as R functions in `../APM/R/sql_scoring.R`. These models are called via the following functions:  

- `changing_ip_score()`
- `ip_location_score()`
- `sharing_ips_with_employer_score()`
- `new_hire_score()`
- `qwr_score()`
- `sharing_ip_score()`

This directory also seems to contain early versions PII SQL (`contagion_pii_edges_prep.sql` and `pii_link_weight.sql`) and the definition of the NRD stored procedure for a Gompertz function (`gompertz_func.sql`).
