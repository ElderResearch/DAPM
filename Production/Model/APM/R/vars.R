# Variables that need to be accessible across the project

# Random seed for reproducible training and testing
random_seed <- 1587655348

# Variables to impute in the case of missing data.
# This should be modified per state.
# (1) NA values and (2) missing keys imply no imputation for a variable.
# See utils.R::impute_model_vars() for implementation.
kVarsToImpute <- list(
  base_paymt_perds_cnt = NA,
  base_perd_paymt_amt = NA,
  max_paymt_amt = NA,
  st_base_perd_paymt_amt = NA,
  st_max_paymt_amt = NA,
  claimt_age = NA,
  cert_perd_end_qtr_num = NA,
  claim_year_seq = NA,
  claimt_id = NA,
  cuml_adjstd_incm_amt = NA,
  cuml_gt_max_incm_cert_perd_cnt = NA,
  cuml_lt_min_incm_cert_perd_cnt = NA,
  cuml_paymt_amt = NA,
  cuml_paymt_perds_cnt = NA,
  cuml_reptd_incm_amt = NA,
  cuml_zero_incm_cert_perd_cnt = NA,
  incm_to_base_paymt_ratio = NA,
  last_emplr_days_emplyd_cnt = NA,
  last_emplr_end_qtr_num = NA,
  paymt_to_base_paymt_perds_ratio = NA,
  paymt_to_base_paymt_ratio = NA,
  perds_to_cert_start_cnt = NA,
  perds_to_claim_start_cnt = NA,
  progm_cd_cnt = NA,
  tot_reglr_adjstd_incm_amt = NA,
  tot_reglr_paymt_amt = NA,
  tot_reglr_reptd_incm_amt = NA
)
