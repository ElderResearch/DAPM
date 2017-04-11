library(APM)
memory.limit(8000)

rc <- ip_location_score(exclude_cellular = TRUE)
OUTPUT <- list(RETN_CD = rc)
