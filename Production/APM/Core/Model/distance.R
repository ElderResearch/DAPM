library(APM)
memory.limit(8000)

rc <- distance_score(exclude_cellular = TRUE)
OUTPUT <- list(RETN_CD = rc)
