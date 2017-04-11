library(APM)
memory.limit(8000)

rc <- sharing_ips_with_employer_score(exclude_cellular = TRUE)
OUTPUT <- list(RETN_CD = rc)
