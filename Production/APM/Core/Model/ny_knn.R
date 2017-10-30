library(APM)
memory.limit(8000)

rc <- ny_statistical_model(model = "KNN_KNN", effect = 0.75)
OUTPUT <- list(RETN_CD = rc)
