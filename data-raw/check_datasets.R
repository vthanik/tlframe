load("data/adsl.rda")
load("data/adae.rda")
load("data/adtte.rda")
load("data/adcm.rda")

cat("=== ADSL ===\n")
cat("Dims:", dim(adsl), "\n")
print(table(adsl$ARM))
print(table(adsl$EOSSTT, adsl$ARM))
cat("Sex:", paste(names(table(adsl$SEX)), table(adsl$SEX), sep="=", collapse=", "), "\n")

cat("\n=== ADAE ===\n")
cat("Dims:", dim(adae), "\n")
cat("Subjects with AE:", length(unique(adae$USUBJID)), "\n")
print(table(adae$AEBODSYS))

cat("\n=== ADTTE ===\n")
cat("Dims:", dim(adtte), "\n")
print(table(adtte$PARAMCD, adtte$CNSR))

cat("\n=== ADCM ===\n")
cat("Dims:", dim(adcm), "\n")
print(sort(table(adcm$CMCAT), decreasing = TRUE))

cat("\n=== CONSISTENCY ===\n")
cat("adae USUBJID in adsl:", all(adae$USUBJID %in% adsl$USUBJID), "\n")
cat("adcm USUBJID in adsl:", all(adcm$USUBJID %in% adsl$USUBJID), "\n")
cat("adtte USUBJID in adsl:", all(adtte$USUBJID %in% adsl$USUBJID), "\n")
cat("adtte rows per subject:", nrow(adtte) / length(unique(adtte$USUBJID)), "(should be 2)\n")
