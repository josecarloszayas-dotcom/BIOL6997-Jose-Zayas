df <- read.csv("Expression 25001 STAGEs.csv", row.names = 1, check.names = FALSE)


# ---- Metadata file ----
metadata <- data.frame(
  SampleID  = rownames(df),
  Condition = df$disease.state,
  stringsAsFactors = FALSE
)
write.csv(metadata, "metadata.csv", row.names = FALSE)

# ---- Expression file (transposed) ----
expr <- df[, -1]                    # drop disease.state column
expr_t <- t(expr)                   # transpose: genes as rows, samples as cols
expr_t <- as.data.frame(expr_t, check.names = FALSE)
expr_t <- cbind(Gene = rownames(expr_t), expr_t)
rownames(expr_t) <- NULL

write.csv(expr_t, "expression.csv", row.names = FALSE)

cat("Wrote metadata.csv and expression.csv\n")
cat("Expression dims:", nrow(expr_t), "genes x", ncol(expr_t) - 1, "samples\n")