counts_diff <- data.table::fread("../chatbots-register/Text modification/R package/surveystats/data-raw/counts_diff.csv")
ux <- data.table::fread("../chatbots-register/Text modification/R package/surveystats/data-raw/userX_per_participant.csv")

P.col.pattern <- list(
  "P",
  I="I+",
  "_S",
  S="[0-3]",
  "_",
  file=".*")
all.col.info <- nc::capture_first_vec(
  names(ux),
  P.col.pattern,
  nomatch.error=FALSE)
is.P.col <- all.col.info[, !is.na(I)]
P.col.info <- all.col.info[is.P.col]
mean(P.col.info$file %in% counts_diff$file)
mean(counts_diff$file %in% P.col.info$file)

ux.P.cols <- ux[, is.P.col, with=FALSE]
ux.tall <- nc::capture_melt_single(
  ux.P.cols,
  P.col.pattern)[value != ""]
ux.tall[, .(count=.N), by=value]
