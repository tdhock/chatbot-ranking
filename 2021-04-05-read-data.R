library(data.table)
user_pref <- data.table::fread("user_preference.csv")

## each line is one observation: a choice from one participant for a
## particular sentence; participants could choose original/modified
## (the NONE options are removed from this dataset, let me know if you
## want them too); choices are stored in the column choice
table(user_pref$choice)

## first column is what datasets were compared (FLG - FLG_mod (mimics
## DailyDialog) or FLG - FLG_mod2 (mimics Frames));
table(user_pref$data)

## the columns G1, G2, G3 is the origin of the answer (who was the
## tourist assistant who produced that response in the original data)
table(user_pref$G_1)
table(user_pref$G_2)
table(user_pref$G_3)

## the columns Pxxx are the participants who evaluated each sentence;
## it's 0 if that participant evaluated that sentence and 0
## otherwise. NOTE: P1 in FLG-DailyDialog is NOT the same person as P1
## in FLG-Frames, you may want to separate them (?)
table(user_pref[["PID_1"]])

## the column construct is whether participants were evaluating
## appropriateness, credibility or overall user experience when made
## that choice;
table(user_pref[["construct"]])

## the other columns should be linguistic features in the sentence;
## there are some NAs, which means that linguistic feature was not
## evaluated for that dataset. The numeric values represent the
## difference between the counts for that feature in FLG and
## FLG_mod/_mod2.

grep("choice|data|social|PID|construct|G", names(user_pref), value=TRUE, invert=TRUE)

## And there is one more column that has the "question". This will
## tell you which sentence corresponds to each observation. This may
## be useful for the next two datasets.
table(user_pref[["question"]])

table(user_pref[["construct"]])

both_counts <- data.table(prefix=c("DD", "frames"))[, {
  counts.csv <- paste0(prefix, "_counts.csv")
  data.table::fread(counts.csv, colClasses=list(
    numeric=c(
      "sub_conj_caus", "wh_rel_pipe", "passive_by",
      "jj_epist_pred", "nn_stance_new", "V88",
      "th_vb_likely", "to_nn_stance_all", "pv_aspect")))
}, by=prefix]
file.pattern <- list(
  version="or|tr",
  "_",
  nc::field("r", "", ".*?"),
  "_",
  nc::quantifier("guide_", "?"),
  guide=".*?",
  "_",
  nc::quantifier("conversation_", "?"),
  conversation=".*",
  "[.]txt")
both_counts_aug <- nc::capture_first_df(both_counts, file=file.pattern)
data.frame(both_counts_aug[, .(file, version, r, guide, conversation)])
both_counts_aug[, question := toupper(
  paste0("R", r, "_", guide, "_", conversation))]

feature.names <- grep(
  "question|choice|data|social|PID|construct|G|V",
  names(user_pref), value=TRUE, invert=TRUE)

both_counts_aug[ user_pref$question[1] , on="question"]

data2prefix <- c(
  "FLG-DailyDialog"="DD",
  "FLG-Frames"="frames")
user_pref[, prefix := data2prefix[data] ]

both_counts_aug[ user_pref[1], feature.names, on=.(question, prefix), with=FALSE]

## preparing data for BT package.
data(CEMS, package="BradleyTerry2")
head(CEMS[["preferences"]])
or.scores <- c(
  ORIGINAL=1,
  TRANSLATED=0,
  NONE=0.5)
uniq.questions.df <- data.frame(
  scale(both_counts_aug[, feature.names, with=FALSE]),
  row.names=both_counts_aug[, paste(version, prefix, question)])
contests.df <- user_pref[, data.frame(
  choice,
  or.score=or.scores[choice])]
contests.df[["tr.score"]] <- 1-contests.df[["or.score"]]
for(version in c("or", "tr")){
  contests.df[[paste0(version, ".question")]] <- factor(
    user_pref[, paste(version, prefix, question)],
    rownames(uniq.questions.df))
}

saveRDS(list(
  question.features=uniq.questions.df,
  contests=contests.df,
  construct.vec=user_pref[["construct"]],
  PID.mat=as.matrix(user_pref[, paste0("PID_", 1:178)]),
  G.mat=as.matrix(user_pref[, paste0("G_", 1:3)]),
  y.vec=user_pref[["choice"]],
  data.set.vec=user_pref[["data"]]
), "2021-04-05-read-data.rds")

