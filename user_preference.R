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

## social_agent_orientation is a value for likert. Some people did not
## answer, it's marked as 0. There are columns with FALSE/TRUE that
## tells us whether the question was answered or not, so then these
## columns are TRUE when the question was answered and FALSE otherwise
## (0 for the number column).
table(user_pref$social_orientation_1)
table(user_pref$social_or_1_na)
user_pref[, table(social_orientation_1, social_or_1_na)]
table(user_pref$social_orientation_2)

pid.tall <- nc::capture_melt_single(
  user_pref,
  nc::field("PID", "_", "[0-9]+", as.integer))

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
