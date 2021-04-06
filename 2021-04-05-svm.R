library(data.table)
library(ggplot2)
data.list <- readRDS("2021-04-05-read-data.rds")

with(data.list, table(data.set.vec, construct.vec))
one.set <- "FLG-Frames"
one.construct <- "UX"
is.set <- with(data.list, data.set.vec==one.set & construct.vec==one.construct)
someFeatures <- function(dlist){
  sd.vec <- sapply(dlist[["question.features"]], sd)
  keep <- is.finite(sd.vec) & 0 < sd.vec
  with(dlist, list(
    contests=contests,
    question.features=question.features[, keep]))
}
train.data.list <- someFeatures(with(data.list, list(
  contests=contests[is.set,],
  question.features=question.features)))
n.folds <- 5
unique.folds <- 1:n.folds
set.q.vec <- sub("or ", "", train.data.list$contests$or.question)
set.q.uniq <- unique(set.q.vec)
set.seed(1)
set.q.folds <- sample(rep(unique.folds, l=length(set.q.uniq)))
validation.fold <- 1
validation.questions <- set.q.uniq[set.q.folds==validation.fold]
is.validation <- set.q.vec %in% validation.questions
set.is.list <- list(
  subtrain=!is.validation,
  validation=is.validation)
set.data.list <- lapply(set.is.list, function(i){
  with(train.data.list, list(
    contests=contests[i,],
    question.features=question.features))
})

subtrain.list <- someFeatures(set.data.list[["subtrain"]])
all.terms <- paste0(names(subtrain.list[["question.features"]]), "[..]")
makeForm <- function(term.vec){
  with.dots <- c(
    "(1|..)",#random effects.
    term.vec)
  formula(paste0("~",paste(with.dots, collapse="+")))
}
bt.fit <- BradleyTerry2::BTm(
  cbind(or.score, tr.score), or.question, tr.question, makeForm(all.terms),
  data=subtrain.list, trace=TRUE)

new.terms <- all.terms
current.terms <- c()
size.acc.dt.list <- list()
for(model.size in 1:5){
  terms.acc.dt.list <- list()
  for(term.i in seq_along(new.terms)){
    term <- new.terms[[term.i]]
    cat(sprintf(
      "size=%2d %4d / %4d terms %s\n",
      length(current.terms), term.i, length(new.terms), term))
    bt.fit <- BradleyTerry2::BTm(
      cbind(or.score, tr.score), or.question, tr.question,
      makeForm(c(term, current.terms)),
      data=subtrain.list, trace=FALSE)
    for(set.name in names(set.data.list)){
      set.data <- set.data.list[[set.name]]
      set.score.vec <- predict(bt.fit, newdata=set.data)
      set.y.vec <- ifelse(set.data$contests$choice=="ORIGINAL", 1, -1)
      log.loss <- log(1+exp(-set.y.vec*set.score.vec))
      ## the log loss computation assumes binary labels (ORIGINAL and
      ## TRANSLATED) but we also have NONE so here we add the log loss
      ## for the other label so that the loss for NONE takes a minimum
      ## at a predicted score of zero.
      NONE.log.loss <- log(1+exp(set.y.vec*set.score.vec))
      NONE.log.loss[set.data$contests$choice!="NONE"] <- 0
      set.pred.vec <- ifelse(0 < set.score.vec, "ORIGINAL", "TRANSLATED")
      terms.acc.dt.list[[paste(term, set.name)]] <- data.table(
        term, set.name,
        n.correct=sum(set.pred.vec == set.data$contests$choice),
        mean.log.loss=mean(log.loss+NONE.log.loss),
        n.labels=length(set.pred.vec))
    }
  }
  (terms.acc.dt <- do.call(rbind, terms.acc.dt.list))
  (selected.terms <- terms.acc.dt[
    set.name=="subtrain"][min(mean.log.loss)==mean.log.loss])
  term.to.add <- selected.terms[1, term]
  new.terms <- new.terms[new.terms!=term.to.add]
  current.terms <- c(current.terms, term.to.add)
  size.acc.dt.list[[length(current.terms)]] <- data.table(
    n.terms=length(current.terms),
    terms.acc.dt[term==term.to.add])
}
size.acc.dt <- do.call(rbind, size.acc.dt.list)
ggplot()+
  geom_line(aes(
    n.terms, 100*n.correct/n.labels, color=set.name),
    data=size.acc.dt)
gg <- ggplot()+
  geom_line(aes(
    n.terms, mean.log.loss),
    data=size.acc.dt)+
  facet_grid(set.name ~ ., scales="free")
png("2021-04-05-svm-BT-terms-loss.png", width=3, height=3, units="in", res=200)
print(gg)
dev.off()

selected.size <- size.acc.dt[
  set.name=="validation"][which.min(mean.log.loss), n.terms]
train.terms <- current.terms[1:selected.size]
##refit to entire train set.
bt.fit <- BradleyTerry2::BTm(
  cbind(or.score, tr.score), or.question, tr.question,
  makeForm(train.terms),
  data=train.data.list, trace=FALSE)
## TODO outer CV, compute test error.

## Preparing data for rankSVMcompare package.
out.name.vec <- c(or="Xip", tr="Xi")
y.to.int <- c(ORIGINAL=1, NONE=0, TRANSLATED=-1)
pairs.list <- list(yi=y.to.int[train.data.list$contests$choice])
train.terms <- all.terms
for(version in names(out.name.vec)){
  q.vec <- train.data.list$contests[[paste0(version, ".question")]]
  out.name <- out.name.vec[[version]]
  ##From rankSVMcompare::check.pairs, the format is a list with n x p
  ##numeric matrices Xi and Xip, and yi is an integer vector of length n
  ##that takes values in c(-1,0,1):
  pairs.list[[out.name]] <- as.matrix(
    train.data.list$question.features[q.vec, sub("\\[.*", "", train.terms)])
}
sub.pairs <- function(i, plist){
  with(plist, list(
    Xi=Xi[i,],
    Xip=Xip[i,],
    yi=yi[i]))
}
set.pairs.list <- lapply(set.is.list, sub.pairs, pairs.list)

gfit <- with(set.pairs.list$subtrain, glmnet::cv.glmnet(
  Xi-Xip, ifelse(yi>0, 1, 0)))
w <- as.matrix(coef(gfit))
w[w!=0,]

svm.data.list <- lapply(set.pairs.list, rankSVMcompare::pairs2svmData)
lab.tab <- table(svm.data.list$subtrain$yi)
class.weights <- 1/lab.tab
## linear svm.
tuning.dt.list <- list()
for(cost.parameter in 10^seq(-3, 6, by=1)){
  if(!paste(cost.parameter, "subtrain") %in% names(tuning.dt.list)){
    print(cost.parameter)
    fit <- tryCatch({
      rankSVMcompare::softCompareQP(
        set.pairs.list[["subtrain"]],
        kernel=kernlab::vanilladot(),
        C=cost.parameter,
        class.weights=class.weights)
    }, error=function(e){
      print(e)
      NULL
    })
    if(!is.null(fit)){
      for(set.name in names(set.pairs.list)){
        L <- set.pairs.list[[set.name]]
        svm.data <- svm.data.list[[set.name]]
        pred.score <- with(svm.data, fit$rank(Xip) - fit$rank(Xi))
        positive.part <- function(x)ifelse(x>0, x, 0)
        hinge.loss <- function(x)positive.part(1-x)
        hinge.vec <- hinge.loss(-svm.data[["yi"]]*pred.score)
        set.pred <- with(L, fit$predict(Xi, Xip))
        set.err <- rankSVMcompare::FpFnInv(L[["yi"]], set.pred)
        tuning.dt.list[[paste(cost.parameter, set.name)]] <- data.table(
          cost.parameter, set.name, set.err, mean.hinge.loss=mean(hinge.vec))
      }
    }
  }
}
(tuning.dt <- do.call(rbind, tuning.dt.list))

ggplot()+
  geom_line(aes(
    cost.parameter, 100*error/count, color=set.name),
    data=tuning.dt)+
  scale_x_log10()

ggplot()+
  geom_line(aes(
    cost.parameter, mean.hinge.loss, color=set.name),
    data=tuning.dt)+
  scale_x_log10()

## non-linear svm with rbf/gaussian kernel.
fit <- rankSVMcompare::softCompareQP(
  set.pairs.list[["subtrain"]],
  kernel=kernlab::rbfdot(sigma=5),
  C=1)

train.score <- fit$rank(pairs.list[["Xi"]])

