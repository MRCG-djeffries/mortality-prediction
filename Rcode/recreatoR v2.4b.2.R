# Code to re-do a predictive model based on
  # algorithm
  # feature combination
  # class weights
  # hyper-parameters
  # seed
# Specify hyperparameters and model specifications in lines 342-...

# Load libraries
library(caret)
library(pROC)   # For ROC
library(DMwR)   # For SMOTE
library(e1071)  # For SVM

###########################
# Load modified functions #
########### RF ############
Ranger <- getModelInfo("ranger")[[1]]
#                 ## Fit the model  with weights
Ranger$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  if((!is.data.frame(x))||dplyr::is.tbl(x)) x <- as.data.frame(x)
  x$.outcome <- y
  dots <- list(...)
  wts <- ifelse(x$.outcome=="X0", dots$mywts[1], 1) 
  if(!is.null(wts)) {
    cat(".") 
    out <- ranger::ranger(dependent.variable.name = ".outcome",
                          data = x,
                          mtry = min(param$mtry, ncol(x)),
                          min.node.size = param$min.node.size,
                          splitrule = as.character(param$splitrule),
                          write.forest = TRUE,
                          probability = classProbs,
                          case.weights = wts)#,
    #...)
  } else {
    cat(";") 
    out <- ranger::ranger(dependent.variable.name = ".outcome",
                          data = x,
                          mtry = min(param$mtry, ncol(x)),
                          min.node.size = param$min.node.size,
                          splitrule = as.character(param$splitrule),
                          write.forest = TRUE,
                          probability = classProbs,
                          ...)
  }
  ## in case the resampling method is "oob"
  if(!last) out$y <- y
  out
}

###########################
# Load modified functions #
########### SVM ###########
RadialSvm <- getModelInfo("svmRadial")[[2]]
## Fit the model independent of the threshold parameter
RadialSvm$fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
  dots <- list(...)
  wts <- dots$mywts
  #cat("Weights: ", wts, "\n") 
  cat(".")
  if(length(levels(y)) != 2)
    stop("This works only for 2-class problems")
  
  if(any(names(list(...)) == "prob.model") | is.numeric(y)) {
    out <- e1071::svm(x = as.matrix(x), y = y,
                      type = 'C-classification',
                      kernel = "radial",
                      class.weights = wts,
                      gamma = param$sigma,
                      cost = param$C, ...)
  } else {
    out <- e1071::svm(x = as.matrix(x), y = y,
                      type = 'C-classification',
                      kernel = "radial",
                      class.weights = wts,
                      gamma = param$sigma,
                      cost = param$C,
                      probability = TRUE,
                      ...)
  }
  out            
}

## get the predicted class
RadialSvm$predict <- function(modelFit, newdata, submodels = NULL) {
  svmPred <- function(obj, x) {
    pred <- e1071:::predict.svm(obj, x)
    pred
  }
  out <- try(svmPred(modelFit, newdata), silent = TRUE)
  if(is.matrix(out)) out <- out[,1]
  out
}

RadialSvm$prob = function(modelFit, newdata, submodels = NULL) {
  out <- try(attr(predict(modelFit, newdata, probability = TRUE), "probabilities"),
             silent = TRUE)
  if(class(out)[1] != "try-error") {
    ## There are times when the SVM probability model will
    ## produce negative class probabilities, so we
    ## induce vlaues between 0 and 1
    if(any(out < 0)) {
      out[out < 0] <- 0
      out <- t(apply(out, 1, function(x) x/sum(x)))
    }
    out <- out[, modelFit$levels, drop = FALSE]
  } else {
    warning("e1071 class probability calculations failed; returning NAs")
    out <- matrix(NA, nrow(newdata) * length(modelFit$levels), ncol = length(modelFit$levels))
    colnames(out) <- modelFit$levels
  }
  out
}


###########################
# Load modified functions #
########## NNET ###########
Nnet <- getModelInfo("nnet")[[1]]
Nnet$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
  dots <- list(...)
  wts <- dots$mywts
  cat(".")
  
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  if(!is.null(wts)) {
    weights <- ifelse(dat$.outcome=="X0", wts[1], 1) # Ws[[w]]["0"], Ws[[w]]["1"]) I opted to use the same weighting style as svmRadialWeights
    cw <- weights#/sum(weights)
    out <- nnet::nnet(.outcome ~ .,
                      data = dat,
                      weights = cw,#wts,
                      size = param$size,
                      decay = param$decay,
                      trace = FALSE,
                      ...)
  } else out <- nnet::nnet(.outcome ~ .,
                           data = dat,
                           size = param$size,
                           decay = param$decay,
                           ...)
  out
}

###########################
# Load modified functions #
##### Learning curve ###### 
# Modified so that a separate test set (tdat) can be used (instead of a proportion of the dat)
learning_curve_tst <- function (dat, tdat, outcome = NULL, proportion = (1:10)/10, test_prop = 0, 
                                verbose = TRUE, ...) 
{
  if (is.null(outcome)) 
    stop("Please give a character stirng for the outcome column name")
  proportion <- sort(unique(proportion))
  n_size <- length(proportion)
  if (test_prop > 0) {
    for_model <- createDataPartition(dat[, outcome], p = 1 - 
                                       test_prop, list = FALSE)
  }
  else for_model <- 1:nrow(dat)
  n <- length(for_model)
  resampled <- vector(mode = "list", length = n_size)
  tested <- if (test_prop > 0) 
    resampled
  else NULL
  apparent <- resampled
  for (i in seq(along = proportion)) {
    if (verbose) 
      cat("Training for ", round(proportion[i] * 100, 
                                 1), "% (n = ", floor(n * proportion[i]), ")\n", 
          sep = "")
    in_mod <- if (proportion[i] < 1) 
      sample(for_model, size = floor(n * proportion[i]))
    else for_model
    mod <- train(x = dat[in_mod, colnames(dat) != outcome, 
                         drop = FALSE], y = dat[in_mod, outcome], ...)
    if (mod$control$method == "none") 
      stop("`learning_curve_dat` uses resampling so please choose a value of ", 
           "`method` that is not 'none'", call. = FALSE)
    if (i == 1) 
      perf_names <- mod$perfNames
    resampled[[i]] <- merge(mod$resample, mod$bestTune)
    resampled[[i]]$Training_Size <- length(in_mod)
    if (test_prop > 0) {
      if (!mod$control$classProbs) {
        test_preds <- extractPrediction(list(model = mod), 
                                        testX = dat[-for_model, colnames(dat) != outcome, 
                                                    drop = FALSE], testY = dat[-for_model, outcome])
      }
      else {
        test_preds <- extractProb(list(model = mod), 
                                  testX = dat[-for_model, colnames(dat) != outcome, 
                                              drop = FALSE], testY = dat[-for_model, outcome])
      }
      test_perf <- mod$control$summaryFunction(test_preds, 
                                               lev = mod$finalModel$obsLevels)
      test_perf <- as.data.frame(t(test_perf))
      test_perf$Training_Size <- length(in_mod)
      tested[[i]] <- test_perf
      try(rm(test_preds, test_perf), silent = TRUE)
    }
    ### v-- Modified by AJ 
    else if (is.data.frame(tdat)) {
      if (!mod$control$classProbs) {
        test_preds <- extractPrediction(list(model = mod), 
                                        testX = tdat[ , colnames(tdat) != outcome, 
                                                      drop = FALSE], testY = tdat[ , outcome])
      }
      else {
        test_preds <- extractProb(list(model = mod), 
                                  testX = tdat[ , colnames(tdat) != outcome, 
                                                drop = FALSE], testY = tdat[ , outcome])
      }
      test_perf0 <- mod$control$summaryFunction(test_preds, 
                                               lev = mod$finalModel$obsLevels)
      test_perf <- mod$control$summaryFunction(test_preds[which(test_preds$dataType=="Test"),], 
                                               lev = mod$finalModel$obsLevels)
      test_perf <- as.data.frame(t(test_perf))
      test_perf$Training_Size <- length(in_mod)
      tested[[i]] <- test_perf
      try(rm(test_preds, test_perf), silent = TRUE)
    }
    ### ^-- Modified by AJ  
    if (!mod$control$classProbs) {
      app_preds <- extractPrediction(list(model = mod), 
                                     testX = dat[in_mod, colnames(dat) != outcome, 
                                                 drop = FALSE], testY = dat[in_mod, outcome])
    }
    else {
      app_preds <- extractProb(list(model = mod), testX = dat[in_mod, 
                                                              colnames(dat) != outcome, drop = FALSE], testY = dat[in_mod, 
                                                                                                                   outcome])
    }
    app_perf <- mod$control$summaryFunction(app_preds, lev = mod$finalModel$obsLevels)
    app_perf <- as.data.frame(t(app_perf))
    ### v-- Modified by AJ
    # app_perf[,1] <- mean(mod$resample$ROC)
    ### ^-- Modified by AJ
    app_perf$Training_Size <- length(in_mod)
    apparent[[i]] <- app_perf
    try(rm(mod, in_mod, app_preds, app_perf), silent = TRUE)
  }
  resampled <- do.call("rbind", resampled)
  resampled <- resampled[, c(perf_names, "Training_Size")]
  resampled$Data <- "Resampling"
  apparent <- do.call("rbind", apparent)
  apparent <- apparent[, c(perf_names, "Training_Size")]
  apparent$Data <- "Training"
  out <- rbind(resampled, apparent)
  if (test_prop > 0) {
    tested <- do.call("rbind", tested)
    tested <- tested[, c(perf_names, "Training_Size")]
    tested$Data <- "Testing"
    out <- rbind(out, tested)
  }
  ### v-- Modified by AJ 
  else if (is.data.frame(tdat)) {
    tested <- do.call("rbind", tested)
    tested <- tested[, c(perf_names, "Training_Size")]
    tested$Data <- "Testing"
    out <- rbind(out, tested)
  }
  ### ^-- Modified by AJ  
  out
}







# Load SMOTE function
smoteN <- function(x, y) {
  dat <-
    if (is.data.frame(x)) {
      if (inherits(x, "tbl_df"))
        as.data.frame(x)
      else
        x
    }
  else
    as.data.frame(x)
  dat$.y <- y
  n <- 1 # 5 x no. of cases
  Over_Perc <- (n*5-1)*100 
  Under_Perc <- 100 + 100/(n*5-1)
  dat <- SMOTE(.y ~ ., data = dat, perc.over = Over_Perc,perc.under=Under_Perc)
  #cat(paste0("SMOTE diagnostics - dat size = ", paste(table(dat$.y), collapse=" + "), "\n"))
  list(x = dat[,!grepl(".y", colnames(dat), fixed = TRUE), drop = FALSE],
       y = dat$.y)
}

create_balanced_folds = function(labs,nfold,seedy){
  # labs are the labels
  # nfold is the number of folds
  # stolen from cvGenStratified in CORElearn
  classVal = factor(labs)
  levs = factor(levels(classVal), levels = levels(classVal))
  classFreq = table(classVal)
  noClasses = length(levs)
  n = length(classVal)
  srt = order(classVal)
  vec = array(1:nfold, dim = n)
  cv = array(0, dim = n)
  cv[srt] = vec
  set.seed(seedy)
  for (i in 1:noClasses){
    cv[classVal == levs[i]] = sample(cv[classVal == levs[i]], size = classFreq[i], replace = F)
  }
  return(cv)
}


# load the data
allcombs=readRDS("combinations_v2.rds")
M <- read.csv("DB_no_red_obs_&_variables_defined_&_cleaned(b)_&_labeled(v2)_trainSetC.csv")
colnames(M) <- sapply(colnames(M), function(x) { sub(pattern = ".", replacement = "_", x=x, fixed=TRUE) })
MD <- M
L=readRDS("dataout17April.rds")
Bassedata <- L$traindata
Basselabels <- L$trainlabe
Basse <- as.data.frame(cbind(Bassedata, Basselabels))
colnames(Basse) <- c(L$trainvarn, "Died")
### Sort data by admission date
MD_ordered <- MD[order(MD$f2date),]
Bassedata_ordered <- Bassedata[order(MD$f2date),]
Basselabels_ordered <- Basselabels[order(MD$f2date),]
### Splitting Basse into 3 and holding out one third for generalizability testing
bfolds <- 3
trainrows <- 1:round(nrow(Bassedata_ordered)/bfolds*(bfolds-1))
testrows <- (round(nrow(Bassedata_ordered)/bfolds*(bfolds-1))+1):nrow(Bassedata_ordered)
d1 <- MD_ordered$f2date[min(which(MD_ordered$f2date!=""))]; d2 <- MD_ordered$f2date[max(trainrows)]; d3 <- MD_ordered$f2date[min(testrows)]; d4 <- MD_ordered$f2date[max(testrows)]
paste0("Training set ranges dates from ", d1, " to ", d2, " (including ", sum(MD$f2date==""), " without admission date)")
paste0("Testing set ranges dates from ", d3, " to ", d4)
traindata = Bassedata_ordered[trainrows,] 
trainlabels = Basselabels_ordered[trainrows]
testdata = Bassedata_ordered[testrows,]
testlabels = Basselabels_ordered[testrows]

# Listing hyperparameters 
rcv_folds <- 10              # Number of folds used in the cross-validation
rcv_repeats <- 5             # Number of times the cross-validation is repeated
Ws <- c(1/4, 1/2, 1/1.5, 1)  # class weights to penalise less misclassifications of non-events (events are weighted 1)
non.linear <- FALSE  

### Features defining the chosen model ###
alg<- "RF"                   # Options: "NNET", "RF", "SVM"
fcomb <- 3436                # Feature combination to use
wts <- 1                     # class weight
##########################################

k <- fcomb
Ws <- wts
model <- TRUE     # TRUE to generate model
lcurve <- TRUE    # TRUE to generate learning curve
hero <- TRUE      # TRUE to save the model
# Extract hyperparameters from ...
#dots <- list(...)
params <- ""
hyperp="-"
modeldef <- data.frame(alg, fcomb, wts)
outputlist <- list(model_id = t(modeldef), caret.model="", lcurve_model="", lcurve_plot="", best_thresh="")
if (alg=="SVM") { alg_fctn <- RadialSvm 
} else if (alg=="NNET") { alg_fctn <- Nnet 
} else if (alg=="RF") { alg_fctn <- Ranger }
cat(paste0("\n Starting feature combination ", k, " with class weights: X0 = ", wts,"; X1 = 1 \n ------------------------- \n"))# using ", cores_2_use, " of ", availableCores()," available cores \n"))
j=which(allcombs[k,]==1)

featnames <- L$trainvarn[j]
traindat.df <- as.data.frame(traindata[,j]) 
testdat.df  <- as.data.frame(testdata[ ,j])
factorY <- as.factor(trainlabels) 
testY <- as.factor(testlabels)
Yfactor<- factorY
levels(Yfactor) <- make.names(levels(Yfactor))
levels(testY) <- make.names(levels(testY))
train.df <- cbind(traindat.df, Yfactor)
test.df  <- cbind(testdat.df,  Yfactor = testY)
names <- sapply(L$trainvarn, function(x) { sub(pattern="_", replacement=".", x=x, fixed=TRUE) })
names["age"] <- "Age"
names(train.df) <- c(names[j], "Yfactor")
names(test.df) <- c(names[j], "Yfactor")

# Preparing table
#  colnames(chosenbyweight) <- c("feat.comb", "n.feats", "Weight", "params", "alg", "AUC", "Sens_0.5", "Spec_0.5", # caret takes X0 as the positive class by default, resulting in
#                                # sensitivity (col 4) and specificity (col 5) being switched
#                                "best_thresh", "Sens_thresh", "Spec_thresh", "time")
if (alg=="SVM") {
  chosenbyweight <- data.frame(matrix(data=0, nrow = length(Ws), ncol = 12))
  colnames(chosenbyweight) <- c("feat.comb", "n.feats", "Weight", "Sigma", "C", "AUC", "Sens_0.5", "Spec_0.5", # caret takes X0 as the positive class by default, resulting in 
                                # sensitivity (col 4) and specificity (col 5) being switched
                                "best_thresh", "Sens_thresh", "Spec_thresh", "time")
} else if (alg=="NNET") {
  chosenbyweight <- data.frame(matrix(data=0, nrow = length(Ws), ncol = 12))
  colnames(chosenbyweight) <- c("feat.comb", "n.feats", "Weight", "size", "decay", "AUC", "Sens_0.5", "Spec_0.5", # caret takes X0 as the positive class by default, resulting in 
                                # sensitivity (col 4) and specificity (col 5) being switched
                                "best_thresh", "Sens_thresh", "Spec_thresh", "time")
} else if (alg=="RF") {
  chosenbyweight <- data.frame(matrix(data=0, nrow = length(Ws), ncol = 13))
  colnames(chosenbyweight) <- c("feat.comb", "n.feats", "Weight", "mtry", "splitrule", "min.node.size", "AUC", "Sens_0.5", "Spec_0.5", # caret takes X0 as the positive class by default, resulting in 
                                # sensitivity (col 4) and specificity (col 5) being switched
                                "best_thresh", "Sens_thresh", "Spec_thresh", "time")
}

cw <- c("X0"=Ws[1], "X1"=1)
tr.ctrl <- trainControl(method="adaptive_cv", number=rcv_folds, repeats = rcv_repeats, 
                        adaptive = list(min = 3, alpha = 0.05, method = 'gls', complete = TRUE),
                        sampling = smoteN,
                        summaryFunction = twoClassSummary, 
                        savePredictions = "final",
                        classProbs = TRUE, 
                        verboseIter = FALSE,
                        search ="random",
                        allowParallel = FALSE)
if(model!=FALSE) {
  set.seed(42, kind="Mersenne-Twister", sample.kind = "Rounding")
  caret.model <- train(form = Yfactor ~ .,
                       data = train.df,
                       method = alg_fctn,  
                       trControl = tr.ctrl,
                       mywts = cw,
                       metric="ROC", 
                       preProcess = c("center", "scale"),
                       tuneLength = 20)
  outputlist$caret.model <- caret.model
}
if(lcurve==TRUE) {
  set.seed(42, kind="Mersenne-Twister", sample.kind = "Rounding")
  learning_curve20 <- learning_curve_tst(dat = train.df,
                                     tdat = test.df,
                                     outcome = "Yfactor",
                                     proportion = (2:40)/40,
                                     verbose = TRUE,
                                     ## 'train' arguments
                                     method = alg_fctn,
                                     trControl = tr.ctrl,
                                     mywts = cw,
                                     metric="ROC",
                                     preProcess = c("center", "scale"),
                                     tuneLength = 20)
  lcurveplot <- ggplot(learning_curve20[which(learning_curve20$Data != "Resampling"),], aes(x = Training_Size, y = ROC, color = Data)) +
    geom_smooth(method = loess) + geom_point() +
    guides(color = guide_legend(reverse=TRUE)) +
    scale_color_discrete(name = "") +
    ylab("AUC") + xlab("Size of training set")
  lcurveplotBW <- ggplot(learning_curve20[which(learning_curve20$Data != "Resampling"),], aes(x = Training_Size, y = ROC, group = Data)) +
    geom_smooth(aes(linetype=Data), method = loess, se=FALSE, color="black") + 
    geom_point(aes(shape=Data)) + scale_shape_manual(values = c(16, 1)) +
    guides(color = guide_legend(reverse=TRUE)) +
    scale_color_discrete(name = "") +
    ylab("AUC") + xlab("Size of training set")
  outputlist$lcurve_model <- learning_curve20
  outputlist$lcurve_plot  <- lcurveplot
  outputlist$lcurve_plotBW  <- lcurveplotBW
}

roc <- roc(caret.model$pred$obs, caret.model$pred$X1, direction = "<", quiet=TRUE)
best_thresh <- coords(roc, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "closest.topleft", transpose = FALSE)
outputlist$best_thresh <- best_thresh

if (alg=="SVM") {
  TheChosenOne <- which(caret.model$results$sigma == caret.model$bestTune$sigma &
                          caret.model$results$C == caret.model$bestTune$C)
  ROC_avg <- mean(caret.model$resample$ROC)
  ROC_sd <- sd(caret.model$resample$ROC)
  ROC_iqr <- round(quantile(caret.model$resample$ROC, c(0.25, 0.75)), 4)
  ROC_range <- range(caret.model$resample$ROC)
  ROC_ci <- ci.auc(roc)
  best.result <- cbind(k, length(j), Ws[[1]], 
                       caret.model$results[TheChosenOne, c(1:3,5,4)], # caret takes X0 as the positive class by default, resulting in 
                       # sensitivity (col 4) and specificity (col 5) being switched
                       best_thresh, (caret.model$times$everything[3]/60))
  # best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (IQR ", ROC_iqr[1], "-", ROC_iqr[2], ")"))
  best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (95%CI ", round(ROC_ci[1], 4), "-", round(ROC_ci[3], 4), ")"))
  chosenbyweight[1, ] <- best.result
} else if (alg=="NNET") {
  TheChosenOne <- which(caret.model$results$size == caret.model$bestTune$size &
                          caret.model$results$decay == caret.model$bestTune$decay)
  ROC_avg <- mean(caret.model$resample$ROC)
  ROC_sd <- sd(caret.model$resample$ROC)
  ROC_iqr <- round(quantile(caret.model$resample$ROC, c(0.25, 0.75)), 4)
  ROC_range <- range(caret.model$resample$ROC)
  ROC_ci <- ci.auc(roc)
  best.result <- cbind(k, length(j), Ws[[1]], 
                       caret.model$results[TheChosenOne, c(1:3,5,4)], # caret takes X0 as the positive class by default, resulting in 
                       # sensitivity (col 4) and specificity (col 5) being switched
                       best_thresh, (caret.model$times$everything[3]/60))
  # best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (IQR ", ROC_iqr[1], "-", ROC_iqr[2], ")"))
  best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (95%CI ", round(ROC_ci[1], 4), "-", round(ROC_ci[3], 4), ")"))
  chosenbyweight[1, ] <- best.result
} else if (alg=="RF") {
  TheChosenOne <- which(caret.model$results$min.node.size == caret.model$bestTune$min.node.size &
                          caret.model$results$mtry == caret.model$bestTune$mtry &
                          caret.model$results$splitrule == caret.model$bestTune$splitrule)
  ROC_avg <- mean(caret.model$resample$ROC)
  ROC_sd <- sd(caret.model$resample$ROC)
  ROC_iqr <- round(quantile(caret.model$resample$ROC, c(0.25, 0.75)), 4)
  ROC_range <- range(caret.model$resample$ROC)
  ROC_ci <- ci.auc(roc)
  best.result <- cbind(k, length(j), Ws[[1]], 
                       caret.model$results[TheChosenOne, c(1:4,6,5)], # caret takes X0 as the positive class by default, resulting in 
                       # sensitivity (col 5) and specificity (col 6) being switched
                       best_thresh, (caret.model$times$everything[3]/60))
  # best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (IQR ", ROC_iqr[1], "-", ROC_iqr[2], ")"))
  best.result$ROC <- c(paste0(round(best.result$ROC, 4), " (95%CI ", round(ROC_ci[1], 4), "-", round(ROC_ci[3], 4), ")"))
  chosenbyweight[1, ] <- best.result
} 


cat(paste0("\n *** Finished feature combination ", k, " with weight ", Ws[1], " in ", sum(caret.model$times$everything[3]/60), " minutes. \n"))
cat("\n")
cat("*** Results from the training set ***")
cat("\n")
outputlist$results_training_set <- chosenbyweight
print(chosenbyweight)
cat("\n")

# If the model is to be saved
if (hero==TRUE) {
  caret.model2 <- caret.model
  caret.model2$gen.data <- list(algorithm = alg, f.comb = fcomb, weights = paste0(wts, " (non-cases) to 1 (cases)"), features = featnames)
  caret.model2$best_thresh <- best_thresh
  outputfilename <- paste(alg, fcomb, "-", wts, ".rds", sep="")
  saveRDS(caret.model2, file=outputfilename)
}

  
  
beep(2)  
  
  














cat("*** Predict on test set with best threshold (from training set) *** \n")
cat("\n")
  
### Predict on test set with best threshold (from training set) ###
predicted_p <- predict.train(caret.model, newdata = test.df, type="prob")
predictions <- as.numeric(predicted_p[,2] > best_thresh$threshold)
sidebyside <- cbind(as.factor(testlabels), predicted_p[,2], predictions)
table(predictions, testlabels)
cm <- (confusionMatrix(data=as.factor(predictions), reference = as.factor(testlabels), positive = "1", mode="everything"))
cm
outputlist$ConfMat_on_testset <- cm
# plot(as.numeric(predicted_p[,2]), as.factor(testlabels));   abline(v=best_thresh$threshold)
densiplot <- ggplot() + 
  geom_density(aes(x = predicted_p[,2], fill = as.factor(testlabels)), alpha = 0.2) +
  geom_vline(xintercept = best_thresh$threshold) +
  ylab("Density") + xlab(paste("predicted probability")) +
  #scale_fill_discrete(name = "Real outcome", labels = c("Alive", "Dead")) +
  theme_bw() + scale_fill_grey(name = "Real outcome", labels = c("Alive", "Dead")) + 
  theme(legend.position = "bottom") +
  annotate("label", x = best_thresh$threshold, y=5.35, label="threshold") +
  annotate("text" , x = best_thresh$threshold-0.01, y=5.1, label="Alive", hjust="right") +
  annotate("text" , x = best_thresh$threshold+0.01, y=5.1, label="Dead", hjust="left")
densiplotBW <- ggplot() + 
  geom_density(aes(x = predicted_p[,2], fill = as.factor(testlabels)), alpha = 0.2) +
  #geom_density_pattern(aes(x = predicted_p[,2], pattern_fill = as.factor(testlabels)) ) + #, alpha = 0.2) +
  geom_vline(xintercept = best_thresh$threshold) +
  scale_fill_brewer(palette="OrRd") +
  ylab("Density") + xlab(paste("predicted probability")) +
  #scale_fill_discrete(name = "Real outcome", labels = c("Alive", "Dead")) +
  theme_bw() + scale_fill_grey(name = "Real outcome", labels = c("Alive", "Dead")) + 
  theme(legend.position = "bottom") +
  annotate("label", x = best_thresh$threshold, y=5.35, label="threshold") +
  annotate("text" , x = best_thresh$threshold-0.01, y=5.1, label="Alive", hjust="right") +
  annotate("text" , x = best_thresh$threshold+0.01, y=5.1, label="Dead", hjust="left")
outputlist$Densiplot_on_testset <- densiplot
roc <- roc(as.factor(testlabels), predicted_p[,2], direction = "<", quiet=TRUE)
th<- coords(roc, x=best_thresh$threshold, input="threshold", ret = c("thresh", "sens", "spec"), transpose = FALSE)

  #   ths <- th
  #   threshpoints <- seq(from=0.1, to=0.9, by=0.1)
  #   for(i in 1:length(threshpoints)) {
  #     ths <- rbind(ths, coords(roc, x=threshpoints[i], input="threshold", ret = c("thresh", "sens", "spec"), transpose = FALSE) )
  #   }
  # plot(roc, main=paste0("ROC curve and alternative thresholds"))
  #   points(x=ths[,3], y=ths[,2], pch=19)  
  #   text(x=ths[,3], y=ths[,2], labels = paste0(round(ths[,1]*100, 2), "%"), adj = c(0,1))

# Calibration plot
cal_obj <- calibration(as.factor(testlabels) ~ predicted_p[,2], class="1", cuts=10)
plot(cal_obj, type = "l", auto.key = list(lines = TRUE, points = FALSE))
calplot <- ggplot(cal_obj, type = "l", auto.key = list(lines = TRUE, points = FALSE)) +
  #geom_vline(xintercept = best_thresh$threshold*100) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,100,10)) +
  ylim(0,50)  +
  xlab(paste("predicted probability")) 
outputlist$Caliplot_on_testset <- calplot  

# roc plot
rocplot <- ggroc(roc, legacy.axes=TRUE) +
  ggtitle(paste0("ROC curve and optimal threshold (", round(th[1],2), ")")) +
  geom_point(aes(x=1-th[1,3], y=th[1,2]), color="red", size=2)
outputlist$Rocplot_on_testset <- rocplot
#t <- coords(roc, "best", best.method = "closest.topleft", transpose = FALSE); abline(v=t[1,2]); abline(h=t[1,3])
#points(x=th[1,3], y=th[1,2], pch=19, col="red")
#abline(v=th[1,3], col="red"); abline(h=th[1,2], col="red")
#legend("bottomright", legend=c("using theoretical best threshold for this ROC", "using best threshold established in training"), fill = c("black", "red"))
ROC_ci <- ci.auc(roc)
ROC_test <- paste0(round(as.numeric(roc$auc), 4), " (95%CI ", round(ROC_ci[1], 4), "-", round(ROC_ci[3], 4), ")") 
Sens_test <- th$sensitivity
Spec_test <- th$specificity
# coords <- matrix(c(1, 1, Spec_test, Sens_test), ncol=2, byrow=TRUE)
# colnames(coords) <- c("Spec", "Sens")
# rownames(coords) <- c("Best", "Current")
# Dist_test <- dist(coords)[1]
weights <- paste0("X0= ", round(wts, 4), " ; X1= 1")
if(alg=="RF") {
  best.result  <- cbind(chosenbyweight[1,1:2], subsamp="", weights, chosenbyweight[1,c(4:7, 10:12)], ROC_test, Sens_test, Spec_test)
} else {
  best.result  <- cbind(chosenbyweight[1,1:2], subsamp="", weights, chosenbyweight[1,c(4:6, 9:11)], ROC_test, Sens_test, Spec_test)
}
#best.result <- cbind(fcomb, length(j), subsamp="", weights, caret.model$results[TheChosenOne,c(1:3, 8:10)], ROC_test, Sens_test, Spec_test)
best.result[3] <- as.character("SMOTE (c*5)")
best.result[4] <- as.character(weights)
names(best.result)[c(8,10:12)] <- c("AUC_train", "Sens_train", "Spec_train", "AUC_test")
print(best.result)

outputlist$results_test_set <- best.result

caret.model3 <- caret.model
caret.model3$gen.data <- list(algorithm = alg, f.comb = fcomb, weights = paste0(wts, " (non-cases) to 1 (cases)"), features = featnames)
caret.model3$best_thresh <- best_thresh
outputfilename <- paste(alg, fcomb, "-", wts, ".rds", sep="")
if (hero==TRUE){
  saveRDS(caret.model3, file=outputfilename)
  saveRDS(outputlist, file="MRCmodel2021.rds")
}



outputlist

