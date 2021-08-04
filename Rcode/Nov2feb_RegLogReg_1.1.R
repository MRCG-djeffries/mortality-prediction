#!/usr/bin/env Rscript
# Original file name: Nov2feb_1.4_code
# v1.0 => Adapt svm code to RegLogReg
            # RegLogReg model info (with weighing scheme)
            # [x] Use non-linear
            # Adapt post-processing tables
            # Prepare for testing: 5 folds, 1 rep, weight=1/10, adapt_min=2, tuneLength=5
# v1.1 => Execution parameters
            # 5 reps of 10-folds cross-validation
            # weights -> 1/4, 1/2, 1/1.5, 1
            # adaptive_cv with min=3 and alpha=0.05
            # tuneLength=20

# Load script with function to assess ROC thresholds (to be used as 'method' in the train() function)
#library(beepr)
library(caret)
#library(doParallel)
#library(future)
library(pROC)
library(LiblineaR)
library(DMwR)

RegLogReg <- getModelInfo("regLogistic")[[1]]
RegLogReg$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  
  if( !(param$loss %in% c( "L1", "L2_dual", "L2_primal")) ) {
    stop("Loss function is not recognised.", call. = FALSE)
  }
  if(!is.factor(y)) {
    stop('y is not recognised as a factor', call. = FALSE)
  }
  model_type <-
    ifelse(param$loss == "L1", 6, ifelse(param$loss == "L2_primal", 0, 7))
  dots <- list(...)
  wts <- dots$mywts
  cat(".")
  
  out <- LiblineaR::LiblineaR(
    data = as.matrix(x), 
    target = y,
    cost = param$cost, 
    epsilon = param$epsilon,
    type = model_type,
    wi = wts,
    ...
  )
  
  out
}

#smoteN <- getSamplingInfo("smote")
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

trans_rlr_v8 = function() {
    args = commandArgs(trailingOnly=TRUE)
    if(length(args)>0) { runy=as.integer(args[1]) }
    #runy <- 200
    #if(exists("runy")) { if(runy!=32) { stop("runy not 32") }}
  #runy=40 # (Lims 2295-2368) to get the combination 2325 for testing

  # load the loop limits data
  Lims=readRDS("runmat_v2.2.rds")
  
  # Check existing data
  allfiles <- list.files("RegLogReg_caret")
  if (length(allfiles)>0) {
    ns <- rep(0,length(allfiles))
    for (i in 1:length(allfiles)) {
      pos = gregexpr('_', allfiles[i])
      n <- substr(allfiles[i], pos[[1]][2]+1, nchar(allfiles[i])-4)
      ns[i] <- as.numeric(n)
    }
    if(runy %in% ns) {
      testresults <- readRDS(paste0("RegLogReg_caret/newgen_rlr_", runy, ".Rds"))[[1]]
      possible_combs <- c(Lims[runy,1]:Lims[runy,2])
      a <- testresults$feat.comb
      if(sum(!(possible_combs %in% a)) == 0) { # Number of values in possible_combs that are not in a
        stop(paste0("runy (", runy, ")  already completely done"))#and runy+886 (", (runy+886), ") already done"))
      }
      missing <- possible_combs[which(!(possible_combs %in% a))]
      mis_start <- min(missing)
      mis_end <- max(missing)
    }
  }
    
    
  # Load the 65,535 possible combinations
  allcombs=readRDS("combinations_v2.rds")
    
  # load the data
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
  rcv_folds <- 10
  rcv_repeats <- 5
  Ws <- c(1/4, 1/2, 1/1.5, 1)
  non.linear <- FALSE
  
  if(exists("mis_start")) { starty <- mis_start } else { starty = Lims[runy,1] }
  if(starty<=16) { starty <- 17 }
  if(exists("mis_end")) { stopy <- mis_end } else { stopy = Lims[runy,2] }

  leny = stopy-starty+1
  if(exists("testresults")) {
    bests <- testresults
  } else {
    bests <- data.frame(matrix(data=0, nrow = leny, ncol = 13))
  }

  # Start the clock!
  ptm <- proc.time()
  k<-starty
    for (k in starty:stopy){
      # # Starting parallelization
      # #cat(paste0("Available cores: ", availableCores(), "\n"))
      # cores_2_use <- availableCores() - 1
      # if (cores_2_use >5) { cores_2_use <- 5 }
      # if (cores_2_use >1) {
      #   #cat(paste0("Cores to use: ", cores_2_use, "\n"))
      #   cl <- makePSOCKcluster(cores_2_use)
      #   registerDoParallel(cl)
      # } else { cores_2_use<-1 }
      
      cat(paste0("\n Starting feature combination ", k, " (runy= ", runy,") \n ------------------------- \n"))# using ", cores_2_use, " of ", availableCores()," available cores \n"))
      j=which(allcombs[k,]==1)

      traindat.df <- as.data.frame(traindata[,j]) 
      if(non.linear==TRUE) {
        ## Deal with non-linear associations
        # Days_unwell -> Days_unwell + Days_unwell^2
        if("Days_unwell" %in% names(traindat.df) == TRUE) {
          traindat.df$Days_unwell_2 <- traindat.df$Days_unwell^2
        }
        # Resp_Rate -> Resp_Rate + ln(RespRate)*Resp_Rate
        if("Resp_Rate" %in% names(traindat.df) == TRUE) {
          traindat.df$Resp_Rate_ln_1 <- log(traindat.df$Resp_Rate)*traindat.df$Resp_Rate
        }
        # Heart_Rate -> Heart_Rate^2 + ln(Heart_Rate)*Heart_Rate^3
        if("Heart_Rate" %in% names(traindat.df) == TRUE) {
          traindat.df$Heart_Rate_ln_3 <- log(traindat.df$Heart_Rate)*traindat.df$Heart_Rate^3
          traindat.df$Heart_Rate <- traindat.df$Heart_Rate^2
        }
        # wfh_zscore -> wfh_zscore + wfh_zscore^2
        if("wfh_zscore" %in% names(traindat.df) == TRUE) {
          traindat.df$wfh_zscore_2 <- traindat.df$wfh_zscore^2
        }
        # MUAC -> MUAC^-2 + ln(MUAC)*MUAC^-2
        if("MUAC" %in% names(traindat.df) == TRUE) {
          traindat.df$MUAC_ln_m2 <- log(traindat.df$MUAC)*traindat.df$MUAC^-2
          traindat.df$MUAC <- traindat.df$MUAC^-2
        }
      }
      
      factorY <- as.factor(trainlabels) 
      Yfactor<- factorY
      levels(Yfactor) <- make.names(levels(Yfactor))
      train.df <- cbind(traindat.df, Yfactor)
      #seeds <- vector(mode = "list", length = (rcv_folds*rcv_repeats+1))
      #for(i in 1:(length(seeds)-1)) seeds[[i]] <- i*c(1:100)#(length(Cs) * (length(Ss)+1) * length(Ws)+100))
      #seeds[[length(seeds)]] <- as.integer(7)
      
      # Handling weights
      chosenbyweight <- data.frame(matrix(data=0, nrow = length(Ws), ncol = 13))
      colnames(chosenbyweight) <- c("feat.comb", "n.feats", "Weight", "cost", "loss", "epsilon", "AUC", "Sens_0.5", "Spec_0.5", # caret takes X0 as the positive class by default, resulting in 
                                    # sensitivity (col 4) and specificity (col 5) being switched
                                    "best_thresh", "Sens_thresh", "Spec_thresh", "time")
      startWindex <- 1
      # Check existing data
      allfiles_ws <- list.files("RegLogReg_caret_ws")
      if (length(allfiles_ws)>0) {
        if (paste0("featcombweights_rlr_", runy, ".Rds") %in% allfiles_ws) {
          stored_chosenbyweight <- readRDS(paste0("RegLogReg_caret_ws/featcombweights_rlr_", runy, ".Rds"))
          if (max(stored_chosenbyweight$feat.comb) != k & max(stored_chosenbyweight$feat.comb) != 0) {
            cat("Chosenbyweights table corresponded to fcomb", max(stored_chosenbyweight$feat.comb), "not", k)
          } else if (max(stored_chosenbyweight$feat.comb) != 0) {
            startWindex <- which(Ws==max(stored_chosenbyweight$Weight))+1
            if (startWindex > length(Ws)) { stop("chosenbyweights table already full?") }
            chosenbyweight <- stored_chosenbyweight
            cat("Unfinished business detected with f.comb", k, ". Continue with weight", Ws[startWindex])
            print(chosenbyweight)
          } else {
            chosenbyweight <- stored_chosenbyweight
          }
        }
      }
      w <- 1
      for (w in startWindex:length(Ws)) {
        #weights <- ifelse(Yfactor=="X0", Ws[w], 1) # Ws[[w]]["0"], Ws[[w]]["1"]) I opted to use the same weighting style as svmRadialWeights
        #cw <- weights/sum(weights)
        cw <- c("X0"=Ws[w], "X1"=1)
        tr.ctrl.rlr <- trainControl(method="adaptive_cv", number=rcv_folds, repeats = rcv_repeats, 
                                       adaptive = list(min = 3, alpha = 0.05, method = 'gls', complete = TRUE),
                                       sampling = smoteN,
                                       summaryFunction = twoClassSummary, 
                                       savePredictions = "final",
                                       classProbs = TRUE, 
                                       verboseIter = FALSE,
                                       search ="random",
                                       allowParallel = FALSE)
        set.seed(42)
        caret.model <- train(form = Yfactor ~ .,
                           data = train.df,
                           method = RegLogReg,#"regLogistic",  
                           #tuneGrid = model.grid,
                           trControl = tr.ctrl.rlr,
                           mywts = cw,
                           metric="ROC", 
                           preProcess = c("center", "scale"),
                           tuneLength = 20)
        roc <- roc(caret.model$pred$obs, caret.model$pred$X1, direction = "<", quiet=TRUE)
        best_thresh <- coords(roc, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "closest.topleft", transpose = FALSE)
        
        TheChosenOne <- which(caret.model$results$cost == caret.model$bestTune$cost &
                                caret.model$results$loss == caret.model$bestTune$loss &
                                caret.model$results$epsilon == caret.model$bestTune$epsilon)
        best.result <- cbind(k, length(j), Ws[[w]], 
                             caret.model$results[TheChosenOne, c(1:4,6,5)], # caret takes X0 as the positive class by default, resulting in 
                                                                            # sensitivity (col 3) and specificity (col 4) being switched
                             best_thresh, (caret.model$times$everything[3]/60))
        chosenbyweight[w, ] <- best.result
        cat(paste0("\n *** Finished feature combination ", k, " with weight ", Ws[w], " in ", sum(caret.model$times$everything[3]/60), " minutes. \n"))
        print(chosenbyweight)
        saveRDS(chosenbyweight,paste0("RegLogReg_caret_ws/featcombweights_rlr_", runy, ".Rds"))
      }
      #chosenbyweight
      bests[k-Lims[runy,1]+1, ] <- chosenbyweight[which(chosenbyweight$AUC==max(chosenbyweight$AUC)),]
      colnames(bests) <- colnames(chosenbyweight)
      L=list(bests=bests)#, results=results)
      saveRDS(L,paste0("RegLogReg_caret/newgen_rlr_", runy, ".Rds"))
      
      # Finishing parallelization
      #if (cores_2_use >1) { stopCluster(cl) }
      cat(paste0("Finished feature combination ", k, " in ", sum(chosenbyweight[,11]), " minutes. \n"))
      chosenbyweight[,] <- 0 
      saveRDS(chosenbyweight,paste0("RegLogReg_caret_ws/featcombweights_rlr_", runy, ".Rds"))
      print(L)
    }
  runytime <- (proc.time() - ptm)[3]/60
  cat(paste0("\n End of run ", runy, "    (time: ", runytime, ") \n"))  
  #beep(4)
  return(L)
}

trans_rlr_v8()
