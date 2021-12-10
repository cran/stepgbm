#' @title Select predictive variables for generalized boosted regression modeling (gbm)
#' by various variable influence methods and predictive accuracy in a stepwise algorithm
#'
#' @description This function is to select predictive variables for
#' generalized boosted regression modeling (gbm) based on various variable influence
#'  methods (i.e., relative variable influence (RVI) and knowledge informed RVI
#'  (i.e., KIRVI, and KIRVI2)) and predictive accuracy. It is implemented via the functions
#'  'stepgbmRVI' and 'steprf::steprfAVIPredictors'.
#'
#' @param trainx a dataframe or matrix contains columns of predictive variables.
#' @param trainy a vector of response, must have length equal to the number of
#' rows in trainx.
#' @param method a variable selection method for 'GBM'; can be: "RVI", "KIRVI"
#'  and "KIRVI2". If "RVI" is used, it would produce the same results as
#'   'stepgbmRVI'. By default, "KIRVI" is used.
#' @param var.monotone an optional vector, the same length as the number of
#' predictors, indicating which variables have a monotone increasing (+1),
#' decreasing (-1), or arbitrary (0) relationship with the outcome. By default,
#' a vector of 0 is used.
#' @param family either a character string specifying the name of the distribution to
#' use or a list with a component name specifying the distribution and any
#' additional parameters needed. See gbm for details. By default, "gaussian" is used.
#' @param n.trees the total number of trees to fit. This is equivalent to the
#' number of iterations and the number of basis functions in the additive
#' expansion. By default, 3000 is used.
#' @param learning.rate a shrinkage parameter applied to each tree in the
#' expansion. Also known as step-size reduction. By default, 0.001 is used.
#' @param interaction.depth the maximum depth of variable interactions.
#' 1 implies an additive model, 2 implies a model with up to 2-way
#' interactions, etc. By default, 2 is used.
#' @param bag.fraction the fraction of the training set observations randomly
#' selected to propose the next tree in the expansion. By default, 0.5 is used.
#' @param train.fraction The first train.fraction * nrows(data) observations
#' are used to fit the gbm and the remainder are used for computing
#' out-of-sample estimates of the loss function.
#' @param n.minobsinnode minimum number of observations in the trees terminal
#' nodes. Note that this is the actual number of observations not the total
#' weight. By default, 10 is used.
#' @param cv.fold integer; number of cross-validation folds to perform within
#' gbm. if > 1, then apply n-fold cross validation; the default is 10, i.e.,
#' 10-fold cross validation that is recommended.
#' @param weights an optional vector of weights to be used in the fitting
#' process. Must be positive but do not need to be normalized.
#' If keep.data = FALSE in the initial call to gbm then it is the user's
#' responsibility to resupply the weights to gbm.more. By default, a vector of
#' 1 is used.
#' @param keep.data a logical variable indicating whether to keep the data and
#' an index of the data stored with the object. Keeping the data and index
#' makes subsequent calls to gbm.more faster at the cost of storing an extra
#' copy of the dataset. By default, 'FALSE' is used.
#' @param verbose If TRUE, gbm will print out progress and performance
#' indicators. By default, 'TRUE' is used.
#' @param n.cores The number of CPU cores to use. See gbm for details. By
#' default, 6 is used.
#' @param rpt iteration of cross validation.
#' @param predacc "VEcv" for vecv in function pred.acc.
#' @param min.n.var minimum number of predictive variables remained in the final
#' predictive model the default is 1.
#' @param delta.predacc minimum changes between the accuracy of two consecutive
#' predictive models. By default, 0.01 is used.
#' @param rseed random seed. By default, 1234 is used.
#' @param ... other arguments passed on to gbm.
#'
#' @return A list with the following components: 1) stepgbmPredictorsFinal: the
#'  variables selected for the last GBM model, whether it is of the highest
#'  predictive accuracy need to be confirmed using 'max.predictive.accuracy'
#'  that is listed next; 2) max.predictive.accuracy: the predictive accuracy
#'  of the most accurate GBM model for each run of 'stepgbmRVI', which can be used
#'  to confirm the model with the highest accuracy, 3) numberruns: number of runs
#'  of 'stepgbmRVI'; 4) laststepRVI: the outpouts of last run of 'stepgbmRVI'; 5)
#'   stepgbmRVIOutputsAll: the outpouts of all 'stepgbmRVI' produced during the
#'   variable selection process; 6) stepgbmPredictorsAll: the outpouts of
#'   'stepgbmRVIPredictors' for all 'stepgbmRVI' produced during the variable
#'    selection process; 7) KIRVIPredictorsAll: predictors used for all
#'    'stepgbmRVI' produced during the variable selection process; for a method
#'    "RVI", if the variables are different from those in the traning dataset,
#'    it suggests that these variables should be tested if the predictive
#'    accuracy can be further improved.
#'
#' @references Li, J., Siwabessy, J., Huang, Z., Nichol, S. (2019). "Developing
#' an optimal spatial predictive model for seabed sand content using machine
#' learning, geostatistics and their hybrid methods." Geosciences 9 (4):180.
#'
#' Li, J., Alvarez, B., Siwabessy, J., Tran, M., Huang, Z., Przeslawski, R.,
#'  Radke, L., Howard, F., Nichol, S. (2017). "Application of random forest,
#'  generalised linear model and their hybrid methods with geostatistical
#'  techniques to count data: Predicting sponge species richness."
#'  Environmental Modelling & Software 97: 112-129.
#'
#' Li, J., Alvarez, B., Siwabessy, J., Tran, M., Huang, Z., Przeslawski, R.,
#'  Radke, L., Howard, F., Nichol, S. (2017). Selecting predictors to form the
#'   most accurate predictive model for count data. International Congress on
#'   Modelling and Simulation (MODSIM) 2017, Hobart.
#'
#' @author Jin Li
#' @examples
#' \donttest{
#' library(spm)
#'
#' data(petrel)
#' stepgbm1 <- stepgbm(trainx = petrel[, c(1,2, 6:9)], trainy =
#' log(petrel[, 5] + 1), method = "KIRVI", family = "gaussian", rpt = 2,
#' predacc = "VEcv", cv.fold = 5,  min.n.var = 2,
#' n.cores = 6, delta.predacc = 0.01, rseed = 1234)
#' names(stepgbm1)
#' stepgbm1$stepgbmPredictorsFinal$variables.most.accurate
#' stepgbm1$max.predictive.accuracy
#' stepgbm1$stepgbmPredictorsAll[[1]]
#'
#' # The variables selected can be derived with
#' stepgbm1$stepgbmPredictorsAll[[1]]$variables.most.accurate
#'
#' data(sponge)
#' stepgbm2 <- stepgbm(trainx = sponge[, -3], trainy = sponge[, 3], method = "KIRVI",
#' family = "poisson", rpt = 2, cv.fold = 5, predacc = "VEcv", min.n.var = 2,
#'  n.cores = 6, delta.predacc = 0.01, rseed = 1234)
#' stepgbm2
#' stepgbm2$max.predictive.accuracy
#'
#' # The variables selected can be derived with
#' stepgbm2$stepgbmPredictorsAll[[1]]$variables.most.accurate
#' }
#'
#' @export
stepgbm <- function (trainx, trainy, method = "KIRVI",
                     var.monotone = rep(0, ncol(trainx)),
                     family = "gaussian",
                     n.trees = 3000,          # default number of trees
                     learning.rate = 0.001,
                     interaction.depth = 2,
                     bag.fraction = 0.5,
                     train.fraction = 1.0,
                     n.minobsinnode = 10,
                     cv.fold = 10,
                     weights = rep(1, nrow(trainx)),   # by default set equal
                     keep.data = FALSE,
                     verbose = TRUE,
                     n.cores = 6,
                     rpt = 2,
                     predacc = "VEcv",
                     min.n.var = 2,
                     delta.predacc = 0.001,
                     rseed = 1234, ...) {
  n <- nrow(trainx); p <- ncol(trainx)
  predictive.accuracy1 <- NULL
  predictive.accuracy2 <- NULL

  stepgbmRVI1 <- list()
  stepgbmRVIPredictors1 <- list()
  kirvi.predictors <- list()
  max.predictive.accuracy <- NULL

  if (method == "RVI") {
    stepgbm1 <- stepgbmRVI(trainx = trainx, trainy = trainy, family = family, n.trees = n.trees, learning.rate = learning.rate, interaction.depth = interaction.depth, bag.fraction = bag.fraction, train.fraction = train.fraction, n.minobsinnode = n.minobsinnode, cv.fold = cv.fold, weights = weights, keep.data = keep.data, verbose = verbose, n.cores = n.cores, rpt = rpt, predacc = predacc, min.n.var = min.n.var, rseed = rseed)

    i <- 1
    stepgbmRVI1[[i]] <- stepgbm1

    predictors.selected <- steprf::steprfAVIPredictors(stepgbm1, trainx = trainx)
    stepgbmRVIPredictors1[[i]] <- predictors.selected
    predictive.accuracy1 <- predictors.selected$max.predictive.accuracy
    max.predictive.accuracy[i] <- predictive.accuracy1
    kirvi.predictors[[i]] <- union(predictors.selected$variables.most.accurate, predictors.selected$PABV)

  } else (
    if (method == "KIRVI") {
      stepgbm1 <- stepgbmRVI(trainx = trainx, trainy = trainy, family = family, n.trees = n.trees, learning.rate = learning.rate, interaction.depth = interaction.depth, bag.fraction = bag.fraction, train.fraction = train.fraction, n.minobsinnode = n.minobsinnode, cv.fold = cv.fold, weights = weights, keep.data = keep.data, verbose = verbose, n.cores = n.cores, rpt = rpt, predacc = predacc, min.n.var = min.n.var, rseed = rseed)

      i <- 1
      stepgbmRVI1[[i]] <- stepgbm1

      predictors.selected <- steprf::steprfAVIPredictors(stepgbm1, trainx = trainx)
      stepgbmRVIPredictors1[[i]] <- predictors.selected
      predictive.accuracy1 <- predictors.selected$max.predictive.accuracy
      max.predictive.accuracy[i] <- predictive.accuracy1
      kirvi.predictors[[i]] <- union(predictors.selected$variables.most.accurate, predictors.selected$PABV)

      trainx1 <- trainx[, kirvi.predictors[[i]], drop = FALSE]
      p <- ncol(trainx1)

      for (i in 2:(p - min.n.var)) {
        stepgbm1 <- NULL
        stepgbm1 <- stepgbmRVI(trainx = trainx1, trainy = trainy, family = family, n.trees = n.trees, learning.rate = learning.rate, interaction.depth = interaction.depth, bag.fraction = bag.fraction, train.fraction = train.fraction, n.minobsinnode = n.minobsinnode, cv.fold = cv.fold, weights = weights, keep.data = keep.data, verbose = verbose, n.cores = n.cores, rpt = rpt, predacc = predacc, min.n.var = min.n.var, rseed = rseed)

        stepgbmRVI1[[i]] <- stepgbm1

        predictors.selected <- steprf::steprfAVIPredictors(stepgbm1, trainx = trainx1)
        stepgbmRVIPredictors1[[i]] <- predictors.selected
        predictive.accuracy2 <- predictors.selected$max.predictive.accuracy
        max.predictive.accuracy[i] <- predictive.accuracy2
        kirvi.predictors[[i]] <- union(predictors.selected$variables.most.accurate, predictors.selected$PABV)

        if ((predictive.accuracy2 - predictive.accuracy1) <= delta.predacc)
        {break
        } else
        {predictive.accuracy1 <- predictive.accuracy2
        trainx1 <- trainx[, kirvi.predictors[[i]], drop = FALSE]
        }
      }
    } else (
      if (method == "KIRVI2") {
        stepgbm1 <- stepgbmRVI(trainx = trainx, trainy = trainy, family = family, n.trees = n.trees, learning.rate = learning.rate, interaction.depth = interaction.depth, bag.fraction = bag.fraction, train.fraction = train.fraction, n.minobsinnode = n.minobsinnode, cv.fold = cv.fold, weights = weights, keep.data = keep.data, verbose = verbose, n.cores = n.cores, rpt = rpt, predacc = predacc, min.n.var = min.n.var, rseed = rseed)

        i <- 1
        stepgbmRVI1[[i]] <- stepgbm1

        predictors.selected <- steprf::steprfAVIPredictors(stepgbm1, trainx = trainx)
        stepgbmRVIPredictors1[[i]] <- predictors.selected
        predictive.accuracy1 <- predictors.selected$max.predictive.accuracy
        max.predictive.accuracy[i] <- predictive.accuracy1
        kirvi.predictors[[i]] <- predictors.selected$PABV

        trainx1 <- trainx[, kirvi.predictors[[i]], drop = FALSE]
        p <- ncol(trainx1)

        for (i in 2:(p - min.n.var)) {
          stepgbm1 <- NULL
          stepgbm1 <- stepgbmRVI(trainx = trainx1, trainy = trainy, family = family, n.trees = n.trees, learning.rate = learning.rate, interaction.depth = interaction.depth, bag.fraction = bag.fraction, train.fraction = train.fraction, n.minobsinnode = n.minobsinnode, cv.fold = cv.fold, weights = weights, keep.data = keep.data, verbose = verbose, n.cores = n.cores, rpt = rpt, predacc = predacc, min.n.var = min.n.var, rseed = rseed)

          stepgbmRVI1[[i]] <- stepgbm1

          predictors.selected <- steprf::steprfAVIPredictors(stepgbm1, trainx = trainx1)
          stepgbmRVIPredictors1[[i]] <- predictors.selected
          predictive.accuracy2 <- predictors.selected$max.predictive.accuracy
          max.predictive.accuracy[i] <- predictive.accuracy2
          kirvi.predictors[[i]] <- predictors.selected$PABV

          if ((predictive.accuracy2 - predictive.accuracy1) <= delta.predacc)
          {break
          } else
          {predictive.accuracy1 <- predictive.accuracy2
          trainx1 <- trainx[, kirvi.predictors[[i]], drop = FALSE]
          }
        }
      } else (
        stop ("This variable selection method is not supported!")
      )))
  list(stepgbmPredictorsFinal = predictors.selected, max.predictive.accuracy = max.predictive.accuracy, numberruns = i, laststepRVI = stepgbm1, stepgbmRVIOutputsAll = stepgbmRVI1, stepgbmPredictorsAll = stepgbmRVIPredictors1, KIRVIPredictorsAll = kirvi.predictors)
}

