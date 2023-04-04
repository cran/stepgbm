#' @title Select predictive variables for generalized boosted regression modeling (gbm)
#' by relative variable influence (rvi) and accuracy in a stepwise algorithm
#'
#' @description This function is to select predictive variables for generalized
#' boosted regression modeling (gbm) by their relative variable influence that is
#' calculated for each model after excluding the least influence variable, and
#' corresponding predictive accuracy. It is also developed for 'stepgbm' function.
#'
#' @param trainx a dataframe or matrix contains columns of predictive variables.
#' @param trainy a vector of response, must have length equal to the number of
#' rows in trainx.
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
#' expansion. Also known as step-size reduction.
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
#' @param rseed random seed. By default, 1234 is used.
#' @param ... other arguments passed on to gbm.
#'
#' @return A list with the following components:
#' variable removed based on avi (variable.removed), averaged predictive accuracy
#' of the model after excluding variable.removed (predictive.accuracy),
#' contribution to accuracy by each variable.removed (delta.accuracy), and
#' predictive accuracy matrix of the model after excluding variable.removed for
#' each iteration (predictive.accuracy2)
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
#' Chang, W. 2021. Cookbook for R. http://www.cookbook-r.com/.
#'
#' @author Jin Li
#' @examples
#' \donttest{
#' library(spm)
#' data(petrel)
#' stepgbm1 <- stepgbmRVI(trainx = petrel[, c(1,2, 6:9)], trainy = log(petrel[, 5] + 1),
#'  cv.fold = 5, min.n.var = 2, n.cores = 2, rseed = 1234)
#' stepgbm1
#'
#' #plot stepgbm1 results
#' library(reshape2)
#' pa1 <- as.data.frame(stepgbm1$predictive.accuracy2)
#' names(pa1) <- stepgbm1$variable.removed
#' pa2 <- melt(pa1, id = NULL)
#' names(pa2) <- c("Variable","VEcv")
#' library(lattice)
#' with(pa2, boxplot(VEcv~Variable, ylab="VEcv (%)", xlab="Predictive variable removed"))
#'
#' barplot(stepgbm1$delta.accuracy, col = (1:length(stepgbm1$variable.removed)),
#' names.arg = stepgbm1$variable.removed, main = "Predictive accuracy vs variable removed",
#' font.main = 4, cex.names=1, font=2, ylab="Increase rate in VEcv (%)")
#'
#' # Extract names of the selected predictive variables by stepgbm
#' library(steprf)
#' steprfAVIPredictors(stepgbm1, trainx = petrel[, c(1,2, 6:9)])
#'
#' data(sponge)
#' set.seed(1234)
#' stepgbm2 <- stepgbmRVI(trainx = sponge[, -3], trainy = sponge[, 3],
#' family = "poisson", cv.fold = 5, min.n.var = 2, n.cores = 2)
#' stepgbm2
#'
#' #plot stepgbm2 results
#' library(reshape2)
#' pa1 <- as.data.frame(stepgbm2$predictive.accuracy2)
#' names(pa1) <- stepgbm2$variable.removed
#' pa2 <- melt(pa1, id = NULL)
#' names(pa2) <- c("Variable","VEcv")
#' library(lattice)
#' with(pa2, boxplot(VEcv~Variable, ylab="VEcv (%)", xlab="Predictive variable removed"))
#'
#' barplot(stepgbm2$delta.accuracy, col = (1:length(stepgbm2$variable.removed)),
#' names.arg = stepgbm2$variable.removed, main = "Predictive accuracy vs variable removed",
#' font.main = 4, cex.names=1, font=2, ylab="Increase rate in VEcv (%)")
#'
#' # Extract names of the selected predictive variables by stepgbm
#' steprfAVIPredictors(stepgbm2, trainx =  sponge[, -3])
#' }
#'
#' @export
stepgbmRVI <- function (trainx, trainy,
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
                     rseed = 1234, ...) {
  n <- nrow(trainx); p <- ncol(trainx)
  impvar <- NULL; rmvar2 <- NULL
  predictive.accuracy <- NULL; delta.accuracy <- NULL
  predictive.accuracy2 <- array(NA,dim=c(rpt, (p - min.n.var)))
  gbmcv1 <- NULL

  # Saving the state of the RNG from Chang
  if (exists(".Random.seed", .GlobalEnv)) oldseed <- .GlobalEnv$.Random.seed
  else oldseed <- NULL

  set.seed(rseed)
  for (j in 1:rpt) {
  gbmcv1[j] <- spm::gbmcv(trainx, trainy,
                    family = family,
                    n.trees = n.trees,
                    learning.rate = learning.rate,
                    interaction.depth = interaction.depth,
                    bag.fraction = bag.fraction,
                    train.fraction =  train.fraction,
                    n.minobsinnode = n.minobsinnode,
                    cv.fold = cv.fold,
                    n.cores= n.cores,
                    predacc = predacc)
  }
  predictive.accuracy1 <- mean(gbmcv1)
  rvi1 <- spm::rvi(trainx, trainy, family = family,
              n.trees = n.trees,
              learning.rate = learning.rate,
              interaction.depth = interaction.depth,
              bag.fraction = bag.fraction,
              train.fraction =  train.fraction,
              n.minobsinnode = n.minobsinnode,
              cv.fold = cv.fold,
              n.cores = n.cores)
  #rmvar <- rvi1$impvar[p]
  rmvar <- which(names(trainx) == as.character(rvi1$gbm.rvi$var[p]))
  #rmvar2[1] <- rvi1$gbm.rvi$var[p]
  rmvar2[1] <- as.character(rvi1$gbm.rvi$var[p])
  trainx1 <- trainx[, -rmvar]

  for (i in 1:(p - min.n.var)) {
    gbmcv1 <- NULL
    set.seed(rseed)
  for (j in 1:rpt) {
    gbmcv1[j] <- spm::gbmcv(trainx1, trainy,
                       family = family,
                       n.trees = n.trees,
                       learning.rate = learning.rate,
                       interaction.depth = interaction.depth,
                       bag.fraction = bag.fraction,
                       train.fraction =  train.fraction,
                       n.minobsinnode = n.minobsinnode,
                       cv.fold = cv.fold,
                       n.cores= n.cores,
                       predacc = predacc)
  }
  predictive.accuracy [i] <- mean(gbmcv1)
  delta.accuracy[i] <- predictive.accuracy1 - predictive.accuracy [i]
  predictive.accuracy1 <- predictive.accuracy [i]
  predictive.accuracy2[ , i] <- gbmcv1
  if (i == p - min.n.var)
    { break
    } else
    {
      rvi1 <- spm::rvi(trainx1, trainy, n.trees = n.trees, family = family,
                  learning.rate = learning.rate,
                  interaction.depth = interaction.depth,
                  bag.fraction = bag.fraction,
                  train.fraction =  train.fraction,
                  n.minobsinnode = n.minobsinnode,
                  cv.fold = cv.fold,
                  n.cores = n.cores)
    #rmvar <- rvi1$impvar[p-i]
    rmvar <- which(names(trainx1) == as.character(rvi1$gbm.rvi$var[p-i]))
    #rmvar2[1+i] <- rvi1$gbm.rvi$var[p-i]
    rmvar2[1+i] <- as.character(rvi1$gbm.rvi$var[p-i])
    trainx1 <- trainx1[, -rmvar, drop = FALSE]
    }
  }

  # Restoring the state of the RNG from Chang
  if (!is.null(oldseed)) .GlobalEnv$.Random.seed <- oldseed
  else rm(".Random.seed", envir = .GlobalEnv)

  #rmvar2 <- rmvar2 [- length(rmvar2)]
  #rmvar3 <- names(trainx[, rmvar2])
  list(variable.removed = rmvar2, predictive.accuracy = predictive.accuracy,
  delta.accuracy = delta.accuracy,  predictive.accuracy2 =  predictive.accuracy2)
}
