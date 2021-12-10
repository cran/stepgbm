#' library(spm)
#'
#' data(petrel)
#' stepgbm1 <- stepgbm(trainx = petrel[, c(1,2, 6:9)], trainy =
#' log(petrel[, 5] + 1), method = "KIRVI", family = "gaussian", rpt = 2,
#' predacc = "VEcv", learning.rate = 0.1, n.trees = 20, cv.fold = 5,  min.n.var = 2,
#' n.cores = 2, delta.predacc = 0.1, rseed = 1234)
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
#' family = "poisson", rpt = 2, learning.rate = 0.1, n.trees = 20, cv.fold = 5,
#' predacc = "VEcv", min.n.var = 2, n.cores = 2, delta.predacc = 0.1, rseed = 1234)
#' stepgbm2
#' stepgbm2$max.predictive.accuracy
#'
#' # The variables selected can be derived with
#' stepgbm2$stepgbmPredictorsAll[[1]]$variables.most.accurate
