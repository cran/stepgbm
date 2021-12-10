#' library(spm)
#' data(petrel)
#' stepgbm1 <- stepgbmRVI(trainx = petrel[, c(1,2, 6:9)], trainy = log(petrel[, 5] + 1),
#' learning.rate = 0.1, n.trees = 20, cv.fold = 5, min.n.var = 2, n.cores = 2, rseed = 1234)
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
#' family = "poisson", learning.rate = 0.1, n.trees = 20, cv.fold = 5, min.n.var = 2, n.cores = 2)
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
