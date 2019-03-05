#' Basic statistics to evaluate sample and model parameters from a Naive Bayes model matrix.
#'
#' @description This function takes a \code{SpNaBaMatrix} or \code{SpNaBaEps}
#'     object and extract basic statistics about sample and parameters epsilon and score.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @name smp_stats
#' @rdname smp_stats-methods
#' @exportMethod smp_stats

setGeneric("smp_stats", function(x) standardGeneric("smp_stats"))


#' @rdname smp_stats-methods
#' @aliases smp_stats,SpNaBaMatrix-method
#'
#' @param x A \code{\link[rspecies]{SpNaBaMatrix}}, \code{\link[rspecies]{SpNaBaEps}},
#'     \code{\link[rspecies]{SpNaBaScore}} or \code{\link[rspecies]{SpNaBaModel}} object.
#'
#' @return An object of class \code{\link[data.table]{data.table}}.
#'
#' @details
#'     \itemize{
#'       \item{SpNaBaMatrix:}{Return a data.table with the follow structure.}
#'       \itemize{
#'         \item{Factor:}{ The name of the factors included in the model.}
#'         \item{N:}{ Number of blocks in the partition of the sample space. If the model is not
#'           for spatial data N represents the number of cases in the model. If the model is
#'           for spatial data N represents the number of cell in the GRID
#'           (see \code{\link[rspecies]{grd_build}}).}
#'         \item{Nw:}{ Number of blocks with at least one observation of factor \eqn{w}. If the model
#'           is not for spatial data Nw represents the number of cases with presences of factor
#'           \eqn{w}. If the model is for spatial data Nw represents the number of cell in the GRID
#'           with at least one obervations of the factor \eqn{w}.}
#'         \item{minObs:}{ the minimum number of observatios per case of factor \eqn{w}. If the model
#'           is not for spatial data minObs = 1, i.e. one observation of factor \eqn{w} for any case
#'           with a presence of factor \eqn{w}. If the model is for spatial data minObs represents
#'           the minimum number of observations of factor \eqn{w} of all cells with at least one
#'           observations of factor \eqn{w}.}
#'         \item{medianObs:}{ the median of the number of observatios per case of factor \eqn{w}. If
#'           the model is not for spatial data medianObs = 1, i.e. one observation of factor \eqn{w}
#'           for any case with a presence of factor \eqn{w}. If the model is for spatial data
#'           medianObs represents the median of the number of observations of factor \eqn{w} of all
#'           cells with at least one observations of factor \eqn{w}.}
#'         \item{maxObs:}{ the maximum number of observatios per case of factor \eqn{w}. If the model
#'           is not for spatial data maxObs = 1, i.e. one observation of factor \eqn{w} for any case
#'           with a presence of factor \eqn{w}. If the model is for spatial data maxObs represents
#'           the maximum number of observations of factor \eqn{w} of all cells with at least one
#'           observations of factor \eqn{w}.}
#'         \item{TotalObs:}{ the total number of observatios of factor \eqn{w}. If the model is not
#'           for spatial data TotalObs = Nw, i.e. the total number of observations of factor
#'           \eqn{w} is equal to the number of cases with a presences of factor \eqn{w}. If the model is
#'           for spatial data TotalObs represents the total number of observations of factor \eqn{w}.}
#'         }
#'
#'
#'     \item{SpNaBaEps and SpNaBaScore:}{A List of two data.table with the follow structure.
#'           The first element of the list maps the epsilon of target variables.
#'           The second element of the list maps the epsilon of predictor variables.}
#'       \itemize{
#'           \item{Factor:}{ The name of the factors included in the model.}
#'           \item{Mean:}{ The mean of epsilon.}
#'           \item{Variance:}{ The Variance of epsilon.}
#'           \item{Std.Dev:}{ The Std.Dev of epsilon.}
#'           \item{Lower:}{ The 25\% quantile of epsilon.}
#'           \item{Median:}{ The Median of epsilon.}
#'           \item{Upper:}{ The 75\% quantile of epsilon.}
#'           \item{Max:}{ The Max of epsilon.}
#'         }
#'
#'       \item{SpNaBaModel:}{A List of 3 dobject. Each one with the descriptions of
#'           the function applied to the objects described below.
#'         }
#'   }
#'
#' @seealso \code{SpNaBaMatrix}, \code{grd_build}, \code{id_pts}
#'
#' @examples
#' # Using the function id_pts() for spatial data --------
#'
#' library(sp)
#' library(rgeos)
#' library(rgdal)
#' library(raster)
#'
#' # Loading data
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' smp_stats(x.mat)
#'
#' @usage NULL
#' @import data.table
#' @importFrom stats median

setMethod("smp_stats", "SpNaBaMatrix", function(x){

  # Initial calculation -------------------------------

  a <- data.table::data.table(x@freqmatrix)

  # Checking proportion of occupied cells -------------

  output <- t(a[,lapply(.SD, function(x){
    list(N = length(x),
         Nw = sum((x>0)),
         minObs = min(x[x > 0], na.rm = TRUE),
         medianObs = stats::median(x[x > 0], na.rm = TRUE),
         maxObs = max(x[x > 0], na.rm = TRUE),
         TotalObs = sum(x[x > 0], na.rm = TRUE))
    # return(output)
  }), .SDcols = colnames(a)])

  colnames(output) <- c("N", "Nw", "minObs", "medianObs",
                        "maxObs", "TotalObs")

  output <- sapply(as.data.frame(output), unlist)

  output <- data.table::data.table(output, keep.rownames = TRUE)
  names(output)[1] <- "Factor"

  output <- output[order(output$Nw, output$maxObs, decreasing = FALSE),]
  return(output)

}
)





#' @rdname smp_stats-methods
#' @aliases smp_stats,SpNaBaEps-method
#'
#' @seealso \code{SpNaBaEps}, \code{epsilon}
#'
#' @examples
#' # Checking stats for epsilon measure --------
#'
#' # Counts calculation ----------------------------------
#'
#' x.counts <- counts(x.mat, target = 1:10)
#'
#' # Probability calculation -----------------------------
#'
#' x.probs <- probs(x.counts)
#'
#' # Estimating epsilon ----------------------------------
#'
#' x.eps <- epsilon(x.counts, x.probs)
#'
#' # Statistics
#'
#' smp_stats(x.eps)
#'
#' @usage NULL
#'
#' @import data.table
#' @importFrom stats median quantile sd var

setMethod("smp_stats", "SpNaBaEps", function(x){

  # Initial calculation -------------------------------

  a <- data.table::data.table(x@Ecx)
  t.a <- data.table::data.table(t(x@Ecx))

  # Checking stats for target variables -------------

  output1 <- t(t.a[,lapply(.SD, function(x){
    list(Mean = mean(x, na.rm = TRUE),
         Var = var(x, na.rm = TRUE),
         Std.Dev = sd(x, na.rm = TRUE),
         Min  = min(x, na.rm = TRUE),
         Lower = quantile(x, probs = 0.25, na.rm = TRUE),
         Median = quantile(x, probs = 0.5, na.rm = TRUE),
         Upper = quantile(x, probs = 0.75, na.rm = TRUE),
         Max = max(x, na.rm = TRUE))
    # return(output)
  }), .SDcols = colnames(t.a)])

  colnames(output1) <- c("Mean", "Var", "Std.Dev", "Min", "Lower", "Median", "Upper", "Max")

  output1 <- sapply(as.data.frame(output1), unlist)

  output1 <- data.table::data.table(output1, keep.rownames = TRUE)
  names(output1)[1] <- "Factor"

  output1 <- output1[order(output1$Mean, decreasing = FALSE),]


  # Checking stats for predictors variables -----------


  output2 <- t(a[,lapply(.SD, function(x){
    list(Mean = mean(x, na.rm = TRUE),
         Var = var(x, na.rm = TRUE),
         Std.Dev = sd(x, na.rm = TRUE),
         Min  = min(x, na.rm = TRUE),
         Lower = quantile(x, probs = 0.25, na.rm = TRUE),
         Median = quantile(x, probs = 0.5, na.rm = TRUE),
         Upper = quantile(x, probs = 0.75, na.rm = TRUE),
         Max = max(x, na.rm = TRUE))
    # return(output)
  }), .SDcols = colnames(a)])

  colnames(output2) <- c("Mean", "Var", "Std.Dev", "Min", "Lower", "Median", "Upper", "Max")

  output2 <- sapply(as.data.frame(output2), unlist)

  output2 <- data.table::data.table(output2, keep.rownames = TRUE)
  names(output2)[1] <- "Factor"

  output2 <- output2[order(output2$Mean, decreasing = FALSE),]


  output0 <- list(Target = output1, Predictors = output2)

  return(output0)

}
)




#' @rdname smp_stats-methods
#' @aliases smp_stats,SpNaBaScore-method
#'
#' @seealso \code{SpNaBaScore}, \code{score}
#'
#' @examples
#' # Checking stats for score measure --------
#'
#' # Score parameter -----------------------------------
#'
#' x.score <- score(x.probs)
#'
#' # Statistics
#'
#' smp_stats(x.score)
#'
#' @usage NULL
#'
#' @import data.table
#' @importFrom stats median quantile sd var

setMethod("smp_stats", "SpNaBaScore", function(x){

  # Initial calculation -------------------------------

  a <- data.table::data.table(x@Scx)
  t.a <- data.table::data.table(t(x@Scx))



  # Checking stats for target variables -------------

  output1 <- t(a[,lapply(.SD, function(x){
    list(Mean = mean(x, na.rm = TRUE),
         Var = var(x, na.rm = TRUE),
         Std.Dev = sd(x, na.rm = TRUE),
         Min  = min(x, na.rm = TRUE),
         Lower = quantile(x, probs = 0.25, na.rm = TRUE),
         Median = quantile(x, probs = 0.5, na.rm = TRUE),
         Upper = quantile(x, probs = 0.75, na.rm = TRUE),
         Max = max(x, na.rm = TRUE))
    # return(output)
  }), .SDcols = colnames(a)])

  colnames(output1) <- c("Mean", "Var", "Std.Dev", "Min", "Lower", "Median", "Upper", "Max")

  output1 <- sapply(as.data.frame(output1), unlist)

  output1 <- data.table::data.table(output1, keep.rownames = TRUE)
  names(output1)[1] <- "Factor"

  output1 <- output1[order(output1$Mean, decreasing = FALSE),]


  # Checking stats for predictors variables -----------


  output2 <- t(t.a[,lapply(.SD, function(x){
    list(Mean = mean(x, na.rm = TRUE),
         Var = var(x, na.rm = TRUE),
         Std.Dev = sd(x, na.rm = TRUE),
         Min  = min(x, na.rm = TRUE),
         Lower = quantile(x, probs = 0.25, na.rm = TRUE),
         Median = quantile(x, probs = 0.5, na.rm = TRUE),
         Upper = quantile(x, probs = 0.75, na.rm = TRUE),
         Max = max(x, na.rm = TRUE))
    # return(output)
  }), .SDcols = colnames(t.a)])

  colnames(output2) <- c("Mean", "Var", "Std.Dev", "Min", "Lower", "Median", "Upper", "Max")

  output2 <- sapply(as.data.frame(output2), unlist)

  output2 <- data.table::data.table(output2, keep.rownames = TRUE)
  names(output2)[1] <- "Factor"

  output2 <- output2[order(output2$Mean, decreasing = FALSE),]


  output0 <- list(Target = output1, Predictors = output2)

  return(output0)

}
)






#' @rdname smp_stats-methods
#' @aliases smp_stats,SpNaBaScore-method
#'
#' @seealso \code{SpNaBaModel}, \code{NBModel}
#'
#' @examples
#' # Fitting model --------
#'
#' x.model <- NBModel(x.mat, target = 1:10, fac.lap = 0.01)
#'
#' # Statistics
#'
#' smp_stats(x.model)
#'
#' @usage NULL
#'
#' @import data.table
#' @importFrom stats median quantile sd var

setMethod("smp_stats", "SpNaBaModel", function(x){

    # Sample Stats ----------------------------

     stats_smp <- smp_stats(x@ModMatrix)

    # Epsilon Stats ---------------------------

     stats_eps <- smp_stats(x@Epsilon)

    # Score Stats -----------------------------

     stats_score <- smp_stats(x@Score)

    # Creating output object ------------------

     output <- list(stats_smp = stats_smp, stats_eps = stats_eps,
                    stats_score = stats_score)

    # Returning output ------------------------

     return(output)
}
)
