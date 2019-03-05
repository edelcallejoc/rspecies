
#' An S4 method for SpNaBaModel class object.
#'
#' @name plotNB
#' @rdname plot-NBmodel
#'
#' @description A plot methods for class "SpNaBaModel".
#'
#' @param nbmodel A \code{SpNaBaModel} object from
#'     \code{\link[rspecies]{NBModel}} function.
#'
#' @param ... Parameters passed to other functions (See
#'     \code{\link[graphics]{par}}).
#'
#' @details This function creates a list with five ggplot objects.
#'
#'          The first element of the list is called 'Smp_density'.
#'          It shows an histogram of the density of cell's occurrence
#'          per specie. In this sense, the x axis of this graph represents
#'          the number of cells with an occurrence and the y axis shows
#'          the density of variables (species) in that range of cell's
#'          occurrence.
#'
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @exportMethod plotNB
#' @import ggplot2
#'
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R SpNaBaEps.R
#' @include SpNaBaScore.R SpNaBaModel.R NBModel.R SpNaBaPred.R predict.R
#'
#'
#'

setGeneric("plotNB", function(nbmodel, ...) standardGeneric("plotNB"))

#' @rdname plot-NBmodel
#' @aliases plotNB,SpNaBa-methods,NBModel-methods
#' @usage NULL
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals with colnames = NULL.
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' # Creando el modelo
#' x.model <- NBModel(x.mat, target = 1:10, fac.lap = 0.01)
#'
#' # Graphic report
#' x.plots <- plotNB(x.model)
#'
#' @importFrom RColorBrewer brewer.pal
#' @import grid gridExtra ggrepel
#'

setMethod("plotNB", c("SpNaBaModel"),
          function(nbmodel, ...) {

          # global Variables

            Mean <- NULL
            Factor <- NULL
            Std.Dev <- NULL

          # samp_stats object

            sum_model <- smp_stats(nbmodel)

          # Graph of sample intesity
            ncells <- get_N(nbmodel)
            nvars <- length(sum_model$stats_smp$Factor)
            max_Nw <- max(sum_model$stats_smp$Nw)
            binwidth <- ceiling(max_Nw/10)


            p1 <- ggplot(sum_model$stats_smp, aes_string(x='Nw')) +
                   geom_histogram(aes_string(y="..density.."), colour="black", fill="white",
                             binwidth =  binwidth,
                             boundary = 0.5) +
                   geom_density(alpha=.2, fill="#FF6666") +
                   labs(title = "Sample intensity",
                        subtitle = paste("Nvars:", nvars, "- Ncells:", ncells)) +
                   scale_x_continuous(breaks = c(seq(0, max_Nw, by = binwidth),
                                                 max_Nw))

           # Graph of epsilon stats

            # Target
            eps_target <- sum_model$stats_eps$Target[order(Mean, decreasing = TRUE),]
            eps_target[, Factor := factor(Factor, levels = Factor)]
            eps_target[, ':='(Mean_plusSD = Mean + Std.Dev,
                              Mean_minusSD = Mean - Std.Dev)]
            target_names<- levels(eps_target$Factor)
            ntarget <- length(target_names)

            p2 <- ggplot(eps_target,
                         aes_string(x = "Factor", y = "Mean", group = 1, label = "Factor")) +
              geom_ribbon(aes_string(ymin = "Mean_minusSD", ymax = "Mean_plusSD"),
                          fill = "#66a3ff", alpha = 0.6) +
              geom_line(color = "#004080") + geom_point(color = "#004080") +
              geom_hline(yintercept = 0, color = "red", linetype="dashed") +
              theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
                    axis.ticks.x=element_blank()) +
              labs(title = "Target")

            if(ntarget > 10){
              target_names[6:(ntarget-5)] <- NA
            }

            p2 <- p2 + geom_text_repel(data = subset(eps_target, !is.na(target_names)),
                                       direction = "x", angle = 90,
                                 vjust = -0.5, hjust = -1)

            # Predictors
            eps_predictor <- sum_model$stats_eps$Predictors[order(Mean, decreasing = TRUE),]
            eps_predictor[, Factor := factor(Factor, levels = Factor)]
            eps_predictor[, ':='(Mean_plusSD = Mean + Std.Dev,
                                 Mean_minusSD = Mean - Std.Dev)]
            predictor_names<- levels(eps_predictor$Factor)
            npredictors <- length(predictor_names)

            p3 <- ggplot(eps_predictor,
                         aes_string(x = "Factor", y = "Mean", group = 1, label = "Factor")) +
              geom_ribbon(aes_string(ymin = "Mean_minusSD", ymax = "Mean_plusSD"),
                          fill = "#66a3ff", alpha = 0.6) +
              geom_line(color = "#004080") + geom_point(color = "#004080") +
              geom_hline(yintercept = 0, color = "red", linetype="dashed") +
              theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
                    axis.ticks.x=element_blank()) +
              labs(title = "Predictor")

            if(npredictors > 10){
              predictor_names[6:(npredictors-5)] <- NA
            }

            p3 <- p3 + geom_text_repel(data = subset(eps_predictor, !is.na(predictor_names)),
                                       direction = "x", angle = 90,
                                       vjust = -0.5, hjust = -1)


            # Grouping Epsilon plos with grid.arrange

            p4 <- gridExtra::grid.arrange(p2, p3, nrow = 2,
                                          top = textGrob("Epsilon",gp=gpar(fontsize=20,font=3)))


            # Graph of Score stats

            # Target
            score_target <- sum_model$stats_score$Target[order(Mean, decreasing = TRUE),]
            score_target[, Factor := factor(Factor, levels = Factor)]
            score_target[, ':='(Mean_plusSD = Mean + Std.Dev,
                                Mean_minusSD = Mean - Std.Dev)]

            p5 <- ggplot(score_target,
                         aes_string(x = "Factor", y = "Mean", group = 1, label = "Factor")) +
              geom_ribbon(aes_string(ymin = "Mean_minusSD", ymax = "Mean_plusSD"),
                          fill = "#66a3ff", alpha = 0.6) +
              geom_line(color = "#004080") + geom_point(color = "#004080") +
              geom_hline(yintercept = 0, color = "red", linetype="dashed") +
              theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
                    axis.ticks.x=element_blank()) +
              labs(title = "Target")

            p5 <- p5 + geom_text_repel(data = subset(score_target, !is.na(target_names)),
                                       direction = "x", angle = 90,
                                       vjust = -0.5, hjust = -1)

            # Predictors
            score_predictor <- sum_model$stats_score$Predictors[order(Mean, decreasing = TRUE),]
            score_predictor[, Factor := factor(Factor, levels = Factor)]
            score_predictor[, ':='(Mean_plusSD = Mean + Std.Dev,
                                   Mean_minusSD = Mean - Std.Dev)]

            p6 <- ggplot(score_predictor,
                         aes_string(x = "Factor", y = "Mean", group = 1, label = "Factor")) +
              geom_ribbon(aes_string(ymin = "Mean_minusSD", ymax = "Mean_plusSD"),
                          fill = "#66a3ff", alpha = 0.6) +
              geom_line(color = "#004080") + geom_point(color = "#004080") +
              geom_hline(yintercept = 0, color = "red", linetype="dashed") +
              theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
                    axis.ticks.x=element_blank()) +
              labs(title = "Predictor")


            p6 <- p6 + geom_text_repel(data = subset(score_predictor, !is.na(predictor_names)),
                                       direction = "x", angle = 90,
                                       vjust = -0.5, hjust = -1)

            # Grouping Score plos with grid.arrange

            p7 <- gridExtra::grid.arrange(p5, p6, nrow = 2,
                                          top = textGrob("Score",gp=gpar(fontsize=20,font=3)))


           # Printing the graphs

            print(p1)
            print(p4)
            print(p7)

           # Returnin the output object

            output <- list(Target_Eps = eps_target,
                           Predictor_Eps = eps_predictor,
                           Target_Score = score_target,
                           Predictor_score = score_predictor)

            return(invisible(output))

})

