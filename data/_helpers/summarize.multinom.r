summarize.multinom <- function (some.multinom.model) {
   temp <- summary(some.multinom.model)
   temp.zscores <- temp$coefficients / temp$standard.errors # or temp$$Wald.ratios
   temp.pvalues <- 2*pnorm(abs(temp.zscores), lower.tail=FALSE)
   temp.confint <- confint(some.multinom.model) # to later add CIs

   output <- list()
   output[["Coefficients"]] <- data.frame(
      "Estimate"=as.numeric(temp$coefficients),
      "Std.Error"=as.numeric(temp$standard.errors),
      "zvalues"=as.numeric(temp.zscores),
      "Pvalues"=as.numeric(temp.pvalues))
      if ("matrix" %in% class(temp$coefficients)) {
         row.names(output[["Coefficients"]]) <- paste(
            rep(colnames(temp$coefficients), each=nrow(temp$coefficients)),
            rep(rownames(temp$coefficients), ncol(temp$coefficients)),
            sep=":")
      } else {
         row.names(output[["Coefficients"]]) <- names(temp$coefficients)
      }
   output[["R2"]] <- R2(some.multinom.model)
   # output[["Accuracy"]] <- mean(predict(some.multinom.model)==some.multinom.model$model[,1])
   # the accuracy computation I have not fully tested yet, that's why it's commented out
   temp <- anova(update(some.multinom.model, ~1), some.multinom.model, test="Chisq")[2,]
   output[["Model significance test"]] <- c(
      "LR statistic"=temp[1,6], "Df"=temp[1,5],
      "P-value"=pchisq(temp[1,6], temp[1,5], lower.tail=FALSE))
   return(output)
}
