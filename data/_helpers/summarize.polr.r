summarize.polr <- function (some.polr.model) {
   library(car)
   temp <- summary(some.polr.model)
   temp.pvalues <- 2*pnorm(abs(temp$coefficients[,"t value"]), lower.tail=FALSE)
   temp.est.and.confint <- Confint(some.polr.model) # here we need the package car again

   output <- list()
   output[["Coefficients"]] <- data.frame(
      temp.est.and.confint,
      temp$coefficients[,c("Std. Error","t value")],
      "Pvalues"=temp.pvalues,
      row.names=rownames(temp.est.and.confint))
   colnames(output[["Coefficients"]]) <- c("Estimate", "95%-CIlwr", "95%-CIupp", "Std. Error", "z", "p2tailed")

   output[["R2"]] <- R2(some.polr.model)

   temp <- anova(update(some.polr.model, ~1), some.polr.model, test="Chisq")[2,]
   output[["Model significance test"]] <- c(
      "LR statistic"=temp[1,6], "Df"=temp[1,5],
      "P-value"=pchisq(temp[1,6], temp[1,5], lower.tail=FALSE))
   return(output)
}
