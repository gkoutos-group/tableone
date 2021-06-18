
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

theme_set(theme_sjplot())

.models_loaded <- function()

multivariate_model <- function(data, outcome, variables, weighted=FALSE, positive_class = '1') {
  data[[outcome]] <- as.factor(as.numeric(data[[outcome]] == positive_class))
  
  if(weighted) {
    weights <- 1 + (as.numeric(as.character(data[[outcome]])) * (as.numeric(nrow(data) / table(data[[outcome]])['1'] - 1)))
  } else {
    weights <- NULL
  }
  
  f2 <- as.formula(paste(outcome, paste(variables, 
                                        collapse=' + '), 
                         sep=' ~ '))
  m2 <- glm(f2, 
            data=data,
            weights=weights,
            family = quasibinomial(link = "logit"))
  return(m2)
}


table_multivariate_model <- function(m2_summary) {
  base <- as.data.frame(summary(m2_summary)$coefficients)
  base$names <- row.names(base)
  base$coefficients <- base$Estimate
  base$OR <- exp(base$Estimate)
  base$Estimate <- NULL
  
  cis <- as.data.frame(confint(m2_summary, level=0.95))
  colnames(cis) <- c('OR_low', 'OR_high')
  cis$names <- row.names(cis)
  cis$OR_low <- exp(cis$OR_low)
  cis$OR_high <- exp(cis$OR_high)
  
  complete <- merge(base,
                    cis,
                    on='names')
  
  # for use in excel
  complete$X_label_pos <- 0.6
  complete$Y_pos <- nrow(complete):1
  complete$low_diff <- complete$OR - complete$OR_low
  complete$high_diff <- complete$OR_high - complete$OR
  # for use in excel
  
  first_cols <- c('names', 'coefficients')
  return(complete[, c(first_cols, setdiff(colnames(complete), first_cols))])
}


plot_multivariate_odds <- function(model, show.values = FALSE, bpe.style='dot', bpe='mean', prob.inner=NULL, prob.outer=NULL) {
  plot_model(model,
             show.values = show.values,
             ci.lvl=0.95,
             bpe.style = bpe.style,
             bpe = bpe,
             prob.inner = prob.inner,
             prob.outer = prob.outer)
}


univariate_table <- function(data, outcome, variables, weighted=FALSE, positive_class = '1') {
  data[[outcome]] <- as.factor(as.numeric(data[[outcome]] == positive_class))
  
  if(weighted) {
    weights <- 1 + (as.numeric(as.character(data[[outcome]])) * (as.numeric(nrow(data) / table(data[[outcome]])['1'] - 1)))
  } else {
    weights <- NULL
  }
  
  vars <- vector()
  coefs <- vector()
  stderrs <- vector()
  zvals <- vector()
  pvals <- vector()
  ors <- vector()
  or_lows <- vector()
  or_highs <- vector()
  
  for(a in variables) {
    print(a)
    tryCatch({
      ff <- as.formula(paste0(outcome, ' ~ ', a))
      
      m <- glm(ff,
               data=data,
               weights=weights,
               family=quasibinomial(link='logit'))
      
      sm <- summary(m)

      name <- row.names(as.data.frame(sm$coefficients))[2]
      coef <- sm$coefficients[2,1]
      stderr <- sm$coefficients[2,2]
      zval <- sm$coefficients[2,3]
      pval <- sm$coefficients[2,4]
      
      or <- exp(coef)
      cint <- confint(m, level=0.95)
      or_low <- exp(cint[2,1])
      or_high <- exp(cint[2,2])
      
      vars <- append(vars, name)
      coefs <- append(coefs, coef)
      stderrs <- append(stderrs, stderr)
      zvals <- append(zvals, zval)
      pvals <- append(pvals, pval)
      ors <- append(ors, or)
      or_lows <- append(or_lows, or_low)
      or_highs <- append(or_highs, or_high)
    }, error=function(cond){
      print(paste('error is on:', cond))
      vars <- append(vars, a)
      coefs <- append(coefs, NA)
      stderrs <- append(stderrs, NA)
      zvals <- append(zvals, NA)
      pvals <- append(pvals, NA)
      ors <- append(ors, NA)
      or_lows <- append(or_lows, NA)
      or_highs <- append(or_highs, NA)
    })
  }
  ll <- list(variable=vars, coefficient=coefs, stderr=stderrs, zval=zvals, pval=pvals, OR=ors, OR_low=or_lows, OR_high=or_highs)
  return(as.data.frame(ll))
}

