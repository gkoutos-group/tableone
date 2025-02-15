
library(xlsx)
library(reshape)
library(reshape2)
library(tidyverse)
library(nortest)

#### to check if things are loaded
.describe_table_loaded <- function() {
  return(TRUE)
}

## infer column type - check matchmatch to be sure using the same version
infer_column_type <- function(df, c, ratio_factor, ratio_numeric) {
  n_op <- length(unique(df[[c]]))
  
  if(n_op <= nrow(df) * ratio_factor) {
    if(n_op == 2) {
      return("binary")
    } else {
      return("factor")
    }
  } else if(n_op == nrow(df)) {
    return("skip")
  } else if(n_op >= nrow(df) * ratio_numeric) {
    return("numeric")
  }
  return("skip")
}

# check for missing columns
check_columns_dataset <- function(df, columns_to_test) {
  invalids <- vector()
  for(i in columns_to_test) {
    if(length(intersect(c(i), colnames(df))) == 0) {
      invalids <- append(invalids, i)
    }
  }
  if(length(invalids) > 0) {
    stop(paste0('Missing dataset columns: "', paste(invalids, collapse='", "'), '"'))
  }
}

#####################
# auxiliary table with fisher test on the categorical variables
table_cat_pval <- function(df, columns_to_test, classvar='predclass', verbose=FALSE, round_digits=8) {
  check_columns_dataset(df, c(columns_to_test, classvar))
  
  condition <- vector()
  pval <- vector()
  #c_to_test <- append(colnames(df)[grepl('comorbidity.', colnames(df))], c('clinical.female', 'other.smoker', 'other.ever_smoked', 'other.spell_death'))
  miss <- vector()
  c_to_test <- columns_to_test
  for(i in c_to_test) {
    if(verbose) {
      print(i)
    }
    tbl <- table(df[[classvar]], df[[i]])
    chi <- NULL
    tryCatch({
      chi <- fisher.test(tbl, simulate.p.value=T)
    }, error = function(e) {
      print(paste('fisher.test failed for', i))
    })
    if(is.null(chi)) {
      chi <- list("p.value" = 1)
    }
    condition <- append(condition, i)
    pval <- append(pval, round(chi$p.value, digits=round_digits))
    miss <- append(miss, sum(is.na(df[[i]])))
    if(verbose) {
      print(chi)
    }
  }
  pval_df <- data.frame(condition, miss, pval_fishert=pval)
  return(pval_df)
}

#####################
# table with the distribution of values N of cases and % for categorical variables
table_cat_values <- function(df, columns_to_test, positive_class="1", classvar='predclass', verbose=FALSE, round_digits=8, round_digits_percent=0) {
  check_columns_dataset(df, c(columns_to_test, classvar))
  
  # prepare a df with these columns
  condition <- vector()
  class <- vector()
  value <- vector()
  n_patients <- vector()
  percent_patients <- vector()
  
  # go through the columns
  for(i in columns_to_test) {
    # if there are more than 2 options or there is no positive case ('1') option
    if((length(levels(df[[i]])) > 2) | (length(intersect(c(positive_class), levels(df[[i]]))) == 0)) { 
      options <- levels(df[[i]]) # check all options
    } else { #check only the positive case ('1')
      options <- positive_class
    }
    
    # for each cluster
    for(c in unique(df[[classvar]])) {
      # this is a separating line ?
      if(length(intersect(positive_class, options)) == 0) {
        condition <- append(condition, i)
        class <- append(class, c)
        value <- append(value, positive_class)
        n_patients <- append(n_patients, -1)
        percent_patients <- append(percent_patients, -1)
      }
      
      for(v in options) {
        condition <- append(condition, i)
        class <- append(class, c)
        value <- append(value, v)
        n <- nrow(df[(df[[classvar]] == c) & (df[[i]] == v) & (!is.na(df[[i]])), ])
        n_patients <- append(n_patients, n)
        # calculate the percentage of patients in both the cluster and with the condition
        percent_patients <- append(percent_patients, 
                                   round(100 * nrow(df[(df[[classvar]] == c) & (df[[i]] == v) & (!is.na(df[[i]])), ])/nrow(df[df[[classvar]] == c, ]), digits=round_digits_percent))
      }
    }
  }
  
  cat_df <- data.frame(condition, class, value, n_patients, percent_patients)
  cat_df$info <- ifelse(cat_df$n_patients >= 0,
                        paste0(cat_df$n_patients, " (", cat_df$percent_patients, "%)"),
                        "-")
  
  cat_df$condition <- ifelse(cat_df$value == positive_class, 
                             as.character(cat_df$condition),
                             paste0(cat_df$condition, 
                                    " (", 
                                    cat_df$value, 
                                    ")"))
  
  cat_df$value <- NULL
  cat_df$n_patients <- NULL
  cat_df$percent_patients <- NULL
  
  class <- vector()
  total_patients <- vector()
  for(i in unique(df[[classvar]])) {
    class <- append(class, i)
    total_patients <- append(total_patients, nrow(df[df[[classvar]] == i, ]))
  }
  ct <- data.frame(class, total_patients)
  
  cat_df <- merge(cat_df, ct, on='class')
  #print(head(cat_df))
  
  cat_df$nclass <- paste0(cat_df$class, ' (', cat_df$total_patients, ')')
  
  cat_df$total_patients <- NULL
  cat_df$nclass <- as.factor(cat_df$nclass)
  cat_df$class <- cat_df$nclass
  cat_df$nclass <- NULL
  
  #print(head(cat_df))
  #print(head(cat_df))
  cat_df %>% pivot_wider(names_from = class, values_from = info) -> cat_df
  #print(head(cat_df))
  pval_df <- table_cat_pval(df, columns_to_test, classvar=classvar, verbose=verbose, round_digits=round_digits)
  
  return(merge(cat_df, pval_df, on='condition', all.x=T))
}

#####################
# auxiliary table with one-way anova test on continuous variables
table_continuous_pval <- function(df, columns_to_test, classvar='predclass', verbose=FALSE, round_digits=8) {
  check_columns_dataset(df, c(columns_to_test, classvar))
  
  condition <- vector()
  pval <- vector()
  miss <- vector()
  c_to_test <- columns_to_test
  for(i in c_to_test) {
    if(verbose) {
      print(i)
    }
    tbl <- table(df[[classvar]], df[[i]]) 
    
    if(length(df[[i]][is.finite(df[[i]])]) >= 5000) {
      pval1 <- ad.test(df[[i]][is.finite(df[[i]])])$p.value
    } else {
      pval1 <- shapiro.test(df[[i]][is.finite(df[[i]])])$p.value
    }
    
    if(pval1 < 0.05) {
      tryCatch({
        test <- kruskal.test(as.formula(paste0(i, ' ~ ', classvar)), data = df[is.finite(df[[i]]), ])
        test_pval <- test$p.value[1]
        if(verbose) {
          print(test)
        }
      }, error=function(e) {
        print(paste('Error on kruskal.test, probably only one a single class in comparison.', i, classvar))
      })
      if(!exists('test_pval')) {
        test_pval <- NA
      }
    } else {
      test <- aov(as.formula(paste0(i, ' ~ ', classvar)), data = df[is.finite(df[[i]]), ])
      test_pval <- summary(test)[[1]]['Pr(>F)'][[1]][1]
      if(verbose) {
        print(summary(test))
      }
    }
    pval <- append(pval, round(test_pval, digits=round_digits))
    condition <- append(condition, i)
    miss <- append(miss, sum(is.na(df[[i]])))
  }
  pval_df <- data.frame(condition, miss, pval=pval)
  return(pval_df)
}

#####################
# table with the distribution of values mean (sd) or median (iqr) depending on shapiro-wilk or anderson-darling (if >= 5000 samples)
table_continuous_values <- function(df, columns_to_test, shapiro_threshold=0.05, classvar='predclass', verbose=FALSE, round_digits=8) {
  check_columns_dataset(df, c(columns_to_test, classvar))
  c_to_test <- columns_to_test
  if(is.null(c_to_test) | (length(c_to_test) == 0)) {
    return()
  }
  
  normality_condition <- vector()
  normality_pval <- vector()
  normality_test <- vector()
  
  condition <- vector()
  class <- vector()
  value <- vector()
  main <- vector()
  secondary <- vector()
  for(i in c_to_test) {
    norm_t <- NULL
    if(length(df[[i]][is.finite(df[[i]])]) >= 5000) {
      if(verbose) {
        print(paste0(i, ' using Anderson-Darling normality test due to amount of samples >= 5000 (', length(df[[i]]), ')'))
      }
      shap_test_pval <- ad.test(df[[i]][is.finite(df[[i]])])$p.value
      norm_t <- 'Anderson-Darling'
    } else {
      shap_test_pval <- shapiro.test(df[[i]][is.finite(df[[i]])])$p.value
      norm_t <- 'Shapiro-Wilk'
    }
    normality_condition <- append(normality_condition, i)
    normality_pval <- append(normality_pval, round(shap_test_pval, digits=round_digits))
    normality_test <- append(normality_test, norm_t)
    if(verbose) {
      print(paste(i, shap_test_pval))
    }
    for(c in unique(df[[classvar]])) {
      for(v in "1") {
        condition <- append(condition, i)
        class <- append(class, c)
        if(shap_test_pval < shapiro_threshold) {
          main <- append(main, round(median(df[(df[[classvar]] == c),][[i]], na.rm=T), digits=round_digits))
          
          quart <- quantile(df[(df[[classvar]] == c),][[i]], probs=c(0.25, 0.75), type=8, na.rm=T)
          secondary <- append(secondary, paste0(round(quart[1], digits=round_digits), '-', round(quart[2], digits=round_digits)))
        } else {
          main <- append(main, mean(df[(df[[classvar]] == c),][[i]], na.rm=T))
          secondary <- append(secondary, round(sd(df[(df[[classvar]] == c),][[i]], na.rm=T), digits=round_digits))
        }
      }
    }
  }
  
  cat_df <- data.frame(condition, class, main, secondary)
  cat_df$info <- sprintf("%0.2f (%s)", cat_df$main, cat_df$secondary)
  
  cat_df$main <- NULL
  cat_df$secondary <- NULL
  
  class <- vector()
  total_patients <- vector()
  for(i in unique(df[[classvar]])) {
    class <- append(class, i)
    total_patients <- append(total_patients, nrow(df[df[[classvar]] == i, ]))
  }
  ct <- data.frame(class, total_patients)
  
  cat_df <- merge(cat_df, ct, on='class')
  
  #head(cat_df)
  
  cat_df$nclass <- paste0(cat_df$class, ' (', cat_df$total_patients, ')')
  
  cat_df$total_patients <- NULL
  cat_df$nclass <- as.factor(cat_df$nclass)
  cat_df$class <- cat_df$nclass
  cat_df$nclass <- NULL
  
  #head(cat_df)
  
  cat_df <- cast(cat_df, condition ~ class, value='info')
  pval_df <- table_continuous_pval(df, columns_to_test, classvar=classvar, round_digits=round_digits)
  
  norm_df <- data.frame(condition=normality_condition, normality_test, normality_pval)
  
  return(merge(merge(cat_df, pval_df, on='condition'), norm_df, on='condition'))
}

#####################
# table comorbidities
table_n_comorb <- function(df, comorbidities, subgroup_cases=c(1), cname='comorbidities', cvalue="1", shapiro_threshold=0.05, classvar='predclass', verbose=FALSE, round_digits=8, round_digits_percent=0) {
  check_columns_dataset(df, c(comorbidities, classvar))
  for(c in comorbidities) {
    if(length(intersect(c(cvalue), unique(df[[c]]))) == 0) {
      stop(paste('Condition', c, 'does not have case', cvalue))
    }
  }
  if(is.null(comorbidities) | (length(comorbidities) == 0)) {
    return(NULL)
  }
  
  class <- vector()
  condition <- vector()
  total_patients <- vector()
  subgroups <- vector()
  main <- vector()
  secondary <- vector()
  
  cvalue <- c(cvalue)
  
  df$comorb_per_patient <- apply(df[, comorbidities], 1, function(x) {sum(x %in% cvalue, na.rm=T)})
  
  norm_t <- NULL
  if(nrow(df) >= 5000) {
    if(verbose) {
      print(paste0('comorbidities test', ' using Anderson-Darling normality test due to amount of samples >= 5000 (', nrow(df), ')'))
    }
    shap_test_pval <- ad.test(df$comorb_per_patient)$p.value
    norm_t <- 'Anderson-Darling'
  } else {
    shap_test_pval <- shapiro.test(df$comorb_per_patient)$p.value
    norm_t <- 'Shapiro-Wilk'
  }
  
  for(i in unique(df[[classvar]])) {
    class <- append(class, i)
    condition <- append(condition, ifelse(shap_test_pval < shapiro_threshold, 'median (iqr)', 'mean (sd)'))
    total_patients <- append(total_patients, nrow(df[df[[classvar]] == i, ]))
    subgroups <- append(subgroups, paste(norm_t, round(shap_test_pval, digits=round_digits)))
    if(shap_test_pval < shapiro_threshold) {
      main <- append(main, median(df[(df[[classvar]] == i),]$comorb_per_patient, na.rm=T))
      secondary <- append(secondary, round(IQR(df[(df[[classvar]] == i),]$comorb_per_patient, na.rm=T, type=8), digits=round_digits)) #calculated with the median: quantile estimates are approximately median-unbiased regardless of the distribution 
    } else {
      main <- append(main, mean(df[(df[[classvar]] == i),]$comorb_per_patient, na.rm=T))
      secondary <- append(secondary, round(sd(df[(df[[classvar]] == i),]$comorb_per_patient, na.rm=T), digits=round_digits))
    }
    
    for(s in subgroup_cases) {
      class <- append(class, i)
      total_patients <- append(total_patients, nrow(df[df[[classvar]] == i, ]))
      condition <- append(condition, cname)
      subgroups <- append(subgroups, s)
      main <- append(main, sum(df[df[[classvar]] == i, ]$comorb_per_patient >= s))
      secondary <- append(secondary, 
                          round(100 * sum(df[df[[classvar]] == i, ]$comorb_per_patient >= s) / nrow(df[df[[classvar]] == i, ]), digits=round_digits_percent))
    }
  }
  cat_df <- data.frame(class, condition, total_patients, subgroups, main, secondary)
  
  #####################
  cat_df$nclass <- paste0(cat_df$class, ' (', cat_df$total_patients, ')')
  cat_df$info <- ifelse((cat_df$condition == 'mean (sd)') | (cat_df$condition == 'median (iqr)'),
                        paste0(cat_df$main, " (", cat_df$secondary, ")"),
                        paste0(cat_df$main, " (", cat_df$secondary, "%)"))
  
  cat_df$condition <- ifelse((cat_df$condition == 'mean (sd)') | (cat_df$condition == 'median (iqr)'),
                             paste0(cat_df$condition, ' (', subgroups, ')'),
                             paste0(cat_df$condition, ' (>=', subgroups, ')'))
  cat_df$subgroups <- NULL
  
  cat_df$total_patients <- NULL
  cat_df$nclass <- as.factor(cat_df$nclass)
  cat_df$class <- cat_df$nclass
  cat_df$nclass <- NULL
  
  #####################
  
  cat_df <- cast(cat_df, condition ~ class, value='info')
  return(cat_df)
}

#####################
# combine the results to a file
compile_results_to_xlsx <- function(df, 
                                    continuous_variables=NULL, 
                                    categorical_variables=NULL, 
                                    comorbidity_variables=NULL,
                                    output_file=NULL,
                                    subgroup_cases=c(1),
                                    positive_class="1",
                                    shapiro_threshold=0.05,
                                    cname='comorbidities', 
                                    cvalue="1",
                                    classvar='predclass',
                                    return_data_even_with_file=FALSE,
                                    verbose=F) {
  #df: the dataset
  #continuous_variables
  #categorical_variables
  #comorbidity_variables
  #output_file: for the xlsx file
  #subgroup_cases: vector of numbers
  #positive_class: variable value for the positive class in the categoricla variables
  #cname: name of the conditions in the comorbidity tests
  #cvalue: variable value for having the comorbidity
  #classvar: the variable for the groups
  
  if(is.null(categorical_variables)) {
    categorical_variables <- names(Filter(is.factor, df))
  }
  if(is.null(continuous_variables)) {
    continuous_variables <- setdiff(colnames(df), categorical_variables)
  }
  if(is.null(comorbidity_variables)) {
    comorbidity_variables <- names(which(sapply(categorical_variables, FUN=function(x) {cvalue %in% levels(df[[x]])}) == T))
  }
  
  result_cat <- table_cat_values(df, 
                                 categorical_variables,
                                 positive_class=positive_class,
                                 classvar=classvar,
                                 verbose=verbose)
  result_cont <- table_continuous_values(df, 
                                         continuous_variables,
                                         shapiro_threshold=shapiro_threshold,
                                         classvar=classvar)
  result_comorb <- table_n_comorb(df, 
                                  comorbidity_variables,
                                  subgroup_cases=subgroup_cases,
                                  cname=cname, 
                                  cvalue=cvalue, 
                                  shapiro_threshold=shapiro_threshold,
                                  classvar=classvar)
  
  if(!is.null(output_file)) {
    tryCatch({
      write.xlsx2(result_cat,
                 file = output_file,
                 sheetName = "categorical variables",
                 append = FALSE)
      
      write.xlsx2(result_cont,
                 file = output_file,
                 sheetName = "continuous variables",
                 append = TRUE)
      
      write.xlsx2(result_comorb,
                 file = output_file,
                 sheetName = "comorbidity relation",
                 append = TRUE)
    },
    error=function(e) {
      ow <- options("warn")
      cat(paste0("Failed to save xlsx file. The file '", output_file, "' is probably open somewhere.\n"), file=stderr())
      stop(e)
      options(ow)
    })
  }
  
  if(return_data_even_with_file | is.null(output_file)) {
    return(list(result_cat,
                result_cont,
                result_comorb))
  }
}

table_split_str <- function(t_df) {
  for(i in colnames(t_df)) {
    if(grepl(" ", i)) {
      p1 <- str_split(i, ' ')[[1]][1]
      p2 <- str_split(i, ' ')[[1]][2]
      all_p1 <- unlist(lapply(str_split(t_df[[i]], ' '), function(x) {x[1]}))
      all_p2 <- unlist(lapply(str_split(t_df[[i]], ' '), function(x) {ifelse(is.na(x[2]), '-', x[2])}))
      t_df[[p1]] <- all_p1
      t_df[[p2]] <- str_replace_all(all_p2, '[)(]', '')
      t_df[[i]] <- NULL
    }
  }
  return(t_df)
}
