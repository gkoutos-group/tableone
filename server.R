library(shiny)
library(readxl)
library(summarytools)
st_options(use.x11 = TRUE)

VERSION = "tableOne_0.1"

if(!exists(".describe_table_loaded", mode='function')) source(here::here('describe_table.R'))

if(!exists(".models_loaded", mode='function')) source(here::here('models.R'))


server <- function(input, output, session) {
  ######################################## this code is copied and pasted on the matchmatch project
  load_dataset <- reactive({
    if (is.null(input$dataset$datapath)) {
      tdf <- read_excel("sample.xlsx", sheet="data")
    } else if(grepl(".xls$|.xlsx$", input$dataset$datapath)) {
      tdf <- read_excel(input$dataset$datapath, sheet=ifelse(input$dataset_sheet == "", 1, input$dataset_sheet))
    } else {
      tdf <- read.csv(input$dataset$datapath)
    }
    
    return(tdf)
  })
  
  output$choose_columns_categorical <- renderUI({
    df <- load_dataset()
    if(is.null(df)){
      return()
    }
    cnames <- colnames(df)
    
    checkboxGroupInput("columns_categorical",
                       "Choose categorical columns", 
                       choices  = cnames,
                       selected = cnames)
  })
  
  output$choose_columns_skip <- renderUI({
    if(is.null(input$columns_categorical)) {
      return()
    }
    ops <- input$columns_categorical
    
    if(!is.null(input$columns_skip)) {
      pre_selected <- intersect(input$columns_skip, ops)
    } else {
      pre_selected <- NULL
    }
    
    checkboxGroupInput("columns_skip", 
                       "Choose columns to skip reporting", 
                       choices  = ops,
                       selected = pre_selected)
  })
  
  output$choose_columns_countable <- renderUI({
    if(is.null(input$columns_categorical)) {
      return()
    }
    ops <- input$columns_categorical
    
    if(!is.null(input$columns_skip)) {
      ops <- setdiff(ops, input$columns_skip)
    }
    
    if(!is.null(input$columns_countable)) {
      pre_selected <- intersect(input$columns_countable, ops)
    } else {
      pre_selected <- ops
    }
    
    checkboxGroupInput("columns_countable", 
                       "Choose columns with countable diseases", 
                       choices  = ops,
                       selected = pre_selected)
  })
  
  gimme_numerical <- reactive({
    if(is.null(input$columns_categorical)) {
      return()
    } else {
      return(setdiff(setdiff(colnames(load_dataset()), input$columns_categorical), input$columns_skip))
    }
  })
  
  gimme_categorical <- reactive({
    if(is.null(input$columns_categorical)) {
      return()
    } else {
      return(setdiff(input$columns_categorical, input$columns_skip))
    }
  })
  
  gimme_count_var <- reactive({
    if(is.null(input$columns_countable)) {
      return()
    } else {
      return(input$columns_countable)
    }
  })
  
  process_df <- reactive({
    df <- load_dataset()
    
    for(c in gimme_numerical()) {
      df[[c]] <- as.numeric(df[[c]])
    }
    
    for(c in gimme_categorical()) {
      positives <- tolower(unlist(strsplit(input$positive_values, ',')))
      if((length(unique(df[[c]])) == 2) & (sum(tolower(as.character(df[[c]])) %in% positives) > 0)) {
        df[[c]] <- as.factor(ifelse(as.factor(tolower(as.character(df[[c]]))) %in% positives, 1, 0))
      } else {
        df[[c]] <- as.factor(as.character(df[[c]]))
      }
    }
    
    df$.all_samples <- 1
    return(df)
  })
  
  loaded_file_output <- function() {
    if (is.null(input$dataset$datapath)) {
      paste0("Using sample data.")
    } else {
      if(is.null(input$column_description$datapath)) {
        paste0("Using uploaded file `", input$dataset$name, "` without column definition file.")
      } else {
        paste0("Using uploaded file `", input$dataset$name, "` with column definition file `", input$column_description$name, "`.")
      }
    }
  }
  
  output$group_samples_red <- renderText({
    if(!(input$group_samples %in% colnames(process_df()))) {
      "Group variable not present, describing all samples"
    }
  })
  
  output$digits_red <- renderText({
    .check_digits <- function(x) {
      if(round(x, digits=0) != x) {
        return(FALSE)
      }
      return(TRUE)
    }
    
    for(i in c(input$digits_categorical, input$digits_numerical, input$digits_counts)) {
      if(.check_digits(i) == FALSE) {
        return("Number of digits must be an integer number.")
      }
    }
  })
  
  ######################################## main figures and tables
  # info/text
  output$dataset_info <- renderText({
    loaded_file_output()
  })
  
  output$table_column_description_red <- renderText({
    ca <- gimme_categorical()
    nu <- gimme_numerical()
    
    df <- load_dataset()
    
    a <- ""
    cam <- ca[sapply(ca, function(x) {!(x %in% colnames(df))})]
    if(length(cam) > 0) {
      a <- paste("Categorical variables described and missing in the dataset:", cam, collapse=",")
    }
    
    b <- ""
    num <- nu[sapply(nu, function(x) {!(x %in% colnames(df))})]
    if(length(num) > 0) {
      b <- paste("Numerical variables described and missing in the dataset:", num, collapse=',')
    }
    
    return(paste(a, b))
    
  })
  
  .tnumerical <- reactive({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    table_continuous_values(df, 
                            gimme_numerical(),
                            shapiro_threshold=input$shapiro_threshold,
                            classvar=classvar,
                            round_digits=input$digits_numerical)
  })
  
  .tcategorical <- reactive({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    table_cat_values(df, 
                     gimme_categorical(),
                     positive_class="1",
                     classvar=classvar,
                     verbose=F,
                     round_digits=input$digits_categorical)
  })
  
  .tcountable <- reactive({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    table_n_comorb(df, 
                   gimme_count_var(),
                   subgroup_cases=as.numeric(unlist(strsplit(input$count_group_breaks, ','))),
                   cname=ifelse(is.null(input$count_group) | (input$count_group == ""), "binary variables", input$count_group),
                   cvalue="1",
                   shapiro_threshold=input$shapiro_threshold,
                   classvar=classvar,
                   round_digits=input$digits_counts)
  })
  
  output$table_numerical <- renderTable({
    .tnumerical()
  }, digits=3, rownames = FALSE)
  
  
  output$table_categorical <- renderTable({
    .tcategorical()
  }, digits=3, rownames = FALSE)
  
  
  output$table_countable <- renderTable({
    .tcountable()
  }, digits=3)
  
  # based download name
  base_download_name <- reactive({
    dfn <- "sample"
    if(!is.null(input$dataset$datapath)) {
      dfn <- input$dataset$name
    }
    
    dfd <- "none"
    if(!is.null(input$column_description$datapath)) {
      dfd <- input$column_description$name
    }
    
    return(paste(format(Sys.time(), "%Y%m%d"), dfn, dfd, VERSION, sep="_"))
  })
  
  output$download_table_numerical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_numerical.csv")
    },
    content = function(file) {
      result_cont <- .tnumerical()
      write.csv(result_cont, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_categorical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "categorical.csv")
    },
    content = function(file) {
      result_cat <- .tcategorical()
      write.csv(result_cat, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_counted <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_counts.csv")
    },
    content = function(file) {
      result_comorb <- .tcounts()
      write.csv(result_comorb, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  ########################################
  output$df_summary <- renderUI({
    data <- process_df()
    summarytools::view(dfSummary(data[, setdiff(colnames(data), c('.all_samples'))]),
                       method='render',
                       style='rmarkdown',
                       plain.ascii=T)
  })
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_df_summary.html")
    },
    content = function(file) {
      data <- process_df()
      view(dfSummary(data[, setdiff(colnames(data), c('.all_samples'))]), 
           method='viewer',
           style='rmarkdown',
           file=file)
    },
    contentType = "text/html"
  )
  ########################################
  .tunivariate <- reactive({
    if(input$univariate_variables == '') {
      pselect <- colnames(process_df())
      vars <- setdiff(pselect, c('.all_samples', input$univariate_output, '', NULL))
    } else {
      vars <- unlist(strsplit(input$univariate_variables, ','))
    }
    if(is.null(input$univariate_output_class)) {
      positive_class <- levels(process_df()[[input$univariate_output]])[1]
    } else {
      positive_class <- input$univariate_output_class
    }
    univariate_table(process_df(), input$univariate_output, variables=vars, weighted=input$univariate_weight, positive_class=positive_class)
  })
  
  output$univariate_positive_class <- renderPrint({
    if(is.null(input$univariate_output_class)) {
      positive_class <- levels(process_df()[[input$univariate_output]])[1]
    } else {
      positive_class <- input$univariate_output_class
    }
    cat(positive_class)
  })
  
  output$univariate_table <- renderTable({
    .tunivariate()
  }, digits=3)
  
  output$download_univariate_table <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_univariate.csv")
    },
    content = function(file) {
      write.csv(.tunivariate(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  ########################################
  .multivariate_model <- reactive({
    if(input$multivariate_variables == '') {
      vars <- setdiff(pselect, c('.all_samples', input$multivariate_output, '', NULL))
    } else {
      vars <- unlist(strsplit(input$multivariate_variables, ','))
    }
    if(is.null(input$multivariate_output_class)) {
      positive_class <- levels(process_df()[[input$multivariate_output]])[1]
    } else {
      positive_class <- input$multivariate_output_class
    }
    multivariate_model(process_df(), outcome=input$multivariate_output, variables=vars, weighted=input$univariate_weight, positive_class=positive_class)
  })
  
  .tmultivariate <- reactive({
    table_multivariate_model(.multivariate_model())
  })
  
  output$multivariate_positive_class <- renderPrint({
    if(is.null(input$multivariate_output_class)) {
      positive_class <- levels(process_df()[[input$multivariate_output]])[1]
    } else {
      positive_class <- input$multivariate_output_class
    }
    cat(positive_class)
  })
  
  output$multivariate_table <- renderTable({
    .tmultivariate()
  }, digits=3)
  
  output$multivariate_figure <- renderPlot({
    plot_multivariate_odds(.multivariate_model(), show.values=input$multivarite_show_or)
  })
  
  output$multivariate_text_output <- renderPrint({
    summary(.multivariate_model())
  })

  output$download_multivariate_table <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_multivariate.csv")
    },
    content = function(file) {
      write.csv(.tmultivariate(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_multivariate_figure <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_multivariate_figure.png")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot_multivariate_odds(.multivariate_model(), show.values=input$multivarite_show_or), device = device)
    })
}