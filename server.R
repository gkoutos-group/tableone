library(shiny)
library(readxl)

VERSION = "tableOne_0.1"

if(!exists(".describe_table_loaded", mode='function')) source(here::here('describe_table.R'))

server <- function(input, output, session) {
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
  
  # read the dtypes file
  load_dtypes <- reactive({
    if (is.null(input$column_description$datapath)) {
      return(NULL)
    } else if(grepl(".xls$|.xlsx$", input$column_description$datapath)) {
      tdf <- read_excel(input$column_description$datapath, sheet=ifelse(input$column_description_sheet == "", 1, input$column_description_sheet))
    } else {
      tdf <- read.csv(input$column_description$datapath)
    }
    return(tdf)
  })
  
  # from the dtypes file process other info
  process_dtypes <- reactive({
    df <- load_dataset()
    dtypes <- load_dtypes()
    
    column <- vector()
    mode <- vector()
    type <- vector()
    countable <- vector()
    
    if(!is.null(dtypes)) {
      column <- as.vector(dtypes$column)
      mode <- as.vector(dtypes$mode)
      type <- as.vector(dtypes$type)
      countable <- as.vector(dtypes$countable)
    }
    
    if(input$guess_column_types) {
      # check the others
      for(c in colnames(df)) {
        if(c %in% column) { #already specified
          next
        }
        
        m <- 'automatic'
        t <- infer_column_type(df, c, input$ratio_factor, input$ratio_numeric)
        
        column <- append(column, c)
        mode <- append(mode, m)
        type <- append(type, t)
        countable <- append(countable, NA)
      }
    }
    
    dtypes_n <- as.data.frame(list(column=column, mode=mode, type=type, countable=countable))
    return(dtypes_n)
  })
  
  
  get_valid_columns <- function(cols) {
    if(length(cols) == 0) {
      return(cols)
    }
    df <- load_dataset()
    return(cols[sapply(cols, function(x) {(x %in% colnames(df))})])
  }
  
  gimme_numerical <- reactive({
    dtypes <- process_dtypes()
    candidates <- unique(dtypes[(tolower(dtypes$type) == 'numeric') | (tolower(dtypes$type) == 'int') | (tolower(dtypes$type) == 'float'), ]$column)
    return(get_valid_columns(candidates))
  })
  
  gimme_categorical <- reactive({
    dtypes <- process_dtypes()
    candidates <- unique(dtypes[(tolower(dtypes$type) == 'factor') | (tolower(dtypes$type) == 'categorical') | (tolower(dtypes$type) == 'binary'), ]$column)
    return(get_valid_columns(candidates))
  })
  
  gimme_count_var <- reactive({
    dtypes <- process_dtypes()
    if(is.null(input$count_group) | (input$count_group == "")) {
      ops <- unique(dtypes[dtypes$type == "binary", ]$column)
      
      df <- load_dataset()
      
      return(get_valid_columns(ops[sapply(ops, function(x) { (sum(tolower(as.character(df[[x]])) %in% positives) > 0)})]))
    } else {
      return(get_valid_columns(unique(dtypes[dtypes$countable == input$count_group, ]$column)))
    }
  })
  
  process_df <- reactive({
    df <- load_dataset()
    dtypes <- process_dtypes()
    
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
  
  output$column_descrition_red <- renderText({
    dtypes <- load_dtypes()
    if(is.null(dtypes) & !input$guess_column_types) {
      "Either specify to guess the column types or insert description file"
    }
  })
  
  output$group_samples_red <- renderText({
    if(!(input$group_samples %in% colnames(process_df()))) {
      "Group variable not present, describing all samples"
    }
  })
  
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
  
  output$table_column_description <- renderTable({
    process_dtypes()
  }, rownames = FALSE)
  
  output$table_numerical <- renderTable({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    result_cont <- table_continuous_values(df, 
                                           gimme_numerical(),
                                           shapiro_threshold=input$shapiro_threshold,
                                           classvar=classvar)
    return(result_cont)
  }, rownames = FALSE)
  
  output$table_categorical <- renderTable({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    result_cat <- table_cat_values(df, 
                                   gimme_categorical(),
                                   positive_class="1",
                                   classvar=classvar,
                                   verbose=F)
    return(result_cat)
  }, rownames = FALSE)
  
  
  output$table_countable <- renderTable({
    df <- process_df()
    
    classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
    result_comorb <- table_n_comorb(df, 
                                    gimme_count_var(),
                                    subgroup_cases=as.numeric(unlist(strsplit(input$count_group_breaks, ','))),
                                    cname=ifelse(is.null(input$count_group) | (input$count_group == ""), "binary variables", input$count_group),
                                    cvalue="1",
                                    shapiro_threshold=input$shapiro_threshold,
                                    classvar=classvar)
  })
  
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
  
  output$download_table_columns_description <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_datatypes.csv")
    },
    content = function(file) {
      write.csv(process_dtypes(), tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv"), row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_numerical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_numerical.csv")
    },
    content = function(file) {
      df <- process_df()
      
      classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
      result_cont <- table_continuous_values(df, 
                                             gimme_numerical(),
                                             shapiro_threshold=input$shapiro_threshold,
                                             classvar=classvar)
      
      write.csv(result_cont, tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv"), row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_categorical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "categorical.csv")
    },
    content = function(file) {
      df <- process_df()
      
      classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
      result_cat <- table_cat_values(df, 
                                     gimme_categorical(),
                                     positive_class=1,
                                     classvar=classvar,
                                     verbose=F)
      
      write.csv(result_cat, tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv"), row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_countable <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_counts.csv")
    },
    content = function(file) {
      df <- process_df()
      
      classvar <- ifelse(!(input$group_samples %in% colnames(process_df())), ".all_samples", input$group_samples)
      result_comorb <- table_n_comorb(df, 
                                      gimme_count_var(),
                                      subgroup_cases=as.numeric(unlist(strsplit(input$count_group_breaks, ','))),
                                      cname=ifelse(is.null(input$count_group) | (input$count_group == ""), "binary variables", input$count_group),
                                      cvalue=1,
                                      shapiro_threshold=input$shapiro_threshold,
                                      classvar=classvar)
      
      write.csv(result_comorb, tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv"), row.names = FALSE)
    },
    contentType = "text/csv"
  )

}