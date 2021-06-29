library(shiny)

ui <- fluidPage(
  titlePanel("tableOne"),
  
  span(
    textOutput("dataset_info"),
    style = "color:blue",
    align = 'right'
  ),
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Main",
      sidebarLayout(
        sidebarPanel(
          helpText("Dataset file"),
          fileInput(
            "dataset",
            "Choose CSV/XLS(X) File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".xls",
                       ".xlsx")
          ),
          textInput("dataset_sheet", label = "If XLSX file, add sheet name:", value=""),
          span(textOutput("dataset_red"),
               style = "color:red"),
          
          helpText("Dataset types definition"),
          fileInput(
            "column_description",
            "Choose CSV/XLS(X) File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".xls",
                       ".xlsx")
          ),
          textInput("column_description_sheet", label = "If XLSX file, add sheet name:", value=""),
          checkboxInput("guess_column_types", "Guess column types", value = TRUE),
          span(textOutput("column_descrition_red"),
               style = "color:red"),
          
          hr(),
          textInput("group_samples", label = "Group samples on:", value="group"),
          span(textOutput("group_samples_red"),
               style = "color:red"),
          
          textInput("count_group", label = "Count positive column elements based:", value=""),
          textInput("count_group_breaks", label = "Number of breaks (comma-separated `,`)", value="1,2,3"),
          span(textOutput("count_group_red"),
               style = "color:red"),
          
          textInput("positive_values", label = "What values indicate positives on binary data? (comma-separated `,`)", value="1,yes"),
          
          hr(),
          helpText("Advanced options"),
          numericInput(
            "ratio_numeric",
            label = "Ratio for numeric column identification",
            value = 0.1, min = 0, max = 1,
          ),
          numericInput(
            "ratio_factor",
            label = "Ratio for categorical column identification",
            value = 0.2, min = 0, max = 1
          ),
          numericInput(
            "shapiro_threshold",
            label = "Normality test p-val",
            value = 0.05, min = 0, max = 1
          ),
          
          hr(),
          helpText("Number of digits in downloaded file"),
          numericInput(
            "digits_categorical",
            label = "Number of digits for categorical table",
            value = 2, min = 0, max = 6, step=1,
          ),
          numericInput(
            "digits_numerical",
            label = "Number of digits for numerical table",
            value = 2, min = 0, max = 6, step=1,
          ),
          numericInput(
            "digits_counts",
            label = "Number of digits for counts table",
            value = 2, min = 0, max = 6, step=1,
          ),
          span(textOutput("digits_red"),
               style = "color:red"),
        ),
        
        mainPanel(
          helpText("Data types specification"),
          tableOutput("table_column_description"),
          span(textOutput("table_column_description_red"),
               style = "color:red"),
          downloadButton("download_table_column_description", "Download"),
          
          helpText("Numerical features"),
          tableOutput("table_numerical"),
          downloadButton("download_table_numerical", "Download"),
          
          helpText("Categorical features"),
          tableOutput("table_categorical"),
          downloadButton("download_table_categorical", "Download"),
          
          helpText("Counted features"),
          tableOutput("table_countable"),
          downloadButton("download_table_counted", "Download")
        )
      )
    ),
    tabPanel("Dataset summary",
             htmlOutput("df_summary"),
             downloadButton("download_summary", "Download")),
    tabPanel("Univariate analysis",
             sidebarLayout(
               sidebarPanel(
                 helpText("Model information:"),
                 textInput("univariate_output", label="What is the output column?", value="group"),
                 textInput("univariate_variables", label = "What variables should be modelled? (comma-separated `,`; empty for all)", value="age,sex"),
                 hr(),
                 helpText("This tries countering class unbalance:"),
                 checkboxInput("univariate_weight", "Class weighting?", value = TRUE),
                 hr(),
                 textInput("univariate_output_class", label="What is the positive output value?", value="af")
               ),
               mainPanel(
                 helpText("Positive class considered:"),
                 verbatimTextOutput("univariate_positive_class"),
                 tableOutput("univariate_table"),
                 downloadButton("download_univariate_table", "Download"),
               )
             )
    ),
    tabPanel("Multivariate analysis",
             sidebarLayout(
               sidebarPanel(
                 helpText("Model information:"),
                 textInput("multivariate_output", label="What is the output column?", value="group"),
                 textInput("multivariate_variables", label = "What variables should be in the model? (comma-separated `,`; empty for all)", value="age,sex"),
                 hr(),
                 helpText("This tries countering class unbalance:"),
                 checkboxInput("multivariate_weight", "Class weighting?", value = TRUE),
                 hr(),
                 checkboxInput("multivarite_show_or", "Show OR on figure", value=FALSE),
                 hr(),
                 textInput("multivariate_output_class", label="What is the positive output value?", value="af")
               ),
               mainPanel(
                 helpText("Positive class considered:"),
                 verbatimTextOutput("multivariate_positive_class"),
                 tableOutput("multivariate_table"),
                 downloadButton("download_multivariate_table", "Download"),
                 plotOutput("multivariate_figure"),
                 downloadButton("download_multivariate_figure", "Download"),
                 verbatimTextOutput("multivariate_text_output")
               )
             )
    ),
    tabPanel("Help",
             HTML("Numerical variables:<br/>
If the number of elements is equal or above 5000 the normality test performed is Anderson-Darling test. In other cases the test performed is Shapiro-Wilk.<br/>
The reported numerical variables will be either:<br/>
- non-normal variables: median (1st-3rd quartile)<br/>
- normal variables: mean (standard deviation)<br/>
<br/>
The calculated statistical difference between the groups is based on the variables provided.<br/>
- non-normal variables: Kruskal-Wallis Rank test [1]<br/>
- normal variables: one-way anova [2] (note that this was made for balanced designs!).<br/>
<br/><br/>
Categorical variables:<br/>
Fisher's Exact test is performed to check for independence. [3]<br/>
If there are more than 2x2 options, the p-value is simulated through Monte Carlo simnulation. [4]<br/>
<br/><br/>
Count tables:<br/>
These are generated counting the number of samples that had more than each possibility specified.<br/>
<br/><br/>
Models:<br/>
Univariate uses a single variable at a time (\"uncorrected\").<br/>
Multivariate model uses all variables together (\"adjusted model\").<br/>
This is a generalised linear model, using a quasi-binomial function with logit link. [5]<br/>
The weighting, if selected weights the samples using a weight for each sample: <br/>
weights <- 1 + (as.numeric(as.character(df[[outcome]])) * (as.numeric(nrow(df) / table(df[[outcome]])['1'] - 1)))<br/><br/>
The confidence interval reported is the 95% CI.<br/>
If the model is not shown for univariate variables; or nothing on multivariate analysis, it means that there is something wrong with the variables; it could be that it is fully correlated with the outcomes.<br/>
<br/><br/>
References:<br/>
[1] Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115-120.<br/>
[2] Chambers, J. M., Freeny, A and Heiberger, R. M. (1992) Analysis of variance; designed experiments. Chapter 5 of Statistical Models in S eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.<br/>
[3] Fisher, R. A. (1970). Statistical Methods for Research Workers. Oliver & Boyd.
[4] Clarkson, D. B., Fan, Y. and Joe, H. (1993) A Remark on Algorithm 643: FEXACT: An Algorithm for Performing Fisher's Exact Test in r x c Contingency Tables. ACM Transactions on Mathematical Software, 19, 484-488. doi: 10.1145/168173.168412.
[5] Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. New York: Springer.

"))
  ),
            hr(),
            HTML('<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is private on <a href="https://github.com/gkoutos-group/tableone/">https://github.com/gkoutos-group/tableone/</a>. For details contact <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.')
)