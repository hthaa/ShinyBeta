library(shiny)
library(shinyjs)
library(betafunctions)

# Define UI for application.
ui <- fluidPage(useShinyjs(),
  titlePanel("Shinybeta: L&L Estimated Diagnostic Performance of Binary Classifications"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Basic",
               tabsetPanel(
                 tabPanel("Required", br(),
                          fileInput("file1", "Choose CSV file with single column of data",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          sliderInput("Reliability",
                                      "Test-score reliability-coefficient",
                                      min = 0,
                                      max = 1,
                                      value = .851,
                                      step = .001),
                          fluidRow(column(4, numericInput("Min", "Min-score", 0)),
                                   column(4, numericInput("Max", "Max-score", 100)),
                                   column(4, numericInput("Cut", "Cut-score", 50))),
                          br(), code("Author: Haakon Haakstad"),
                          br(), code("Version: 1.6.1"),
                          br(), code("Updated: December 15th, 2022"),
                          br(), code("Launched: December 2nd, 2020")
                 ),
                 tabPanel("ROC", br(),
                          checkboxInput("InclTcut", "Specify a 'true cut-score'.", FALSE),
                          numericInput("Tcut", "True-cut", 50), br(),
                          checkboxInput("ROC", "Do ROC-curve analysis.", FALSE),
                          numericInput("Eval", "Number of points for evaluation", 100),
                          checkboxInput("AUC", "Calculate AUC statistic.", FALSE),
                          checkboxInput("J", "Locate cut-point maximizing Youden's J.", FALSE),
                          checkboxInput("A", "Locate cut-point maximizing Accuracy.", FALSE),
                          checkboxInput("Locate", "Locate cut-point where:", FALSE),
                          fluidRow(column(8, selectInput("Index", "Diagnostic Performance Index:", 
                                                         list("Sensitivity", "Specificity", "Positive Predictive Value (PPV)", "Negative Predictive Value (NPV)"), 
                                                         selected = "Sensitivity", 
                                                         multiple = FALSE, 
                                                         selectize = TRUE,
                                                         width = NULL,
                                                         size = NULL)),
                                   column(4, numericInput("Reaches", "Reaches:", 0.9)))
                 ),
                 tabPanel("Model fit", br(),
                          numericInput("nbins", "Set initial number of bins:", 100), br(),
                          numericInput("minexp", " Set minimum number of expected observations:", 10), br(),
                          checkboxInput("mdlfitgfx", "Plot expected vs. observed values:", FALSE)
                 )
               )
      ),
      tabPanel("Advanced", 
               tabsetPanel(
                 tabPanel("Distributions", br(),
                          selectInput(
                            "Model",
                            "Choose true-score model",
                            list("4P", "2P"),
                            selected = "4P",
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL,
                            size = NULL
                          ),
                          sliderInput("L",
                                      "Lower-bound (if true-score model == 2P)",
                                      min = 0,
                                      max = 1,
                                      value = 0),
                          sliderInput("U",
                                      "Upper-bound (if true-score model == 2P)",
                                      min = 0,
                                      max = 1,
                                      value = 1)
                 ),
                 tabPanel("Graphs", br(),
                          checkboxInput("CustomBins", "Set number of histogram bins?", FALSE),
                          numericInput("Bins",
                                       "Number of bins",
                                       5), br(),
                          checkboxInput("Ylim", "Customize scale of Y-axis?", FALSE),
                          numericInput("Yupper", "Upper limit of Y-axis", 10)
                 )
               )
      )
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Graph", plotOutput("distPlot")),
      tabPanel("Raw output",
               tabsetPanel(tabPanel("Diagnostic performance", 
                                    verbatimTextOutput("acc")),
                           tabPanel("Classification consistency", 
                                    verbatimTextOutput("con")),
                           tabPanel("Technical",
                                    verbatimTextOutput("tec")),
                           tabPanel("Model fit",
                                    verbatimTextOutput("mdlfit"))
               )
      ),
      tabPanel("Information", 
               tabsetPanel(tabPanel("About", br(),
                                    "This application is developed by Haakon Haakstad, Centre for Educational Measurement at the University of Oslo (CEMO), and uses the betafunctions R-package (Haakstad, 2022) to apply the Livingston and Lewis (1995) approach to a set of values representing test-scores.",  br(), br(),
                                    "Questions regarding- or suggestions concerning further development of this application are encouraged, and can be directed at h.e.haakstad@gmail.com.", br(), br(),
                                    "Follow me on Twitter for announcements regarding the betafunctions R-package and the Shinybeta web-app:", a("https://twitter.com/h_haakstad", href = "https://twitter.com/h_haakstad", .noWS = "after"), ".", br(), br(), br(),
                                    strong("References:"), br(),
                                    "Haakstad, H. (2022). betafunctions: Functions for Working with Two- And Four-Parameter Beta Probability Distributions and Psychometric Analysis of Classifications.", a("https://cran.r-project.org/web/packages/betafunctions/index.html", href = "https://cran.r-project.org/web/packages/betafunctions/index.html"), br(), br(),
                                    "Lewis, D. and Burke, C. J. (1949). The Use and Misuse of the Chi-Square Test. Psychological Bulletin, 46(6): 433-489:", a("https://doi.org/10.1037/h0059088", href = "https://doi.org/10.1037/h0059088"), br(), br(),
                                    "Livingston, S.A. and Lewis, C. (1995). Estimating the Consistency and Accuracy of Classifications Based on Test Scores. Journal of Educational Measurement, 32: 179-197.", a("https://doi.org/10.1111/j.1745-3984.1995.tb00462.x", href = "https://doi.org/10.1111/j.1745-3984.1995.tb00462.x"), br(), br(),
                                    "Lord, F. (1965). A Strong-True-Score Theory, With Applications. Psychometrica, 30(30): 239???270:", a("https://doi.org/10.1007/BF02289490", href = "https://doi.org/10.1007/BF02289490")
               ),
               tabPanel("Input", 
                        tabsetPanel(
                          tabPanel("Basic",
                                   tabsetPanel(
                                     tabPanel("Required", 
                                              br(), strong("Data:"), br(),
                                              "The input file must include values organized in a single column. If the test-scores are non-integers (i.e., numbers containing decimals), the decimal-point must be marked by punctuation (e.g., one-and-a-half = 1.5). The input-file must be in the .csv format, which can be made by employing software such as Microsoft Excel, or simply by using Notepad.", br(), br(), 
                                              "An example data-set can be found", a("here", href = "https://raw.githubusercontent.com/hthaa/shinybeta/master/example.csv", .noWS = "after"), ". The minimum possible score to attain for the test represented by this set of data is 0 and the maximum possible score is 25. The estimated reliability of the test is 0.79. The cut-score is not set, but must be less than 25 and greater than 0.", br(), br(),
                                              strong("Reliability:"), br(),
                                              "The reliability coefficient represent the proportion of variance shared by the observed- and true-score distributions. Common ways of estimating this quantity is by employing estimators such as coefficent alpha, coefficient omega, or the generalizability coefficient.", br(), br(),
                                              "The slider allows for specifying the reliability coefficient down to the third decimal place. Because it is so fine-grained, it can be difficult to hit the desired reliability input. If you have clicked the slider, you can use the arrow keys on your keyboard to increase or decrease the value by 0.001 at a time.", br(), br(),
                                              strong("Test information:"), br(),
                                              "The min- and max-score entries represent the minimum and maximum possible scores on the test (i.e., not necessarily the minimum and maximum observed-scores). The cut-score represents the point on the observed-score scale which marks the threshold for categorization."
                                     ),
                                     tabPanel("ROC",
                                              br(),
                                              strong("Specifying a 'true-cut':"), br(),
                                              "Specifying a true-cut allows for examining the effects of changing the used cut-score on diagnostic performance, given a true cut-score. This might be useful in cases where there are asymmetric costs associated with the different types of errors. For example, using a cut above the true-cut would be 'erring on the side of caution', valuing correct classification of individuals with true-scores below the true-cut above the correct classification of individuals with true-scores above the true-cut.",
                                              br(), br(), 
                                              strong("ROC-curve analysis:"), br(),
                                              "Checking the ROC-curve box allows for doing what is referred to as Receiver-Operator Characteristic (ROC) analysis, which is a graph showing the performance of a classification model at all classification thresholds. That is, it holds the true-cut constant and evalutes the false-postive and true-positive rates across actually applied cut-scores from the minimum to the maximum possible score. Such an analysis can for example be used to find the point at which the sum of sensitivity and specificity is maximized (i.e., the Youden's J. statistic), or to evaluate the contribution of the test to prediction by means of the Area Under the Curve (AUC) statistic. More specifically, the AUC provides an aggregate measure of performance across all possible classification thresholds."
                                              ),
                                     tabPanel("Model fit",
                                              br(),
                                              strong("Set initial number of bins:"), br(),
                                              "Allows for determining the initial number of bins the observed-score distribution is to be lumped into for the purpose of model-fit testing. Default is set to 100.",
                                              br(), br(), 
                                              strong("Set minimum number of expected observations:"), br(),
                                              "Allows for setting the minimum number of expected observations for each bin. The initial number of bins are collapsed so that each bin has a number of expected observations corresponding to this specified number. Lord (1963) suggested this value should be set to 1 so as to be able to detect misfits in the tails. Lewis and Burke however suggested that this number should be set to 10. Here, the initial number is set to 10."
                                     )
                                     
                                   )
                          ),
                          tabPanel("Advanced", 
                                   tabsetPanel(
                                     tabPanel("Distributions",
                                              br(),
                                              strong("True-score model:"), br(),
                                              "Allows the user to choose between a four- (4P) or two-parameter (2P) Beta-distribution true-score model. For the 4P option, four parameters of the true-score distribution are estimated. These are the location parameters (lower- and upper-bounds of the distribution) and the shape parameters (known as the alpha and beta parameters of the distribution). The fitting procedure finds the necessary values for the four-parameters to produce a distribution with the same mean, variance, skewness, and kurtosis as the estimated true-score distribution (which, in turn, is estimated using the observed-scores and the reliability coefficient). For the 2P solution, the location-parameters are fixed to some user-specified values (default is a lower-bound of 0 and an upper-bound of 1), and the shape-parameters are estimated so that the resulting distribution has the same mean and variance as the estimated true-score distribution.", br(), br(), 
                                              strong("When to change these parameters:"), br(),
                                              "There can be several good reasons for making adjustments to the default setting. First, the fitting procedure for the four-parameter solution is prone to produce impermissible estimates - especially for small samples. Impermissible estimates can be a lower-bound parameter < 0, upper-bound parameter > 1, or alpha/beta < 0. Second, there might be cases where it makes good theoretical sense to fix these parameters to some pre-specified values. For example, in the case of a multiple-choice test consisting of items with four response-categories, there is a 25% chance for test-takers to guess the correct answer. In such a circumstance, it can make sense to specify the lower-bound location-parameter to 0.25 a-priori.",
                                     ),
                                     tabPanel("Graphs",
                                              br(),
                                              strong("Specify number of histogram bins:"), br(),
                                              "Allows for customizing the number of bins that is to make up the histogram.",
                                              br(), br(),
                                              strong("Specify upper limit of Y-axis"), br(),
                                              "Allows for customizing the scale of the Y-axis in the histogram plot.")
                                   )
                          )
                        )
               ),
               tabPanel("Output", 
                        tabsetPanel(
                          tabPanel("Graph", br(),
                                   strong("Default histogram:"), br(),
                                   "The graph show a histogram of the observed-score distribution with the estimated true-score distribution super-imposed. The lower the reliability, the more narrow the true-score distribution will be relative to the observed-score distribution. Conversely, the higher the reliability, the more closely the true-score distribution will match the observed-score distribution.", br(), br(),
                                   "The top-left side of the graph shows some diagnostic performance indices estimated by the procedure.", br(), br(),
                                   strong("ROC-curve:"), br(),
                                   "The ROC curve option under Advanced allows one to do a so-called Receiver Operator Characteristic Curve analysis, which produces a graph displaying the performance of a classification model at all classification thresholds. This curve plots the true-positive rate against the false-positive rate. By holding a true-cut constant, changing the used cut-score allows one to, among other things, find the used cut-score for which the sum of sensitivity and specificity is maximized (i.e., the employed cut-score which maximizes the Youden J. statistic if the true-cut is held constant), or the Area Under the Curve statistic (AUC), which is an aggregate measure of performance across all possible classification thresholds.", br(), br(),
                                   strong("Model fit"), br(),
                                   "Graph tracing the expected versus the observed frequencies of observations. Allows for visually gauging the severity of model misfit in the event that the chi-square test yields an unsatisfactory p-value."
                          ),
                          tabPanel("Diagnostic performance", br(),
                                   "Diagnostic performance indices quantify different aspects of test-functioning when binary classifications are made based on test-scores.", br(), br(),
                                   "Diagnostic performance indices are calculated based on (estimates of) true and false positives and negatives. A true-positive occurs when a phenomenon is identified as being present when in fact it is, and a false-positive when a phenomenon is identified as being present when in fact it is not. Conversely, a true-negative occurs when a phenomenon is identified as not being present when it fact it is not, and a false-negative occurs when a phenomenon is identified as not being present when in fact it is.", br(), br(),
                                   "Here, a positive is an observation falling below the defined cut-off point, and a negative is an observation falling at or above the cut-off point. Hence, in the context of an examination, a positive can be considered as a failing grade, and a negative can be considered as a passing grade. A true-positive can as such be considered an individual producing an observed-score in the failing region of the scale, when his true-score is indeed to be found in the failing region of the scale. A false-positive would then be an examinee with a true-score at or above the cut-score producing an observed-score below the cut-off score. Conversely, a true-negative is a true-pass examinee producing a passing observed-score, and a false-negative a true-pass examinee producing a failing observed-score.", br(), br(),
                                   "True and false positives and negatives are usually organized in what is referred to as a", strong("confusion matrix"), "(provided as output by this application). From this matrix, a number of diagnostic performance indices can be calculated. This application provides only a (popular) selection of these.", br(), br(), "The diagnostic performance indices provided by this application are:", br(), 
                                   strong("Sensitivity:"), "In this context, the proportion of correctly classified true-fail examinees.", br(), 
                                   strong("Specificity:"), "The proportion of correctly classified true-pass examinees.", br(),
                                   strong("LR+:"), "The positive likelihood ratio. The probability of a true-fail examinee producing a failing score, divided by the probability of a true-pass examinee producing a failing score.", br(),
                                   strong("LR-:"), "The negative likelihood ratio. The probability of a true-fail examinee producing a passing score, divided by the probability of a true-pass examinee producing a passing score.", br(),
                                   strong("PPV:"), "The Positive Predictive Value. The proportion of true-fail among all observed-fail.", br(),
                                   strong("NPV:"), "The Negative Predictive Value. The proportion of true-pass among all observed-pass.", br(),strong("Youden's J.:"), "The sum of Sensitivity and Specificity, minus 1.", br(),
                                   strong("Accuracy:"), "The overall proportion of correctly classified examinees."
                          ),
                          tabPanel("Classification consistency", br(),
                                   "Classification consistency indices quantify the reproducibility of classifications across replications of the testing procedure. Conventionally, this refers to the probability (or proportions) of consistent classifications across two independent test administrations. Hence, the classification consistency indices are estimated by assuming the statistical properties of independence and strict parallelism. The proportions of consistent and inconsistent classifications can be arranged in the form of a", strong("contingency matrix"), "where the proportions of consistent classifications are found in the principal diagonals (i.e., from the top-left down to the bottom-right), and inconsistent classifications are found in the upper- and lower triangles (which in the case of two possible categories amount to the non-principal diagonal, meaning from the top-right down to the bottom-left).", br(), br(),
                                   "The classification consistency indices offered by this application are:", br(),
                                   strong("p:"), "The proportion of consistent classifications.", br(),
                                   strong("p_c:"), "The proportion of consistent classifications by chance.", br(),
                                   strong("Kappa:"), "The proportion of consistent classifications after chance consistency is removed from consideration."),
                          tabPanel("Technical", br(),
                                   "The technical output relates to the true- and error-score distribution parameters estimated by the Livingston and Lewis approach.", br(), br(), 
                                   "The effective test length is a parameter used to decompose the observed-score distribution into the true-score and error-distributions. In the Livingston and Lewis approach, this value is usually rounded to the nearest integer. In the technical output, both the rounded and non-rounded values are reported, although the rounded value is the one used in the procedure.", br(), br(),
                                   "The l, u, alpha, and beta parameters are parameters of the true-score beta distribution. The l and u parameters are referred to as the lower- and upper-bound parameters of the beta distribution, and alpha and beta the shape-parameters of the distribution. The higher the test-score reliability, the more closely the true-score beta distribution will match the observed-scores."),
                          tabPanel("Model fit", br(),
                                   "Model fit is examined using the chi-square test in accordance with the procedures outlined by Don and Burke (1949). This is an approximation procedure, and the chi-squared test should be treated as such.", br(), br(),
                                   "The 'contingencytable' output  shows the expected and observed frequencies for bins.", br(), br(), 
                                   "The 'chisquared' value shows the chi-square calculated based on the table of expected and observed frequencies above.", br(), br(),
                                   "The 'df' value shows the degrees of freedom. In accordance with Lord (1965), the chi-squared test has N-bins minus 4 degrees of freedom for the four-parameter true-score distribution, and N-bins minus 2 degress of freedom for the two-parameter true-score distribution.", br(), br(),
                                   "The 'pvalue' output shows the probability of observing a chi-squared as large or larger given the degrees of freedom, given that the model is correct.", br(),
                                   "Note that the model-fit test is very sensitive to the reliability coefficient provided that is provided as input, so for the model-fit test providing a coefficient down to the third decimal place is recommended.")
                        )
               ),
               tabPanel("Change-log", br(),
                        strong("v. 1.6.1:"), "Aesthetic changes to the user interface. Greys out options that are irrelevant unless certain input is specified. Reliability slider now allows for specifying reliability down to the third decimal place. If it proves difficult to specify the desired reliability with the mouse following change, know that the arrow-keys on the keyboard should allow you to increase or decrease this value by 0.001 at a time.", br(), br(),
                        strong("v. 1.6.0:"), "Added functionality for controlling the model-fit testing procedure by specifying the initial number of bins and the minimum number of expected observations per bin. Also includes the possibility of creating a plot that plots the expected vs observed observations within each bin against each other, which should give some indication of the degree of model misfit.", br(), br(),
                        strong("v. 1.5.1:"), "The number of bins for model-fit estimation has been increased to an initial number of 1000, and the minimum size of the bins is set to 1 (in line with the recommendations by Lord (1965): A Strong True-Score Theory, With Applications. Psychometrika, 30(3).", br(), br(),
                        strong("v. 1.5.0:"), "ROC-curve now exhibits the classical 'staircase' aesthetic. New model-fit tab under the 'Raw Output' tab.", br(), br(),
                        strong("v. 1.4.2:"), "Fixed the issue of diagnostic performance statistics not being listed under the Raw Output tab. Minor adjustments to raw output.", br(), br(),
                        strong("v. 1.4.1:"), "Added functionality for the ROC analysis, allowing for the possibility of searching for a cut-point where Sensitivity, Specificity, Negative Predictive Value (NPV) or Positive Predictive Value (PPV) reach some specifiable value (between 0 and 1). Removed option for specifying a Beta error model as testing indicated underperformance.", br(), br(),
                        strong("v. 1.4.0:"), "Minor correction to the calculation of diagnostic performance (previously evaulated up-to and including the cut-point, where the intended behavour was to evaulate up-to and not including the cut-point). Prior to this correction it is expected that the app will have slightly underestimated diagnostic performance. ROC analysis now includes the possibility of finding the used-cut resulting in the highest Accuracy.", br(), br(),
                        strong("v. 1.3.0:"), "Several adjustments to UI and functionality. Added additional options for tailoring the look of the default graph output, such as specifying the number of histogram bins and the upper limit of the Y-axis. Options are found under Advanced -> Graphs. Choice of ROC-curve analysis is also moved to this tab. Allows now for specifying the number of cut-points for the ROC analysis. Now allows for choosing a Beta error model rather than the default Binomial error model. Error model specification can be done under the Advanced -> Distributions tab. The Beta error model is experimental, and has not as of yet been tested.", br(), br(),
                        strong("v. 1.2.2:"), "IMPORTANT: Input data no longer need to be named. If the input is named, the app will handle it in the back-end, so the app should be backwards compatible. Added link to an example data-set to Information -> Input tab. This file can be used as a template for ones own data.", br(), br(),
                        strong("v. 1.2.1:"), "Minor adjustments to user interface and documentation. Most importantly, fixed an error in documentation with respect to the descriptions of true and false positives and negatives. Now also includes a 'How to cite' tab.", br(), br(),
                        strong("v. 1.2.0:"), "Added functionality to do ROC-curve analysis.", br(), br(),
                        strong("v. 1.1.0:"), "Launch."),
               tabPanel("How to cite", br(),
                        strong("APA7-style citation:"), br(),
                        HTML("Haakstad, H. (2022). <em>Shinybeta: L&L Estimated Diagnostic Performance of Binary Classifications</em>. https://hthaa.shinyapps.io/shinybeta/"), br(), br(),
                        strong("Bibtex entry:"), br(),
                        "@misc{shinybeta,", br(),
                        HTML('&emsp;'), "author = 'H. Haakstad',", br(),
                        HTML('&emsp;'), "title = 'Shinybeta: L&L Estimated Diagnostic Performance of Binary Classifications',", br(),
                        HTML('&emsp;'), "year = '2022',", br(),
                        HTML('&emsp;'), "url = 'https://hthaa.shinyapps.io/shinybeta/'", br(),
                        "}")
               )
      )
    )
  )
)
server <- function(input, output) {
  observeEvent(input$InclTcut, {
    toggleState("Tcut")
    toggleState("ROC")
    if (input$ROC) {
      toggleState("Eval")
      toggleState("AUC")
      toggleState("J")
      toggleState("A")
      toggleState("Locate")
      toggleState("Index")
      toggleState("Reaches")
    }
  })
  observeEvent(input$ROC, {
    toggleState("Eval")
    toggleState("AUC")
    toggleState("J")
    toggleState("A")
    toggleState("Locate")
    toggleState("Index")
    toggleState("Reaches")
  })
    observeEvent(input$Model == "2P", {
    toggleState("L")
    toggleState("U")
  })
  observeEvent(input$CustomBins, {
    toggleState("Bins")
  })
  observeEvent(input$Ylim, {
    toggleState("Yupper")
  })
  output$distPlot <- renderPlot({
    if(!is.null(input$file1$datapath)) {
      dta <- read.csv(input$file1$datapath, header = FALSE)
      dta <- as.vector(dta[, 1])
      if (class(dta[1]) == "character") {
        dta <- as.numeric(dta[-1])
      }
      headline <- "Proportional Distribution"
    } else {
      set.seed(123)
      dta <- rbinom(1000, 100, rBeta.4P(1000, .2, .95, 6, 4))
      headline <- "No data entered. Showing example data."
    }
    if(input$InclTcut == FALSE) {
      Tcut <- NULL
    } else {
      Tcut <- input$Tcut
    }
    if((input$ROC == FALSE & input$mdlfitgfx == FALSE) | ((input$ROC == TRUE & input$InclTcut == FALSE) & input$mdlfitgfx == FALSE)) {
      otp <- LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                   truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp))
      if (input$Ylim == FALSE) {
        ymax <- max(dBeta.4P(seq(0, 1, .001), otp$parameters$l, otp$parameters$u, otp$parameters$alpha, otp$parameters$beta))
      } else {
        ymax <- input$Yupper
      }
      hist((dta - input$Min) / (input$Max - input$Min), col = "grey95", xlim = c(0, 1), 
           ylim = c(0, ymax), 
           xlab = "Proportion-of-max scores", main = headline, freq = FALSE, breaks = if(input$CustomBins) {input$Bins} else {"Sturges"})
      curve(dBeta.4P(x, otp$parameters$l, otp$parameters$u, otp$parameters$alpha, otp$parameters$beta), from = 0, to = 1,
            ylab = "Density", xlab = "True-scores", add = TRUE)
      if (!is.null(Tcut)) {
        abline(v = (Tcut - input$Min) / (input$Max - input$Min), lty = 3)
      }
      abline(v = (input$Cut - input$Min) / (input$Max - input$Min), lty = 2)
      legend("topleft", bty = "n", cex = 1.25, 
             legend = 
               c(paste("Sensitivity =", round(otp$classification.accuracy$Sensitivity, 3)),
                 paste("Specificity =", round(otp$classification.accuracy$Specificity, 3)),
                 paste("PPV =", round(otp$classification.accuracy$PPV, 3)),
                 paste("NPV =", round(otp$classification.accuracy$NPV, 3)),
                 paste("Accuracy =", round(otp$classification.accuracy$Accuracy, 3))))
      if (!is.null(Tcut)) {
        legend("topright", bty = "n", cex = 1.25, lty = c(1, 2, 3),
               legend = c("Traced true-score distribution", "Used cut-point", "True cut-point"))
      } else {
        legend("topright", bty = "n", cex = 1.25, lty = c(1, 2),
               legend = c("Traced true-score distribution", "Cut-point"))
      }
    }
    if(input$ROC == TRUE & input$mdlfitgfx == FALSE & input$InclTcut) {
      LL.ROC(dta, input$Reliability, input$Min, input$Max, input$Tcut, true.model = input$Model, AUC = input$AUC, maxJ = input$J, 
             maxAcc = input$A, locate = if (input$Locate) { c(input$Index, input$Reaches) } else { NULL }, 
             l = input$L, u = input$U, grainsize = input$Eval)
    }
    if(input$mdlfitgfx == TRUE) {
      mdlfit.gfx(LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                   truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp)))
    }
  }
  )
  output$acc <- renderPrint({
    if(!is.null(input$file1$datapath)) {
      dta <- read.csv(input$file1$datapath)
      dta <- as.vector(dta[, 1])
      if (class(dta[1]) == "character") {
        dta <- as.numeric(dta[-1])
      }
    } else {
      set.seed(123)
      dta <- rbinom(1000, 100, rBeta.4P(1000, .2, .95, 6, 4))
    }
    if(input$InclTcut == FALSE) {
      Tcut <- NULL
    } else {
      Tcut <- input$Tcut
    }
    ot <- LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp))
    otacc <- list("Confusion matrix:" = ot$confusionmatrix, "Diagnostic performance indices:" = ot$classification.accuracy)
    print(otacc)
  })
  output$con <- renderPrint({
    if(!is.null(input$file1$datapath)) {
      dta <- read.csv(input$file1$datapath)
      dta <- as.vector(dta[, 1])
      if (class(dta[1]) == "character") {
        dta <- as.numeric(dta[-1])
      }
    } else {
      set.seed(123)
      dta <- rbinom(1000, 100, rBeta.4P(1000, .2, .95, 6, 4))
    }
    if(input$InclTcut == FALSE) {
      Tcut <- NULL
    } else {
      Tcut <- input$Tcut
    }
    ot <- LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp))
    otcon <- list("Contingency matrix:" = ot$consistencymatrix, "Classification consistency indices:" = ot$classification.consistency)
    print(otcon)
  })
  output$tec <- renderPrint({
    if(!is.null(input$file1$datapath)) {
      dta <- read.csv(input$file1$datapath)
      dta <- as.vector(dta[, 1])
      if (class(dta[1]) == "character") {
        dta <- as.numeric(dta[-1])
      }
    } else {
      set.seed(123)
      dta <- rbinom(1000, 100, rBeta.4P(1000, .2, .95, 6, 4))
    }
    if(input$InclTcut == FALSE) {
      Tcut <- NULL
    } else {
      Tcut <- input$Tcut
    }
    ot <- LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp))
    ottec <- list("Model parameters:" = ot$parameters)
    print(ottec)
  })
  output$mdlfit <- renderPrint({
    if(!is.null(input$file1$datapath)) {
      dta <- read.csv(input$file1$datapath)
      dta <- as.vector(dta[, 1])
      if (class(dta[1]) == "character") {
        dta <- as.numeric(dta[-1])
      }
    } else {
      set.seed(123)
      dta <- rbinom(1000, 100, rBeta.4P(1000, .2, .95, 6, 4))
    }
    if(input$InclTcut == FALSE) {
      Tcut <- NULL
    } else {
      Tcut <- input$Tcut
    }
    ot <- LL.CA(dta, input$Reliability, input$Cut, min = input$Min, max = input$Max, true.model = input$Model, 
                truecut = Tcut, l = input$L, u = input$U, modelfit = c(input$nbins, input$minexp))
    ottec <- list("Model fit:" = ot$modelfit)
    print(ottec)
  })
}

# Run app
shinyApp(ui = ui, server = server)


# Update the application
#install.packages("rsconnect")
#library(rsconnect)
#rsconnect::deployApp(choose.dir())
