#
# This is the user-interface portion of the shiny-app.


library(shiny)
library(survival)
shinyUI(fluidPage(
  titlePanel("Do Induced Abortions Cause Infertility as or more often than Miscarriages?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderind", "How many prior Induced Abortions?", 0, 2, value = 1),
      sliderInput("sliderspont", "How many prior Miscarriages?", 0, 2, value = 1),
      sliderInput("sliderstra","3 Cluster Strata Number",1,83,value=1),
      checkboxInput("showModel1", "Show/Hide Fixed Miscarriage Model", value = TRUE),
      checkboxInput("showModel2", "Show/Hide Fixed Induced Abortion Model", value = TRUE), 
submitButton("Submit")),
    mainPanel("Many people fear that induced abortions may be as or more dangerous in terms of losing fertility as having a miscarriage (spontaneous abortion). In this data, we explore whether this may be the case. The data are from the base R package (infert), and you can learn more about the data set from typing ?infert, or visiting https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html. Further, a reproducible pitch for the app can be found on http://rpubs.com/easoneli17/271650, and the code with documentation for each part of the app is available on github at this address.",
      h3("Odds of Infertility from a Logistic Regression Model"),
      textOutput("pred1"),
      h3("Odds of Infertility From a Clogit Model"),
      textOutput("pred2"),
      #Note that the slope of these plots indicates the effect of abortions on fertility.
      #A steeper slope indicates that the variable that is not fixed by the slider has
      #a greater risk for causing a patient to become infertile.
      plotOutput("plot1"),
      plotOutput("plot2"),
      h3("95% Confidence Interval from Logistic Regression Model"),
      textOutput("int1")
    )
  )
))
