#
# This app begins to explore the question of whether induced abortions are actually
# as dangerous as miscarriages in terms of the patient becoming infertile. The data are
# available from R Studio, and more information can be found about the data by using 
# ?infert, or https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html

library(shiny)
library(survival)
shinyServer(function(input, output) {
  #This first model is a basic logistic regression using the number of spontaneous
  #abortions (i.e., miscarriages) and the number of induced abortions to predict whether 
  #the patient became infertile (case=1) or remained fertile (case=0). The number of both 
  #types of abortions is coded as 0, 1, or 2 for 2 or more.
  model1 <- glm(case ~ spontaneous+induced,data = infert, family = binomial("logit"))
  #This second model is the suggested method for modelling the data using 
  #conditional logistic regression since the data were chosen in pairs.
  model2 <- clogit(case ~ spontaneous+induced+strata(stratum), data = infert)
  
  #This calculates the predicted log odds of infertility by model 1 based on the slider
  #values of the number of previous induced/spontaneous abortions. Then, it takes that 
  #prediction and converts it to the actual odds.
  model1pred <- reactive({
    indInput <- input$sliderind
    spontInput <- input$sliderspont
    exp(predict(model1, newdata = data.frame(list(induced=indInput,spontaneous=spontInput))))
  })

  #This calculates the predicted log odds of infertility by model 2 based on the slider
  #values of the number of previous induced/spontaneous abortions. Then, it takes that 
  #prediction and converts it to the actual odds.  
  model2pred <- reactive({
    indInput <- input$sliderind
    spontInput <- input$sliderspont
    straInput<-input$sliderstra
    exp(predict(model2, newdata = data.frame(list(induced=indInput,spontaneous=spontInput,stratum=straInput)),reference="strata"))
  })

  #This calculates the 95% prediction interval for the predicted log odds of infertility 
  #by model 1 based on the slider values of the number of previous induced/spontaneous 
  #abortions. Then, it takes that prediction interval and converts both ends to the 
  #actual odds. Note that a prediction interval is the prediction for a single woman whereas
  #a confidence interval predicts the mean value for all women with these numbers of prior
  #spontaneous and induced abortions.
  model1predint <- reactive({
    indInput <- input$sliderind
    spontInput <- input$sliderspont
    critval<-1.96
    fit<-predict(model1, newdata = data.frame(list(induced=indInput,spontaneous=spontInput)),interval="prediction",type="link",se.fit=T)$fit
    se<-predict(model1, newdata = data.frame(list(induced=indInput,spontaneous=spontInput)),interval="prediction",type="link",se.fit=T)$se.fit
    exp(c(fit-se*critval,fit+se*critval))
  })

  #This first plot takes the input from the slider for the number of spontanteous
  #abortions, then subsets the data to use only the cases with the specified number
  #of spontaneous abortions and builds a logistic regression model from that data
  #using the number of induced abortion to predict infertility.
  output$plot1 <- renderPlot({
    if(input$showModel1){
    spontInput <- input$sliderspont 
    newdata<- infert[ which(infert$spontaneous==spontInput), ]
    g=glm(case~induced,family=binomial("logit"),newdata)
    plot(newdata$case~newdata$induced,xlab="Number of Induced Abortions",ylab="Probability of Infertility") 
    lines(newdata$induced, g$fitted, type="l", col="red")
    title(main="GLM with fixed number of Miscarriages")}
  })

  #This next plot takes the input from the slider for the number of induced
  #abortions, then subsets the data to use only the cases with the specified number
  #of induced abortions and builds a logistic regression model from that data
  #using the number of spontaneous abortion to predict infertility.  
  output$plot2 <- renderPlot({
    if(input$showModel2){
    indInput <- input$sliderind 
    newdata<- infert[ which(infert$induced==indInput), ]
    g=glm(case~spontaneous,family=binomial("logit"),newdata)
    plot(newdata$case~newdata$spontaneous,xlab="Number of Miscarriages",ylab="Probability of Infertility") 
    lines(newdata$spontaneous, g$fitted, type="l", col="red")
    title(main="GLM with fixed number of Induced Abortions")}
  })
  
  output$pred1 <- renderText({
    model1pred()
  })
  
  output$pred2 <- renderText({
    model2pred()
  })
  
  output$int1 <- renderText({
    model1predint()
  })

})