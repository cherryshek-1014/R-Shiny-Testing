# Please Install these specific packages 
#install.packages('ggExtra')
#install.packages('ggfortify')

library(ggplot2)
library(ggExtra)
library(ggfortify)

data = read.table('lvh.dat', header=TRUE)
data = sweep(data,2,c(0,0,1,1,1,0),'-')

cols_to_change = c(3:6)
for(i in cols_to_change){
  data[, i] = as.factor(data[,i])
}

attach(data)

ui <- fluidPage(
  navbarPage('ADA Shiny Assignment',

    tabPanel('Boxplot',
      titlePanel('Exploring Boxplots'),
  
      sidebarLayout(
        sidebarPanel (
          selectInput('variable','Select a factor:',c('Gender'='sex','Cardiac Disease'='card','Kidney Disease'='dgp','Death in 2 yrs'='surv')),
          selectInput('y_variable','Select a continous variable:', choices = c('left ventricular mass index'='llvmi' ,
                                                                              'systolic blood pressure'='sbp'))
        ),
    
    
        mainPanel(
          plotOutput("box")
        )
      )
    ),
  
    tabPanel('Density Plots',
      titlePanel('Exploring Density Plots'),
           
      sidebarLayout(
       sidebarPanel (
          selectInput('y_variable_den','Select a continous variable:', choices = c('left ventricular mass index'='llvmi' ,
                                                                              'systolic blood pressure'='sbp')),
          selectInput('variable_den','Select a factor:',choices = c('Gender','Cardiac Disease','Kidney Disease','Death in 2 yrs'))
       ),
             
        mainPanel(
         plotOutput("density") 
        )
      )
    ),
    
    tabPanel('Summary',
      titlePanel('Data Summary'),
      
      sidebarLayout(
        sidebarPanel(
          selectInput('variable_sum','select variable:',choices = c('Gender'='sex',
                                                                           'Cardiac Disease'='card',
                                                                           'Kidney Disease'='dgp',
                                                                           'Death in 2 yrs'='surv',
                                                                           'systolic blood pressure' = 'sbp',
                                                                           'left ventricular mass index'='llvmi'))
        ),
        mainPanel(
          verbatimTextOutput('summary')
        )
      )
    ),
    
    tabPanel('Marginal Histogram',
      titlePanel('Marginal Histogram'),
      
      sidebarLayout(
        sidebarPanel(
          radioButtons('xvar','Select x axis:',choices = c('Gender'='sex',
                                                                  'Cardiac Disease'='card',
                                                                  'Kidney Disease'='dgp',
                                                                  'Death in 2 yrs'='surv',
                                                                  'systolic blood pressure' = 'sbp',
                                                                  'left ventricular mass index'='llvmi'), selected = 'llvmi'),
                 
          radioButtons('yvar','Select y axis:',choices = c('Gender'='sex',
                                                                  'Cardiac Disease'='card',
                                                                  'Kidney Disease'='dgp',
                                                                  'Death in 2 yrs'='surv',
                                                                  'systolic blood pressure' = 'sbp',
                                                                  'left ventricular mass index'='llvmi'), selected = 'sbp'),
          sliderInput("xbin", "Select Binwidth for x:", min = 0.5, max = 30, value = 1),
          sliderInput("ybin", "Select Binwidth for y:", min = 0.5, max = 30, value = 1 , step = 0.1)
        ),
               
        mainPanel(
          plotOutput('margplot')
        )
      )
   ),
   tabPanel('Linear Regression:(llvmi as response)',
    titlePanel('Fitting Regression'),
            
    sidebarLayout(
      sidebarPanel (
        checkboxGroupInput('variable_lm','Variables to Select:',choices = list('Gender'='sex',
                                                                                       'Cardiac Disease'='card',
                                                                                       'Kidney Disease'='dgp',
                                                                                       'Death in 2 yrs'='surv',
                                                                                       'systolic blood pressure' = 'sbp'), selected = 'sex')
      ),
              
        mainPanel(
          h4('Model Summary'),
          verbatimTextOutput('sum'),
          h4('Diagnostic Plots'),
          plotOutput('plots'),
          plotOutput('hist')
        )
     )
   ),
   
   tabPanel('Linear Regression',
    titlePanel('Fitting Regression'),
            
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput('variable_m','Variables to Select:',choices = list('Gender'='sex',
                                                                                      'Cardiac Disease'='card',
                                                                                      'Kidney Disease'='dgp',
                                                                                      'Death in 2 yrs'='surv',
                                                                                      'systolic blood pressure' = 'sbp',
                                                                                      'left ventricular mass index'='llvmi'),selected ='sex'),
        radioButtons('response','Select Response:',choices = c('Gender'='sex',
                                                                       'Cardiac Disease'='card',
                                                                       'Kidney Disease'='dgp',
                                                                       'Death in 2 yrs'='surv',
                                                                       'systolic blood pressure' = 'sbp',
                                                                       'left ventricular mass index'='llvmi'), selected = 'llvmi'),
        
        radioButtons('Model','Select type of Model:',choices = c('Linear Regression'= 'gaussian',
                                                                         'Logistic Regression'='binomial'), selected = 'gaussian')
                
      ),
              
      mainPanel(
        h4('Model Summary'),
        verbatimTextOutput('sum_m'),
        h4('Diagnostic Plot'),
        plotOutput('plots_m')
          ) 
      
    )
   )
  )
)

#server.R#
server <- function(input, output) {
# Boxplot #  
  output$box <- renderPlot({
    
    ggplot(data,aes(data[,input$variable], data[,input$y_variable] )) + 
      geom_boxplot(fill = 'pink')+
      labs(x= input$variable ,y = input$y_variable)
  })
# Density Plot #  
  varinput = reactive({
    switch(input$variable_den,
           "Gender"=sex,
           "Cardiac Disease"=card,
           "Kidney Disease" = dgp,
           "Death in 2 yrs"= surv)
  })
  
  output$density <- renderPlot({
    cat = varinput()
    ggplot(data)+
      geom_density(aes(data[,input$y_variable_den],fill=factor(cat)), alpha=0.6)+
      labs(x=input$y_variable_den,
           fill = input$variable_den)
  })
  output$summary = renderPrint((
    summary(data[,input$variable_sum])
  ))
  
  output$margplot = renderPlot({
    ggMarginal(ggplot(data, aes(x=data[,input$xvar], y=data[,input$yvar])) +
               geom_jitter()+ labs(x=as.name(input$xvar), y=as.name(input$yvar)) , type = "histogram", fill="transparent" , 
               xparams = list(binwidth = input$xbin, fill='pink'),
               yparams = list(binwidth = input$ybin, fill='purple')
    )
  })
  
  mvar = reactive({
    c('llvmi',input$variable_lm)
  })
  
  mdata = reactive({
    mdata=data[,mvar()]
  })
  
  model = reactive({
    validate({
      need(length(input$variable_lm) > 0 , 'select at least one variable')
    })
    lm(llvmi~. , data=mdata())
  })
  
  output$sum = renderPrint({
    summary(model())
  })

  output$hist = renderPlot({
    ggplot(data,aes(resid(model()))) + 
      geom_histogram(col='black', fill='pink' ,binwidth = 0.1)+
      labs(x='residual')
  })
  
  output$plots = renderPlot({
    autoplot(model())
  })
  
  checking=function(model, response, variable){
    
  if (response %in% variable){
      'Response cannot be a preditor'
    } else if (model=='gaussian' & class(data[,response])== 'factor'){
      ' Response cannot be factor'
    } else if (model=='binomial' & class(data[,response])== 'numeric'){
      ' Response cannot be numeric'
    } else NULL
  }
  
  mvar_m = reactive({
    validate({
      checking(input$Model,input$response, input$variable_m)
    })
    c(input$response,input$variable_m)
  })
  
  mdata_m = reactive({
    data[,mvar_m()]
  })
  
  model_m = reactive({
    validate({
      need(length(input$variable_m) > 0 , 'select at least one variable')
    })
    glm(as.formula(paste(input$response,'~',paste(input$variable_m,collapse='+'))),family=input$Model, data=mdata_m())
  })
  
  output$sum_m = renderPrint({
    summary(model_m())
  })

  output$plots_m = renderPlot({
    autoplot(model_m())
  })
  
}


shinyApp(ui = ui, server = server)

