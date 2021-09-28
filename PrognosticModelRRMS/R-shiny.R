library(shiny)
library(shinythemes)
library(ggplot2)
server <- function(input, output, session) {


  log.risk.score=reactive(-1.942+0.294872-0.034*(input$Age-42.65153)+0.320*(log(input$DiseaseDuration+10)-2.9947)+0.112*(input$EDSS-2.458)+0.004*GdLesions()+NrRelapses()-0.502*(log(input$MonthsSinceLastRelapse+10)-3.80)+0.137*TrNaive()+0.245*Gender()-0.22*TreatNow())
  risk.score1 = reactive(exp(log.risk.score())/(1+exp(log.risk.score())))
  risk.score2=reactive(round(risk.score1(),2))
  risk.score=reactive(risk.score2()*100)

  data <- reactive({



    y<-risk.score()
    z<-(c("1"))
    z<-as.factor(z)
    z<-as.data.frame(z)
    m<-as.data.frame(cbind(y,z))
    colnames(m)<-c("Probability (%) of relapsing within the next 2 years", "Category")
    m<-as.data.frame(m)

  })

 # Gender <- reactive({
  #  switch(input$Gender, "female"=1, "male"=0)
  #})
  Gender <- reactive({
    if(input$Gender == "Female") {
      Gender <- 1
    } else {
      Gender <- 0
    }
    Gender
  })

  TrNaive <- reactive({
    if(input$TrNaive == "No") {
      TrNaive <- 1
    } else {
      TrNaive <- 0
    }
    TrNaive
  })

  GdLesions <- reactive({
    if(input$GdLesions>0) {
      GdLesions <- 1
    } else {
      GdLesions <- 0
    }
    GdLesions
  })


  NrRelapses <- reactive({
   if(input$NrRelapses=="One") {
     NrRelapses <- -0.093
       }
    else {
         NrRelapses <- 0.108
    }
     NrRelapses
  })

  TreatNow <- reactive({
    if(input$TreatNow == "Yes") {
      TreatNow <- 1
    } else {
      TreatNow <- 0
    }
    TreatNow
  })

  output$final.risk.score <- renderText({
    paste("Probability of experiencing a relapse within the next 2 years:", as.integer(risk.score()), "%")
  })

  output$plot <- renderPlot({
    ggplot(data(), aes(x=`Category`, y=`Probability (%) of relapsing within the next 2 years`)) + ylim(0,50) +
      geom_bar(stat="identity", position=position_dodge(), color=c("orange"), fill=c("orange"),width=0.1)+  theme(text = element_text(size = 17))+labs(x="") +
      theme(axis.text.x = element_text(size=20)) + scale_x_discrete(labels=c(paste("Somebody with", "your", "characteristics"))) + ggtitle("")


  })


  output$final.text2 <- renderText({
    paste("Somebody with your characteristics has", risk.score(), "% probability (%) of relapsing within the next 2 years")
  })

  output$final.text <- renderText({


    paste("")

  })

}

ui <-  fluidPage(theme=shinytheme("readable"),
                 titlePanel(h1("Prognosis for patients with Relapsing-Remitting Multiple Sclerosis")),
                 titlePanel(h4("Estimated from the Swiss MS Cohort")),
                 # using strong as a direct tag
                 #h1("Using textInput and checkboxInput")
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId = "Age",
                                 label = "Age (years)",
                                 value = 30, min = 17, max = 80),
                     radioButtons(inputId="Gender", label="Select Gender:",
                                  choices =list( "Male", "Female")),
                     numericInput(inputId="DiseaseDuration", label="Disease Duration (years)",value=1, min=0,max=70, step=0.1),
                     sliderInput(inputId="EDSS", label="Expanded Disability Status Scale (EDSS)",value=1, min=0,max=7,step=0.5),
                     #checkboxInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions > 0"),
                     numericInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions",value=0, min=0,max=18, step=1),

                     #checkboxInput(inputId="NrRelapses1", label="1 prior relapse"),
                     #checkboxInput(inputId="NrRelapses2", label="2 or more prior relapses"),
                     radioButtons(inputId="NrRelapses", label="Number of prior relapses",
                                  choices =list( "One", "More than one")),

                     numericInput(inputId="MonthsSinceLastRelapse", label="Months since last relapse",value=1, min=0,max=600, step=0.1),
                     radioButtons(inputId="TreatNow", label="I am currently on MS treatment",
                                  choices =list( "Yes", "No")),
                     radioButtons(inputId="TrNaive", label="I had a MS treatment before",
                                  choices =list( "Yes", "No"))
                     #checkboxInput(inputId="TrNaive", label="Treatment Naive"),
                     #checkboxInput(inputId="Gender", label="Female")),
                   ),



                   mainPanel(  span(h3(textOutput("final.risk.score")),style="color:blue"),
                               plotOutput("plot"),
                               span(h3(textOutput("final.text")),style="color:green"),
                   )
                 )
)


shinyApp(ui = ui, server = server)





