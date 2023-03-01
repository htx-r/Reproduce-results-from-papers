library(xlsx)
library(readxl)
library(shiny)
library(shinythemes)
library(gridExtra)
library(ggpubr)
library(ggplot2)
library(tidyverse)

#write.xlsx(GraphdataL,"Data for the Graph_IPDAD.xls",row.names = F)
#GraphdataF<-read_excel("C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/ThreeStageModelRRMS/Results/Data for the Graph_IPDAD.xls")
#Graphdata1<-GraphdataF[,3]
#Graphdata2<-GraphdataF[,2]
#Graphdata3<-GraphdataF[,1]
#Graphdata<-cbind(Graphdata1,Graphdata2,Graphdata3)
#colnames(Graphdata)<-c("Treatment", "Predicted probability to relapse within the next 2 years %", "Baseline risk score")
#write.csv(Graphdata,"Data for the Graph_IPDAD.csv",row.names = F

GraphdataF<-read_csv("Data for the Graph_IPDAD.csv")
colnames(GraphdataF)<-c("Treatment","Predicted probability to relapse within the next 2 years %", "Baseline risk score")
#GraphdataF$`Predicted probability to relapse within the next 2 years %`<-round(GraphdataF$`Predicted probability to relapse within the next 2 years %`,1)
GraphdataF$Treatment<-as.character(GraphdataF$Treatment)
GraphdataF$Treatment[grepl('Glateramere Acetate', GraphdataF$Treatment)] <- 'Glatiramer Acetate'
GraphdataF$Treatment<-as.factor(GraphdataF$Treatment)

#GraphdataF$Treatment[which(GraphdataF$Treatment=="Glateramere Acetate")]<-c("Glatiramer Acetate")
server <- function(input, output, session) {

  data <- reactive({
    GraphdataF
  })
  log.risk.score=reactive(-1.137-0.025*(input$Age-37.04233)+0.237*(log(input$DiseaseDuration+10)-2.824242)+0.265*(input$Edss-2.390698)+0.217*GdLesions()+NrRelapses()-0.335*(log(input$MonthsSinceLastRelapse+10)-2.774328)-0.244*TrNaive()+0.178*Gender())
  risk.score1 = reactive(exp(log.risk.score())/(1+exp(log.risk.score())))
  risk.score2=reactive(round(risk.score1(),2))
  risk.score=reactive(risk.score2())
  risk.score3=reactive(risk.score()*100)
  output$plot <- renderPlot({
    ggplot(data(), aes(x=`Baseline risk score`, y=`Predicted probability to relapse within the next 2 years %`, group=Treatment)) +
      geom_line(aes(color=Treatment))+
      geom_point(aes(color=Treatment))+geom_vline(xintercept=risk.score(), color="blue")+labs( x="Baseline risk score")+labs( y="Probability to relapse within the next 2 years (%)")+
      theme( text = element_text(size = 16), panel.background = element_rect(fill = "white",
                                                                             colour = "lightblue",
                                                                             size = 0.5, linetype = "solid"),
             panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                             colour = "white"),
             panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                             colour = "white")
      )# theme(axis.text.x = element_text(size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #      axis.text.y = element_text(size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),
    #        axis.title.x = element_text(size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
    #       axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

    # +xlab("Baseline risk score")+ylab("Predicted probability to relapse in 2 years")


  })

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
      NrRelapses <- -0.049
    }
    else {
      NrRelapses <- 0.093
    }
    NrRelapses
  })



  table0<- reactive(GraphdataF[which(GraphdataF$`Baseline risk score` == risk.score()),])
  table1<- reactive(as.data.frame(table0()$Treatment))
  table2<- reactive(as.data.frame(round(table0()$`Predicted probability to relapse within the next 2 years %`,0)))
  table3<- reactive(table0()[,3])

  table4<-reactive(table0()[order(table0()$`Predicted probability to relapse within the next 2 years %`),])



  output$Ranking.Probabilities1 <- renderText({
    paste(table4()[1,1], "with", round(table4()[1,2]*100,2), "% probability to relapse." )

  })
  output$Ranking.Probabilities2 <- renderText({
    paste(table4()[2,1], "with ", round(table4()[2,2]*100,2), "% probability to relapse.")

  }
  )
  output$Ranking.Probabilities3 <- renderText({
    paste(table4()[3,1], "with", round(table4()[3,2]*100,2), "% probability to relapse.")

  }
  )

  output$Ranking.Probabilities4 <- renderText({
    paste(table4()[4,1], "with", round(table4()[4,2]*100,2), "% probability to relapse")

  }
  )

  #output$Predicted.Probabilities <- renderText({
  # paste("Your predicted probabilities to relapse in two years under each one of the treatments are:", cbind(as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),1]), as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),2]))
  #   )
  #})

  output$final.risk.score <- renderText({
    paste("Your baseline risk score is", risk.score()*100,"%")
  })


  #output$best.treatment<- renderText({
  #paste("The optimal treatment is", table[order(table[,2]),3])
  #})
}

ui <-  fluidPage(theme=shinytheme("readable"),
                 titlePanel(h1("Prevention of relapses in patients with Relapsing-Remitting Multiple Sclerosis")),
                 titlePanel(h4("(Estimation through several data sources)")), # using strong as a direct tag
                 #h1("Using textInput and checkboxInput")
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId = "Age",
                                 label = "Age (years)",
                                 value = 30, min = 17, max = 80),
                     radioButtons(inputId="Gender", label="Select Gender:",
                                  choices =list( "Male", "Female")),
                     numericInput(inputId="DiseaseDuration", label="Disease Duration (years)",value=1, min=0,max=70, step=0.1),
                     sliderInput(inputId="Edss", label="Expanded Disability Status Scale (EDSS)",value=1, min=0,max=7,step=0.5),
                     #checkboxInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions > 0"),
                     numericInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions",value=0, min=0,max=18, step=1),

                     #checkboxInput(inputId="NrRelapses1", label="1 prior relapse"),
                     #checkboxInput(inputId="NrRelapses2", label="2 or more prior relapses"),
                     radioButtons(inputId="NrRelapses", label="Number of prior relapses",
                                  choices =list( "One", "More than one")),

                     numericInput(inputId="MonthsSinceLastRelapse", label="Months since last relapse",value=1, min=0,max=600, step=0.1),

                     radioButtons(inputId="TrNaive", label="I had a MS treatment before",
                                  choices =list( "Yes", "No"))
                     #checkboxInput(inputId="TrNaive", label="Treatment Naive"),
                     ),

                   mainPanel( h5(textOutput("final.risk.score")),
                              h4("Plot of predicted probabilities to relapse within the next two years"),plotOutput("plot"),
                              h4("Ranking of predicted probabilities to relapse within the next two years"),
                              h6("1. The lowest probability to relapse is under treatment:"), textOutput("Ranking.Probabilities1"), #, textOutput("best.treatment")
                              h6("2. Second best choice based on the probability to relapse:"),textOutput("Ranking.Probabilities2"),
                              h6("3. The treatment that follows is:") ,textOutput("Ranking.Probabilities3"),
                              h6("4. The treatment with the highest probability to relapse is:") ,textOutput("Ranking.Probabilities4")
                   )
                 )
)


shinyApp(ui = ui, server = server)





