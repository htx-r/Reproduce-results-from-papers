#library(xlsx)
#GraphdataF<-read_excel("C:/Users/kc19o338/Desktop/Real world predictions project/Data for the Graph2.xls")
#class(GraphdataF$`Baseline Risk`)
#GraphdataF$`Baseline Risk`<-as.integer(GraphdataF$`Baseline Risk`)
#Graphdata1<-GraphdataF[,3]
#Graphdata2<-GraphdataF[,2]
#round(GraphdataF[,2],0)
#Graphdata3<-GraphdataF[,1]
#Graphdata<-cbind(Graphdata1,Graphdata2,Graphdata3)
#colnames(Graphdata)<-c("Treatment", "Predicted probability to relapse in 2 years %", "Baseline risk score")
#GraphdataF<-Graphdata
#write.xlsx(Graphdata,"Data for the Graph7.xls",row.names = F)
library(readxl)
library(shiny)
library(shinythemes)
library(gridExtra)
library(ggpubr)
library(ggplot2)
GraphdataF<-read.csv("C:/Users/kc19o338/shinies/apps/koms/Data_for_the_Graph7.csv")
colnames(GraphdataF)<-c("Treatment","Predicted probability to relapse in 2 years %", "Baseline risk score")
GraphdataF$`Predicted probability to relapse in 2 years %`<-round(GraphdataF$`Predicted probability to relapse in 2 years %`,1)
GraphdataF$Treatment<-as.character(GraphdataF$Treatment)
GraphdataF$Treatment[grepl('Glateramere Acetate', GraphdataF$Treatment)] <- 'Glatiramer Acetate'
GraphdataF$Treatment<-as.factor(GraphdataF$Treatment)

#GraphdataF$Treatment[which(GraphdataF$Treatment=="Glateramere Acetate")]<-c("Glatiramer Acetate")
server <- function(input, output, session) {

  data <- reactive({
    GraphdataF
  })
  log.risk.score=reactive(-0.8656-0.0181*input$Age - 0.1379*input$SEX  + 0.1683*input$EDSSBL + 0.0587*log(input$ONSYRS+1) - 0.0142*input$RACE
                          +0.5963*log(input$RLPS1YR+1) - 0.0126 *input$TRELMOS + 0.1901*input$PRMSGR - 0.1718*log(input$T25FWABL+1)
                          +0.3022*log(input$NHPTMBL+1)+0.0029*input$PASATABL-0.0010*input$VFT25BL-0.0195*input$SFPCSBL+0.0036*input$SFMCSBL)
  risk.score1 = reactive(exp(log.risk.score())/(1+exp(log.risk.score())))
  risk.score2=reactive(round(risk.score1(),2))
  risk.score=reactive(risk.score2()*100)
  output$plot <- renderPlot({
    ggplot(data(), aes(x=`Baseline risk score`, y=`Predicted probability to relapse in 2 years %`, group=Treatment)) +
      geom_line(aes(color=Treatment))+
      geom_point(aes(color=Treatment))+geom_vline(xintercept=risk.score(), color="blue")+labs( x="Baseline risk score")+labs( y="Predicted probability to relapse in 2 years %")+
      theme(text = element_text(size = 17))# theme(axis.text.x = element_text(size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #      axis.text.y = element_text(size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),
    #        axis.title.x = element_text(size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
    #       axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

    # +xlab("Baseline risk score")+ylab("Predicted probability to relapse in 2 years")


  })

  table0<- reactive(GraphdataF[which(as.integer(GraphdataF$`Baseline risk score`) == as.integer(risk.score())),])
  table1<- reactive(as.data.frame(table0()$Treatment))
  table2<- reactive(as.data.frame(round(table0()$`Predicted probability to relapse in 2 years %`,0)))
  table3<- reactive(table0()[,3])

  table4<-reactive(table0()[order(table0()$`Predicted probability to relapse in 2 years %`),])

  output$Predicted.Probabilities <- renderText({
    paste(
      table1()[1,1], "-", table2()[1,1], "% / ",
      table1()[2,1], "-", table2()[2,1], "% /",
      table1()[3,1], "-", table2()[3,1], "% / ",
      table1()[4,1], "-", table2()[4,1], "% ")
  })

  output$Ranking.Probabilities1 <- renderText({
    paste(table4()[1,1], "with", table4()[1,2], "% probability to relapse." )

  })
  output$Ranking.Probabilities2 <- renderText({
    paste(table4()[2,1], "with ", table4()[2,2], "% probability to relapse.")

  }
  )
  output$Ranking.Probabilities3 <- renderText({
    paste(table4()[3,1], "with", table4()[3,2], "% probability to relapse.")

  }
  )

  output$Ranking.Probabilities4 <- renderText({
    paste(table4()[4,1], "with", table4()[4,2], "% probability to relapse")

  }
  )

  #output$Predicted.Probabilities <- renderText({
  # paste("Your predicted probabilities to relapse in two years under each one of the treatments are:", cbind(as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),1]), as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),2]))
  #   )
  #})

  output$final.risk.score <- renderText({
    paste("Your baseline risk score is", as.integer(risk.score()))
  })


  #output$best.treatment<- renderText({
  #paste("The optimal treatment is", table[order(table[,2]),3])
  #})
}

ui <-  fluidPage(theme=shinytheme("readable"),
                 titlePanel(h1("Prevention of relapses in patients with Relapsing-Remitting Multiple Sclerosis")), # using strong as a direct tag
                 #h1("Using textInput and checkboxInput")
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId = "Age",
                                 label = "Age (years)",
                                 value = 1, min = 18, max = 60),
                     checkboxInput(inputId="SEX", label="Male"),
                     checkboxInput(inputId="RACE", label="White"),
                     checkboxInput(inputId="PRMSGR", label="Prior treatment"),
                     sliderInput(inputId="EDSSBL", label="Baseline EDSS",value=1, min=0,max=6,step=0.5),
                     sliderInput(inputId="ONSYRS", label="Years since onset of symptoms",value=1, min=0,max=45),
                     sliderInput(inputId="RLPS1YR", label="Number of relapses the last 1 year",value=1, min=0,max=20),
                     numericInput(inputId="TRELMOS", label="Months since last relapse",value=1, min=0,max=200),
                     numericInput(inputId="T25FWABL", label="Baseline Timed 25-Foot Walk",value=1, min=0,max=200, step=0.1),
                     sliderInput(inputId="PASATABL", label="Baseline PASAT 3",value=1, min=0,max=80),
                     sliderInput(inputId="VFT25BL", label="Baseline VFT 2.5%",value=1, min=0,max=60),
                     numericInput(inputId="NHPTMBL", label="Baseline 9 Hole Peg Test",value=1, min=0,max=300, step=0.1),
                     numericInput(inputId="SFPCSBL", label="Baseline SF-36 PCS",value=1, min=0,max=80, step=0.01),
                     numericInput(inputId="SFMCSBL", label="Baseline SF-36 MCS",value=1, min=0,max=80, step=0.001)),

                   mainPanel( h5(textOutput("final.risk.score")),
                              h4("Plot of predicted probabilities to relapse in two years"),plotOutput("plot"),
                              h4("Predicted probabilities to relapse in two years"), textOutput("Predicted.Probabilities"),
                              h4("Ranking of predicted probabilities to relapse in two years"),
                              h6("1. The lowest probability to relapse is under treatment:"), textOutput("Ranking.Probabilities1"), #, textOutput("best.treatment")
                              h6("2. Second best choice based on the probability to relapse:"),textOutput("Ranking.Probabilities2"),
                              h6("3. The treatment that follows is:") ,textOutput("Ranking.Probabilities3"),
                              h6("4. The treatment with the highest probability to relapse is:") ,textOutput("Ranking.Probabilities4")
                   )
                 )
)


shinyApp(ui = ui, server = server)





