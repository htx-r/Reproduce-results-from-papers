library(readxl)
library(shiny)
library(shinythemes)
library(gridExtra)
library(ggpubr)
library(ggplot2)

#write.xlsx(GraphdataL,"Data for the Graph_IPDAD.xls",row.names = F)
#GraphdataF<-read_excel("C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/ThreeStageModelRRMS/Results/Data for the Graph_IPDAD.xls")
#Graphdata1<-GraphdataF[,3]
#Graphdata2<-GraphdataF[,2]
#Graphdata3<-GraphdataF[,1]
#Graphdata<-cbind(Graphdata1,Graphdata2,Graphdata3)
#colnames(Graphdata)<-c("Treatment", "Predicted probability to relapse within the next 2 years %", "Baseline risk score")
#write.csv(Graphdata,"Data for the Graph_IPDAD.csv",row.names = F

GraphdataF<-read.csv("Data for the Graph_IPDAD.csv")
colnames(GraphdataF)<-c("Treatment","Predicted probability of relapsing within the next two years %", "Baseline risk")
#GraphdataF$`Predicted probability to relapse within the next 2 years %`<-round(GraphdataF$`Predicted probability to relapse within the next 2 years %`,1)
GraphdataF$Treatment<-as.character(GraphdataF$Treatment)
#GraphdataF$Treatment[grepl('Glateramere Acetate', GraphdataF$Treatment)] <- 'Glatiramer Acetate'
GraphdataF$Treatment<-as.factor(GraphdataF$Treatment)


Graphdata_SMSC<-read.csv("GraphData_SMSC.csv")
#Graphdata_SMSC<-Graphdata_SMSC[,c(2,3,4)]
colnames(Graphdata_SMSC)<-c("Treatment","Predicted probability of relapsing within the next two years %", "Baseline risk")
#Graphdata_SMSC<-Graphdata_SMSC[,c(3,2,1)]
#Graphdata_SMSC$Treatment[grepl('Placebo', Graphdata_SMSC$Treatment)] <- 'untreated'
Graphdata_SMSC$Treatment<-as.factor(Graphdata_SMSC$Treatment)


#C:/Users/kc19o338/D ocuments/GitHub/shinies/apps/test/GraphData_SMSC.csv
#GraphdataF$Treatment[which(GraphdataF$Treatment=="Glateramere Acetate")]<-c("Glatiramer")
server <- function(input, output, session) {

  data <- reactive({
    GraphdataF
  })

  data_SMSC <- reactive({
    Graphdata_SMSC
  })
  df_x <- reactive({tibble(
    x = c(1, 2),
    ymax = c(0.5, 0.8),
    ymin = c(0.4, 0.3)
  ) })
  log.risk.score=reactive(-1.137-0.025*(input$Age-37.04233)+0.237*(log(input$DiseaseDuration+10)-2.824242)+0.265*(input$Edss-2.390698)+0.217*GdLesions()+NrRelapses()-0.335*(log(input$MonthsSinceLastRelapse+10)-2.774328)-0.244*TrNaive()+0.178*Gender())
  risk.score1 = reactive(exp(log.risk.score())/(1+exp(log.risk.score())))
  risk.score2=reactive(round(risk.score1(),2))
  risk.score=reactive(risk.score2())
  risk.score3=reactive(risk.score()*100)
  output$plot <- renderPlot({
    ggplot(data(), aes(x=`Baseline risk`, y=`Predicted probability of relapsing within the next two years %`, group=Treatment)) +
      geom_line(aes(color=Treatment))+geom_vline(xintercept=0.1265625, linetype="dashed", color = "red")+
      geom_vline(xintercept=0.7040377, linetype="dashed", color = "red")+
      geom_point(aes(color=Treatment))+geom_vline(xintercept=risk.score(), color="blue")+labs( x="Baseline risk")+labs( y="Probability of relapsing within the next two years")+
      geom_rect(aes(xmin=0, xmax=0.1265625, ymin=-Inf,ymax=Inf), alpha=0.01, fill="grey")+  geom_rect(aes(xmin=0.7040377, xmax=1, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey")+
      theme_minimal()    # theme( text = element_text(size = 16), panel.background = element_rect(fill = "white",
    #                                                                      colour = "lightblue",
    #                                                                     size = 0.5, linetype = "solid"),
    #   panel.grid.major = element_line(size = 0.8, linetype = 'solid',
    #                                    colour = "white"),
    #   panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
    #                                colour = "white"))#+
    #+
    # geom_rect(data=data_background(), mapping=aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4)
    # theme(axis.text.x = element_text(size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #      axis.text.y = element_text(size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),
    #        axis.title.x = element_text(size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
    #       axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

    # +xlab("Baseline risk score")+ylab("Predicted probability to relapse in 2 years")
    #geom_rect(aes(xmin=0, xmax=0.1265625, ymin=-Inf,ymax=Inf), alpha=0.01, fill="grey")+  geom_rect(aes(xmin=0.7040377, xmax=1, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey")+


  })

  log.risk.score_SMSC=reactive(-1.137-0.025*(input$Age_SMSC-37.04233)+0.237*(log(input$DiseaseDuration_SMSC+10)-2.824242)+0.265*(input$Edss_SMSC-2.390698)+0.217*GdLesions_SMSC()+NrRelapses_SMSC()-0.335*(log(input$MonthsSinceLastRelapse_SMSC+10)-2.774328)-0.244*TrNaive_SMSC()+0.178*Gender_SMSC())
  risk.score1_SMSC = reactive(exp(log.risk.score_SMSC())/(1+exp(log.risk.score_SMSC())))
  risk.score2_SMSC=reactive(round(risk.score1_SMSC(),2))
  risk.score_SMSC=reactive(risk.score2_SMSC())
  risk.score3_SMSC=reactive(risk.score_SMSC()*100)

  output$plot_SMSC<- renderPlot({
    ggplot(data_SMSC(), aes(x=`Baseline risk`, y=`Predicted probability of relapsing within the next two years %`, group=Treatment)) +
      geom_line(aes(color=Treatment))+geom_vline(xintercept=0.03925, linetype="dashed", color = "red")+
      geom_vline(xintercept=0.66126, linetype="dashed", color = "red")+
      geom_point(aes(color=Treatment))+geom_vline(xintercept=risk.score_SMSC(), color="blue")+labs( x="Baseline risk")+labs( y="Probability of relapsing within the next two years")+
      geom_rect(aes(xmin=0, xmax=0.03925, ymin=-Inf,ymax=Inf), alpha=0.01, fill="grey")+  geom_rect(aes(xmin=0.66126, xmax=1, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey")+
      theme_minimal()
    # theme( text = element_text(size = 16), panel.background = element_rect(fill = "white",
    #                                                                       colour = "lightblue",
    #                                                                      size = 0.5, linetype = "solid"),
    #     panel.grid.major = element_line(size = 0.8, linetype = 'solid',
    #                                    colour = "white"),
    #   panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
    #                                  colour = "white")
    #)# theme(axis.text.x = element_text(size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #      axis.text.y = element_text(size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),
    #        axis.title.x = element_text(size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
    #       axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"))

    # +xlab("Baseline risk score")+ylab("Predicted probability to relapse in 2 years")
    #geom_rect(aes(xmin=0, xmax=0.03925, ymin=-Inf,ymax=Inf), alpha=0.01, fill="grey")+  geom_rect(aes(xmin=0.66126, xmax=1, ymin=-Inf, ymax=Inf), alpha=0.01, fill="grey")+


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

  Gender_SMSC <- reactive({
    if(input$Gender_SMSC == "Female") {
      Gender_SMSC <- 1
    } else {
      Gender_SMSC <- 0
    }
    Gender_SMSC
  })

  TrNaive_SMSC <- reactive({
    if(input$TrNaive_SMSC == "No") {
      TrNaive_SMSC <- 1
    } else {
      TrNaive_SMSC <- 0
    }
    TrNaive_SMSC
  })

  GdLesions_SMSC <- reactive({
    if(input$GdLesions_SMSC>0) {
      GdLesions <- 1
    } else {
      GdLesions_SMSC <- 0
    }
    GdLesions_SMSC
  })


  NrRelapses_SMSC <- reactive({
    if(input$NrRelapses_SMSC=="One") {
      NrRelapses_SMSC <- -0.049
    }
    else {
      NrRelapses_SMSC <- 0.093
    }
    NrRelapses_SMSC
  })





  table0<- reactive(GraphdataF[which(GraphdataF$`Baseline risk` == risk.score()),])
  table1<- reactive(as.data.frame(table0()$Treatment))
  table2<- reactive(as.data.frame(round(table0()$`Predicted probability of relapsing within the next two years %`,0)))
  table3<- reactive(table0()[,3])

  table4<-reactive(table0()[order(table0()$`Predicted probability of relapsing within the next two years %`),])

  table0_SMSC<- reactive(Graphdata_SMSC[which(Graphdata_SMSC$`Baseline risk` == risk.score_SMSC()),])
  table1_SMSC<- reactive(as.data.frame(table0_SMSC()$Treatment))
  table2_SMSC<- reactive(as.data.frame(round(table0_SMSC()$`Predicted probability of relapsing within the next two years %`,0)))
  table3_SMSC<- reactive(table0_SMSC()[,3])

  table4_SMSC<-reactive(table0_SMSC()[order(table0_SMSC()$`Predicted probability of relapsing within the next two years %`),])

  output$Placebo <- renderText(paste("With placebo, the predicted probability of relapsing within the next two years is",round(table0()[4,2]*100,1), "%. This probability can be modified with an active treatment as follows:"))
  output$Ranking.Probabilities1 <- renderText({
    paste("With", table4()[1,1], "the predicted probability of relapsing within the next two years will be", round(table4()[1,2]*100,1), "%." )

  })
  output$Ranking.Probabilities2 <- renderText({
    paste("With",table4()[2,1], "the predicted probability of relapsing within the next two years will be", round(table4()[2,2]*100,1), "%.")

  }
  )
  output$Ranking.Probabilities3 <- renderText({
    paste("With",table4()[3,1], "the predicted probability of relapsing within the next two years will be", round(table4()[3,2]*100,1), "%.")

  }
  )

  output$Ranking.Probabilities4 <- renderText({
    paste(table4()[4,1], "with", round(table4()[4,2]*100,1), "% probability of relapsing")

  }
  )
  output$Untreated <- renderText(paste("Without treatment, the predicted probability of relapsing within the next two years is",round(table0_SMSC()[4,2]*100,1), "%. This probability can be modified with an active treatment as follows:"))

  output$Ranking.Probabilities1_SMSC <- renderText({
    paste("With", table4_SMSC()[1,1], "the predicted probability of relapsing within the next two years will be", round(table4_SMSC()[1,2]*100,1), "%." )

  })
  output$Ranking.Probabilities2_SMSC <- renderText({
    paste("With",table4_SMSC()[2,1], "the predicted probability of relapsing within the next two years will be", round(table4_SMSC()[2,2]*100,1), "%.")

  }
  )
  output$Ranking.Probabilities3_SMSC <- renderText({
    paste("With",table4_SMSC()[3,1], "the predicted probability of relapsing within the next two years will be", round(table4_SMSC()[3,2]*100,1), "%.")

  }
  )

  output$Ranking.Probabilities4_SMSC <- renderText({
    paste(table4_SMSC()[4,1], "with", round(table4_SMSC()[4,2]*100,2), "% probability of relapsing")

  }
  )



  #output$Predicted.Probabilities <- renderText({
  # paste("Your predicted probabilities to relapse in two years under each one of the treatments are:", cbind(as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),1]), as.data.frame(GraphdataF[which(GraphdataF$`Baseline risk score`==38),2]))
  #   )
  #})

  output$baseline.risk <- renderText({
    if (risk.score()<=0.30) {"You are in the lowest quantile out of the four risk groups."}
    else if (risk.score()>0.30 & risk.score()<=0.3615) {"You are in the 2nd lowest quantile out of the four risk groups."}
    else if (risk.score()>0.3615 & risk.score()<=0.4278) {"You are in the 3rd quantile out of the four risk groups."}
    else if (risk.score()>0.4278) {"You are in the highest quantile out of the four risk groups."}

  })
  output$final.risk.score <- renderText({
    paste("Your baseline risk is", risk.score()*100,"%")
  })

  output$baseline.risk_SMSC <- renderText({
    if (risk.score_SMSC()<=0.12022) {"You are in the lowest quantile out of the four risk groups."}
    else if (risk.score_SMSC()>0.12022 & risk.score_SMSC()<=0.17034) {"You are in the 2nd lowest quantile out of the four risk groups."}
    else if (risk.score_SMSC()>0.17034 & risk.score_SMSC()<=0.24834) {"You are in the 3rd quantile out of the four risk groups."}
    else if (risk.score_SMSC()>0.24834) {"You are in the highest quantile out of the four risk groups."}

  })

  output$final.risk.score_SMSC <- renderText({
    paste("Your baseline risk is", risk.score_SMSC()*100,"%")
  })

  output$message <- renderText({"Multiple data sources were used for the presented estimations: aggregate data, individual participant data from randomized clinical trials, as well as individual participant data from an observational study"})
  #output$best.treatment<- renderText({
  #paste("The optimal treatment is", table[order(table[,2]),3])
  #})
}
ui <- navbarPage("Probability of relapsing in patients with relapsing-remitting multiple sclerosis",
                 tabPanel("Randomized clinical trials population",
                          fluidPage(
                            sidebarLayout(

                              # Sidebar with a slider input
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

                              # Show a plot of the generated distribution
                              mainPanel(
                                h5(textOutput("baseline.risk")),
                                plotOutput("plot"),
                                tags$i("Between the two red vertical dashed lines are the baseline risk values observed in the randomized clinical trials with individual participant data. The results in the grey areas are not estimated from the data (extrapolation).",style = "font-size:10px;"),
                                br(),tags$b("Predictions",style = "font-size:20px;"),br(), h5(textOutput("Placebo"),style = "font-size:17px;"),
                                tags$tab(), h5(textOutput("Ranking.Probabilities1"),style = "font-size:17px;"), #, textOutput("best.treatment")
                                h5(textOutput("Ranking.Probabilities2"),style = "font-size:17px;"),
                                h5(textOutput("Ranking.Probabilities3"),style = "font-size:17px;"),
                                tags$hr(),tags$i("Multiple data sources were used for the presented estimations: aggregate data from randimized clinical trials (RCTs) (1),(2), individual participant data from RCTs (3),(4),(5), as well as individual participant data from an observational study (6)",style = "font-size:10px;"),
                                tags$tab(),a("1. Johnson study",
                                             href = "https://pubmed.ncbi.nlm.nih.gov/7617181/",style = "font-size:8px;"),
                                a("2. Bornstein study",
                                  href = "https://pubmed.ncbi.nlm.nih.gov/3302705/",style = "font-size:8px;"),
                                a("3. AFFIRM study",
                                  href = "https://pubmed.ncbi.nlm.nih.gov/16510744/",style = "font-size:8px;"),
                                a("4. CONFIRM study",
                                  href = "https://pubmed.ncbi.nlm.nih.gov/22992072/",style = "font-size:8px;"),
                                a("5. DEFINE study",
                                  href = "https://pubmed.ncbi.nlm.nih.gov/22992073/",style = "font-size:8px;"),
                                a("6. Swiss Multiple Sclerosis Cohort",
                                  href = "https://pubmed.ncbi.nlm.nih.gov/27032105/",style = "font-size:8px;")






                              )
                            )
                          )

                 ),
                 tabPanel("Swiss real-world population",fluidPage(
                   sidebarLayout(

                     # Sidebar with a slider input
                     sidebarPanel(
                       sliderInput(inputId = "Age_SMSC",
                                   label = "Age (years)",
                                   value = 30, min = 17, max = 80),
                       radioButtons(inputId="Gender_SMSC", label="Select Gender:",
                                    choices =list( "Male", "Female")),
                       numericInput(inputId="DiseaseDuration_SMSC", label="Disease Duration (years)",value=1, min=0,max=70, step=0.1),
                       sliderInput(inputId="Edss_SMSC", label="Expanded Disability Status Scale (EDSS)",value=1, min=0,max=7,step=0.5),
                       #checkboxInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions > 0"),
                       numericInput(inputId="GdLesions_SMSC", label="Number of Gadolinium enhanced lesions",value=0, min=0,max=18, step=1),

                       #checkboxInput(inputId="NrRelapses1", label="1 prior relapse"),
                       #checkboxInput(inputId="NrRelapses2", label="2 or more prior relapses"),
                       radioButtons(inputId="NrRelapses_SMSC", label="Number of prior relapses",
                                    choices =list( "One", "More than one")),

                       numericInput(inputId="MonthsSinceLastRelapse_SMSC", label="Months since last relapse",value=1, min=0,max=600, step=0.1),

                       radioButtons(inputId="TrNaive_SMSC", label="I had a MS treatment before",
                                    choices =list( "Yes", "No"))
                       #checkboxInput(inputId="TrNaive", label="Treatment Naive"),
                     ),

                     # Show a plot of the generated distribution
                     mainPanel(h5(textOutput("baseline.risk_SMSC")),plotOutput("plot_SMSC"),
                               tags$i("Between the two red vertical dashed lines are the baseline risk values observed in the observational study. The results in the grey areas are not estimated from the data (extrapolation).",style = "font-size:10px;"),
                               br(),tags$b("Predictions",style = "font-size:20px;"),br(), h5(textOutput("Untreated"),style = "font-size:17px;"),
                               tags$tab(),h5(textOutput("Ranking.Probabilities1_SMSC"),style = "font-size:17px;"), #, textOutput("best.treatment")
                               h5(textOutput("Ranking.Probabilities2_SMSC"),style = "font-size:17px;"),
                               h5(textOutput("Ranking.Probabilities3_SMSC"),style = "font-size:17px;"),
                               tags$hr(),tags$i("Multiple data sources were used for the presented estimations: aggregate data from randimized clinical trials (RCTs) (1),(2), individual participant data from RCTs (3),(4),(5), as well as individual participant data from an observational study (6)",style = "font-size:10px;"),
                               tags$tab(),a("1. Johnson study",
                                            href = "https://pubmed.ncbi.nlm.nih.gov/7617181/",style = "font-size:8px;"),
                               a("2. Bornstein study",
                                 href = "https://pubmed.ncbi.nlm.nih.gov/3302705/",style = "font-size:8px;"),
                               a("3. AFFIRM study",
                                 href = "https://pubmed.ncbi.nlm.nih.gov/16510744/",style = "font-size:8px;"),
                               a("4. CONFIRM study",
                                 href = "https://pubmed.ncbi.nlm.nih.gov/22992072/",style = "font-size:8px;"),
                               a("5. DEFINE study",
                                 href = "https://pubmed.ncbi.nlm.nih.gov/22992073/",style = "font-size:8px;"),
                               a("6. Swiss Multiple Sclerosis Cohort",
                                 href = "https://pubmed.ncbi.nlm.nih.gov/27032105/",style = "font-size:8px;")


                     )
                   )
                 )

                 )
)


shinyApp(ui = ui, server = server)



