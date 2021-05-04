
server <- function(input, output, session) {


  log.risk.score=reactive( -1.884607+0.3491485-0.03529109*(input$Age-42.65153)+0.3224459*(log(input$DiseaseDuration+10)-2.9947)+0.1242894*(input$EDSS-2.458)+0.03572392*input$GdLesions-0.07593726*input$NrRelapses1+0.1201103*input$NrRelapses2-0.481536*(log(input$MonthsSinceLastRelapse+10)-3.80)+0.07933781*input$TrNaive+0.2527921*input$Gender)
  risk.score1 = reactive(exp(log.risk.score())/(1+exp(log.risk.score())))
  risk.score2=reactive(round(risk.score1(),2))
  risk.score=reactive(risk.score2()*100)

  data <- reactive({

    x<-19.1
    y<-risk.score()
    z<-(c("1","2"))
    z<-as.factor(z)
    z<-as.data.frame(z)
    w<-as.data.frame(rbind(x,y))
    m<-as.data.frame(cbind(w,z))
    colnames(m)<-c("Predicted probability (%) to relape at 2 years", "Category")
    m<-as.data.frame(m)
  })

  output$final.risk.score <- renderText({
    paste("Your risk to relapse at 2 years is", as.integer(risk.score()), "%")
  })

  output$plot <- renderPlot({
    ggplot(data(), aes(x=Category, y=`Predicted probability (%) to relape at 2 years`)) + ylim(0,50) +
      geom_bar(stat="identity", position=position_dodge(), color=c("lightblue","orange"), fill=c("lightblue","orange"),width=0.1)+  theme(text = element_text(size = 17))+labs(x="") +
      geom_errorbar(aes(ymin=18.5, ymax=19.6), width=.05, size=2,
                    position=position_dodge(.9)) +theme(axis.text.x = element_text(size=20)) + scale_x_discrete(labels=c("Average", paste("Somebody with", "your", "characteristics"))) + ggtitle("Plot of predicted probabilities (%) to relapse at 2 years")


  })

  output$final.text1 <- renderText({
    paste("The average predicted probability (%) to relapse at 2 years is 19.1 with 95% C.I. (18.5, 19.6)")
  })

  output$final.text2 <- renderText({
    paste("Somebody with your characteristics has", risk.score(), "% predicted probability (%) to relapse at 2 years")
  })

  output$final.text3 <- renderText({
   if (risk.score()>=19.2) {
     paste("Your predicted probability (%) to relapse at 2 years is", risk.score()-19.2, "% higher than the average")
   }

      })

  output$final.text4 <- renderText({

    if (risk.score()<19.2) {
      paste("Your predicted probability (%) to relapse at 2 years is", 19.2-risk.score(), "% lower than the average")
    }
  })

  output$final.text <- renderText({


      paste("Numerical Results:")

  })

}

ui <-  fluidPage(theme=shinytheme("readable"),
                 titlePanel(h1("Prevention of relapses in patients with Relapsing-Remitting Multiple Sclerosis")), # using strong as a direct tag
                 #h1("Using textInput and checkboxInput")
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId = "Age",
                                 label = "Age (years)",
                                 value = 1, min = 17, max = 80),
                     numericInput(inputId="DiseaseDuration", label="Disease Duration (years)",value=1, min=0,max=70, step=0.1),
                     sliderInput(inputId="EDSS", label="EDSS",value=1, min=0,max=7,step=0.5),
                     checkboxInput(inputId="GdLesions", label="Number of Gadolinium enhanced lesions > 0"),
                     checkboxInput(inputId="NrRelapses1", label="1 prior relapse"),
                     checkboxInput(inputId="NrRelapses2", label="2 or more prior relapses"),
                     numericInput(inputId="MonthsSinceLastRelapse", label="Months since last relapse",value=1, min=0,max=600, step=0.1),
                     checkboxInput(inputId="TrNaive", label="Treatment Naive"),
                     checkboxInput(inputId="Gender", label="Female")),


                   mainPanel(  span(h3(textOutput("final.risk.score")),style="color:blue"),
                              plotOutput("plot"),
                              span(h3(textOutput("final.text")),style="color:green"),
                              h4(textOutput("final.text1")),
                              h4(textOutput("final.text2")),
                              h4(textOutput("final.text3")),
                              h4(textOutput("final.text4"))
                            )
                 )
)


shinyApp(ui = ui, server = server)





