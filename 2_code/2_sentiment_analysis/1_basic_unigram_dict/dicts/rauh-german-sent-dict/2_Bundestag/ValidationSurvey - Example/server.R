library(shiny)

# Read the survey questions
Qlist <- read.csv("Qlist1.csv", sep = ";")
# Qlist <- Qlist[1:3,] # Shorter Version for testing purposes

shinyServer(function(input, output) {
  
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(Qlist))
  # Name each element of the vector based on the
  # first column of the Qlist (sentence ID)
  names(results)  <<- Qlist[,1]
  
  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("Hits: ", counter)
    })
  
  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0) 
      return(
        list(
          h4(" "),
          h4(" \nWillkommen!"),
          h5("Im Folgenden werden Dir 250 zufaellig ausgewaehlte Textfragmente gezeigt."),
          h5("Gib jeweils an, ob der Text fuer Dich eine neutrale, positive oder negative Grundstimmung transportiert."),
          h5("Entscheide ganz spontan und ohne ueber die konkreten Textinhalte oder Kontexte nachzudenken. Es gibt kein richtig oder falsch, es geht nur um Deinen allerersten Eindruck."),
          h5("---"),
          h5("Lade bitte die Ergebnisse am Ende dieser Umfrage herunter und schick sie mir per E-Mail (mit Name und Durchgang) zurück."),
          h5("---"),
          h5("\nVielen Dank!\n")
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
      return(
        list(
          h5(paste(input$Click.Counter, "/", nrow(Qlist))), # Progress
          h3(textOutput("question")),
          radioButtons("survey", "Dein Eindruck:", 
                       c(option.list()))
        )
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter>nrow(Qlist))
      return(
        list(
          # h4("View aggregate results"),
          # tableOutput("surveyresults"),
          h4("Vielen Dank fuer die Teilnahme!"),
          downloadButton('downloadData', 'Download der Ergebnisse'),
          br(),
          h6("Dieser 'Weiter' Button funktioniert nicht mehr...")
        )
      )    
  })
  
  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist)))
      try(results[input$Click.Counter] <<- input$survey)
    # try is used because there is a brief moment in which
    # the if condition is true but input$survey = NULL
    
    # If the user has clicked through all of the survey questions
    # then R saves the results to the survey file.
    if (input$Click.Counter==nrow(Qlist)+1) {
      if (file.exists("survey.results.Rdata")) 
        load(file="survey.results.Rdata")
      if (!file.exists("survey.results.Rdata")) 
        presults<-NULL
      presults <- presults <<- rbind(presults, results)
      rownames(presults) <- rownames(presults) <<- 
        paste("User", 1:nrow(presults))
      save(presults, file="survey.results.Rdata")
    }
    # Because there has to be a UI object to call this
    # function I set up render text that distplays the content
    # of this funciton.
    ""
  })
  
  # This function renders the table of results from the
  # survey.
  output$surveyresults <- renderTable({
    t(summary(presults))
  })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      Qlist[input$Click.Counter,2]
    )
  })
  
})

