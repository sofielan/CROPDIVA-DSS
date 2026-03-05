library(shiny)
library(httr)
library(jsonlite)
library(markdown)
library(cld2)
# Load precomputed embeddings (example: load your embeddings file)
load("test2.RData")

time_it <- function(label, expr) {
  start <- Sys.time()
  result <- eval(expr)
  elapsed <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 2)
  cat(sprintf("[TIMER] %-30s : %s sec\n", label, elapsed))
  attr(result, "elapsed") <- elapsed
  result
}

# Define your OpenAI API key
api_key <- Sys.getenv("OPENAI_API_KEY")
# ------------------------------------------------
# Helper: Build system prompt dynamically => zorg dat antwoord op gebruiker is afgestemd
# ------------------------------------------------
build_system_prompt <- function(user_role) {
  paste0(
    "You are a decision support system that provides information on the use of orphan crops ",
    "in farming systems in Europe, based on the CROPDIVA project.\n\n",
    
    "The user is a ", user_role, ".\n\n",
    
    "Tailor your answers to this role by:\n",
    "- always start with naming the user role i.e.",user_role,",\"n",
    "- Adjusting technical depth and vocabulary\n",
    "- Emphasizing aspects most relevant to this role\n",
    "- Providing practical implications where appropriate\n",
    "- start answer with referening to the specific user  \n\n",
    
    "Rules:\n",
    "- Mention CROPDIVA explicitly in every answer\n",
    "- Base answers primarily on the provided CROPDIVA documents\n",
    "- Clearly indicate when information comes from CROPDIVA PDFs\n",
    "- You may complement with external knowledge if needed\n",
    "- end each answer with links to other projects e.g. from the cordis website\n",
    "-End also with integrating external, authoritative agroecology sources, at least as the links to the broader context or to external services \n",
    "- Respond using Markdown formatting"
  )
}

# Function to get embeddings using OpenAI
get_embeddings <- function(text_chunks) {
  embeddings <- list()
  for (chunk in text_chunks) {
    response <- openai::create_embedding(
      model = "text-embedding-ada-002",
      input = chunk,
      openai_api_key =api_key
    )
    embeddings <- c(embeddings, list(response$data$embedding))
  }
  return(embeddings)
}

# Function to search for the best-matching text chunk based on the query
search_query <- function(query, embeddings, chunked_text) {
  query_embedding <- get_embeddings(list(query))[[1]]
  
  similarities <- sapply(embeddings, function(e) {
    sum(unlist(query_embedding) * unlist(e)) / 
      (sqrt(sum(unlist(query_embedding)^2)) * sqrt(sum(unlist(e)^2)))
  })
  
  best_match_idx <- which.max(similarities)
  m=sort(similarities,decreasing=TRUE,index.return=T)
  best=paste(chunked_text[m$ix[1]],chunked_text[m$ix[2]],chunked_text[m$ix[3]],chunked_text[m$ix[4]],chunked_text[m$ix[5]])
  #return(chunked_text[best_match_idx])
  return(best)
}

# Function to chat with GPT-4 using retrieved content
chat_with_pdf <- function(conversation_history) {
  url <- "https://api.openai.com/v1/chat/completions"
  
  headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key)
  )
  data <- list(
    model = "gpt-4-turbo",
    messages = conversation_history,
    temperature = 0.7
  )
  response <- POST(
    url,
    body = toJSON(data, auto_unbox = TRUE),
    add_headers(.headers = headers),
    config = httr::config(http_version = 1.1)
  )
  content <- content(response, as = "text", encoding = "UTF-8")
  result <- fromJSON(content)
  
  if (length(result$choices) > 0) {
    return(result$choices$message$content)
  } else {
    return("Error: No response received from OpenAI API.")
  }
}

#om nieuwe vragen als standalone vraag te herformuleren
reformulate_followup <- function(last_question, previous_questions) {
  # Build a minimal prompt with only previous questions
  system_prompt <- paste0(
    "You are a decision support system for CROPDIVA project.\n",
    "Rephrase the user's follow-up question as a standalone question.\n",
    "Use previous questions for context.\n\n"
  )
  
  # Combine previous questions as context
  context_text <- ""
  context_text <- paste("Previous questions:\n-",
                        paste(previous_questions, collapse = "\n- "), "\n")
  # Final messages for GPT
  messages <- list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste0(context_text, "\nFollow-up question: ", last_question))
  )
  # Call GPT
  reformulated <- chat_with_pdf(messages)
  return(reformulated)
}


# --------- Shiny UI ---------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    .shiny-notification {
                    color: white;
                    position: fixed;
                    top: 20%;
                    left: 50%;
                    background-color: blue;
                    font-size: 50px;
                    font-weight: bold;
                    #top: 5% !important;
                    #left: 50% !important;
                    transform: translateX(-50%) !important;
                    }
                    "))),
  # ),
  
  
  titlePanel("Chat with the CROPDIVA project"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "user_role",
        label = "I am a:",
        choices = c(
          "Farmer",
          "Breeder",
          "Food processor",
          "Scientist",
          "Policy maker",
          "Advisor / Extension service",
          "Other"
        ),
        selected = "Scientist"
      ),
      textInput("query", "What do you want to know from the CROPDIVA project results?", ""),
      actionButton("ask", "Ask!"),
      actionButton("reset", "Reset Conversation")
    ),
    mainPanel(
      #uiOutput("greeting"),
      uiOutput(outputId = "response")
    )
  )
)

# --------- Shiny Server ---------
server <- function(input, output, session) {
  
  # Create a reactive value to track loading state
  #loading <- reactiveVal(FALSE)
  conversation_history <- reactiveVal(list())
  previous_user_questions <- reactiveVal(character(0))
  
  # --- Initialize conversation when role changes ---
  observeEvent(input$user_role, {
    previous_user_questions(character(0))
    updateTextInput(session, "query", value = "")
    output$response <- renderUI({
      HTML("<p>The conversation has been initialized/reset due to change in user role.</p>")
    })
  })
  
  # greeting header niet nodig
  #output$greeting <- renderUI({
  #req(input$user_role)
  #h4(paste0("Dear ", input$user_role, ", here is the answer:"))
  #h4(build_system_prompt(input$user_role))
  #})
  
  # --- Ask button ---
  observeEvent(input$ask, {
    req(input$query)
    output$response <- renderUI({
      div(style = "font-size: 50px; color: red;", "Thinking...")
    })
    
    shiny::withProgress(message = "Thinking...", {
      
      # 1️⃣ Reformulate follow-up question
      if(length(previous_user_questions())>0){
        standalone_question <- time_it(
          "Reformulation (GPT)",
          reformulate_followup(input$query, previous_user_questions())
        )
      }else{standalone_question=input$query}
      
      # 2️⃣ Add to previous questions
      previous_user_questions(c(previous_user_questions(), input$query))
      
      # 3️⃣ Retrieve best-matching CROPDIVA chunks
      best_match <- time_it(
        "Embedding + similarity search",
        search_query(standalone_question, all_embeddings, all_text_chunks)
      )
      if (is.null(best_match) || best_match == "") {
        best_match <- "No relevant CROPDIVA document found."
      }
      
      # 4️⃣ Update conversation with context and user query
      conv <-  list(list(role = "system", content = build_system_prompt(input$user_role)),
                    list(role = "system", content = paste("Context from CROPDIVA documents:", best_match)),
                    list(role = "user", content = standalone_question))
      
      # 5️⃣ Call GPT
      response_text <- time_it(
        "Answer generation (GPT-4)",
        chat_with_pdf(conv)
      )
      
      # 8️⃣ Render HTML
      output$response <- renderUI({
        #HTML(markdownToHTML(text = paste0("<b>Answer:</b> ", conv, "<br><hr>"), fragment.only = TRUE))
        HTML(markdownToHTML(text = paste0("<b>Answer:</b> ",   response_text, "<br><hr>"), fragment.only = TRUE))
        
      })
      
      # 9️⃣ Log
      log_entry <- paste0(
        "Date: ", Sys.Date(), "\n",
        "Time: ", Sys.time(), "\n",
        "User role: ", input$user_role, "\n",
        "Question (original): ", input$query, "\n",
        "Question (standalone): ", standalone_question, "\n",
        "Answer: ", response_text, "\n",
        "-------------------------\n"
      )
      file_name <- paste0("logs/chatlog_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
      writeLines(log_entry, file_name, useBytes = TRUE)
    })
  })
  
  # --- Reset Conversation button ---
  observeEvent(input$reset, {
    previous_user_questions(character(0))
    updateTextInput(session, "query", value = "")
    output$response <- renderUI({
      HTML("<p>The conversation has been reset. You can start a new conversation.</p>")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
