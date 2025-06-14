library(shiny)
library(shinydashboardPlus)

right_control_bar <- dashboardControlbar(
  skin = "dark",
  collapsed = FALSE,
  overlay = FALSE,
  width = 400,
  
  controlbarMenu(
    id = "menu",
    controlbarItem(
      fluidPage(
        # "Assistant",
        # LLM Models #####
        selectInput(
          "model_gen",
          "AI Model",
          choices = c(
            "Meta-Llama-3.2",
            # "gpt-3.5-turbo",
            "gemini-pro",
            "HuggingFaceTB",
            "Phi-3.5-mini",
            # "claude-2.1",
            # "claude-instant",
            "google-gemma-7b-it",
            "Mixtral-v0.1",
            "Mistral-v0.3",
            "Yi-1.5"
          ),
          selected = "Meta-Llama-3.2"
        ), 
        sliderInput(
          "temperature",
          "Temperature (gemini, llama-3.1 & Phi only)",
          min = 0,
          max = 1,
          value = 0.5, 
          step = 0.1
        ),
        # Chat container #####
        tags$div(
          id = "chat-container",
          tags$div(id = "chat-history", 
                   style = "overflow-y: scroll; height: 280px; display: flex; flex-direction: column; 
                     text-align:left;", 
                   uiOutput("chat_history")),
          
          tags$div(id = "chat-input", tags$form(
            textAreaInput(
              inputId = "prompt",
              label = "",
              placeholder = "Type your message here...",
              width = "100%"
            ),
            # fileInput("file_chat", "Upload (.docx, .pptx)", accept = c(".docx", ".pptx")),
            fluidRow(
              tags$div(
                style = "margin-left: 0em;",
                actionButton(
                  inputId = "chat",
                  label = "Send",
                  icon = icon("paper-plane")
                ),
                actionButton(
                  inputId = "remove_chatThread",
                  label = "Clear History",
                  icon = icon("trash-can")
                ),
                CopyButton(
                  "clipbtn",
                  label = "Copy",
                  icon = icon("clipboard"),
                  text = ""
                )
                
              )
            )
          ))
        )
        # Chat Container end #####
        
      )
    )
  )
)