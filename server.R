library(shiny)
library(tidyverse)
library(yaml)

source("common.R")

# NEW Server
server <- function(input, output, session) {
  output$study_format <- renderUI({
    selectInput("format", label = h4("Study Format"),
                choices = filter(studies, study == input$study)$format)
  })
  
  output$specification <- renderPrint({
    req(input$study, input$format)
    yaml_file_path <- paste0("data_specifications/", input$study, "_", input$format, ".yaml")
    yaml::yaml.load_file(yaml_file_path)
  })
  
  output$validator_output <- renderPrint({
    req(input$file)
    
    yaml_file_path <- paste0("data_specifications/", input$study, "_", input$format, ".yaml")
    fields <- yaml::yaml.load_file(yaml_file_path)
    
    tryCatch({
      df <- read_csv(input$file$datapath)
      cat("Upload successful!\n\n")
    }, error = function(e) {
      stop(safeError(e))
    })
    
    valid <- validate_dataset(fields, df)
    
    if (valid) {
      cat("\nDataset is valid!")
    } else {
      cat("\nDataset is NOT valid, please correct and try again!")
    }
  })
  
  userData <- reactive({
    nVars <- input$numVars
    data_list <- list()
    
    if (nVars > 0) {
      for (i in 1:nVars) {
        field_type <- input[[paste0("field_type_", i)]]
        
        if (field_type == "Options") {
          strs <- strsplit(input[[paste0("option_input_", i)]], ",")
          str_new <- paste(unlist(strs), collapse = ", ")
        }
        
        
        options <- ifelse(field_type == "Options", str_new, NA)
        lowerlimit <- ifelse(field_type == "Numeric" && input[[paste0("range_req_", i)]] == "Yes", input[[paste0("min_value_", i)]], NA)
        upperlimit <- ifelse(field_type == "Numeric" && input[[paste0("rage_req_", i)]] == "Yes", input[[paste0("max_value_", i)]], NA)
        
        format <- if (field_type == "Numeric" && input[[paste0("range_req_", i)]] == "Yes") {
          "ranged"
        } else if (field_type == "String" && input[[paste0("caped_", i)]] == "Yes") {
          "capitalized"
        } else {
          "open"
        }
        
        data_list[[i]] <- list(
          field = input[[paste0("field_name_", i)]],
          description = input[[paste0("field_description_", i)]],
          type = field_type,
          options = options,
          format = format,
          lowerlimit = lowerlimit,
          upperlimit = upperlimit,
          required = input[[paste0("is_required_", i)]],
          NA_allowed = input[[paste0("allow_na_", i)]],
          error_message = input[[paste0("error_message_", i)]]
        )
      }
    }
    
    return(data_list)
  })
  
  
  output$downloadSetup <- downloadHandler(
    filename = function() {
      paste("data_settings_", Sys.Date(), ".yaml", sep = "")
    },
    content = function(file) {
      data <- userData()
      write_yaml(data, file)
    }
  )

  output$variableInputs <- renderUI({
    numVars <- input$numVars
    if (numVars > 0) {
      field_list <- lapply(1:numVars, function(i) {
        fluidRow(
          # Field Humanize Variables
          textInput(paste0("field_name_", i), paste("Enter the name of your data (Field ", i, "):")),
          textInput(paste0("field_description_", i), paste("Enter a description of your data (Field ", i, "):")),
          
          # Field Type and global variables
          selectInput(paste0("field_type_", i), "Choose your data type:", 
                      choices = c("Options", "Numeric", "String")),
          selectInput(paste0("is_required_", i), "Is the data type required:", 
                      choices = c("Yes", "No")),
          selectInput(paste0("allow_na_", i), "Are NA Values Allowed:", 
                      choices = c("Yes", "No")),
          
          conditionalPanel(
            condition = paste0("input.field_type_", i, " == 'Numeric'"),
            selectInput(paste0("range_req_", i), "Are there range restrictions on the input:", 
                        choices = c("No", "Yes")),
            
            numericInput(paste0("min_value_", i), "Minimum Value:", value = 0),
            numericInput(paste0("max_value_", i), "Maximum Value:", value = 100)
          ),
          
          conditionalPanel(
            condition = paste0("input.field_type_", i, " == 'String'"),
            selectInput(paste0("caped_", i), "Should the input have capitalizations:", 
                        choices = c("No", "Yes"))
          ),
          
          conditionalPanel(
            condition = paste0("input.field_type_", i, " == 'Options'"),
            textInput(paste0("option_input_", i), "Enter the name of the options separated by a comma and no space:")
          ),
          
          textInput(paste0("error_message_", i), "Enter an error message for your data:")
        )
      })
      
      do.call(tagList, field_list)
    }
  })
}

  


  