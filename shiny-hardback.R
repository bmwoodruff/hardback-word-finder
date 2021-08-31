library(shiny)
library(stringr)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Hardback Word Finder"),
  HTML('Place the letters that you must and/or desire to be required as part of your word (so inked, special, timeless classics, etc.) in the "Required Letters:" section. Place all other letters in the "Optional Letters" section. These letters will be considered as wilds and may or not be included in the final word. Then click "Find Words" to obtain all possible words from 12dicts 2of12inf.txt dictionay (valid for playing in Harback at boardgamearena.com with the default dictionary). The list is sorted by number of matched letters (so fewest wilds). Based on possible combos, the top words may or may not be the best.</p>'),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "letters_req",
                label = "Required letters:", 
                value = ""),
      textInput(inputId = "letters_opt",
                label = "Optional Letters:", 
                value = ""),
      actionButton(inputId = "find_words", 
                   label = "Find Words"),
    ),
    
    mainPanel(
      tableOutput(outputId = "myTable")
    )
  )
)

server <- function(input, output) {
  words <- read_lines("2of12inf.txt") %>% 
    str_remove_all("!") %>% 
    str_remove_all("%") %>% 
    str_to_lower()
  my_dat <- tibble(word = words) %>% 
    mutate(matches = c(0)) %>% 
    mutate(length = str_length(word))
  
  my_table <- eventReactive(input$find_words, {
    letters_required <- input$letters_req %>% str_to_lower() %>% str_remove_all("[^[:alpha:]]")
    letters_optional <- input$letters_opt %>% str_to_lower() %>% str_remove_all("[^[:alpha:]]")
    
    wanted_letters_min <- str_length(letters_required)
    wanted_letters_max <- str_length(letters_required)+str_length(letters_optional)
    
    my_let_table <- 
      tibble(letter = letters_required %>% str_split("") %>% unlist()) %>% 
      distinct()%>%
      mutate(freq = str_count(letters_required,letter))
    unique_letters <- my_let_table %>% pull(letter)
    letters_freq <- my_let_table %>% pull(freq)
    
    req_dict <- my_dat
    
    i <- 0
    while(i < length(unique_letters)){
      i <- i+1
      req_dict <- req_dict %>% 
        mutate(matches = matches + pmin(str_count(word,unique_letters[i]),letters_freq[i]))
    }
    
    req_dict <- req_dict %>% 
      filter(matches >= wanted_letters_min) %>%  
      filter(length>= wanted_letters_min) %>% 
      filter(length<= wanted_letters_max) %>% 
      arrange(-matches) 
    
    req_dict_updated <- req_dict
    
    my_let_table_opt <- 
      tibble(letter = letters_optional %>% str_split("") %>% unlist()) %>% 
      distinct()%>%
      mutate(freq_opt = str_count(letters_optional,letter)) %>% 
      left_join(my_let_table, by = "letter") %>% 
      mutate(freq=replace_na(freq,0))
    unique_letters_opt <- my_let_table_opt %>% pull(letter)
    letters_freq_opt <- my_let_table_opt %>% pull(freq_opt)
    letters_freq_used <- my_let_table_opt %>% pull(freq)
    
    i <- 0
    while(i < length(unique_letters_opt)){
      i <- i+1
      req_dict_updated <- req_dict_updated %>% 
        mutate(matches = matches + pmin(str_count(word,unique_letters_opt[i])-letters_freq_used[i],letters_freq_opt[i]))
    }
    
    req_dict_updated %>% 
      arrange(-matches) %>% 
      mutate(matches = as.integer(matches))
  } )
  
  output$myTable <- renderTable(my_table())
}

shinyApp(ui = ui, server = server)
