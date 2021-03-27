

make_test_select_box <- function(test_id, test_name, choices, selected = "", width = "200px"){
  choices <- c("Select Number of Items" = "", choices)
  selectInput(test_id, test_name, choices, selected = selected, width = width)
}

make_test_select_box_from_params <- function(param_file, test_id, width = "200px"){
  test_params <- param_file %>% filter(name == test_id) 
  choices <- test_params %>%  
    arrange(desc(number_items)) %>% 
    pull(number_items) %>% 
    as.integer()
  header <- sprintf("%s (%s)", test_params$full_name_en[1], test_id)
  make_test_select_box(test_id, header, choices, width = width)    
}

make_all_test_select_boxes <- function(param_file, width = "200px"){
  test_ids <- param_file %>% filter(type == "performance_test") %>% pull(name) %>% unique()
  select_boxes <- purrr::map(test_ids, function(x) make_test_select_box_from_params(param_file, x, width))
  select_boxes 
}

make_questionnaire_checkbox_group <- function(param_file, width = "300px"){
  quests <- param_file %>% filter(type == "questionnaire") 
  choices <- quests %>% pull(name)
  names(choices) <- sprintf("%s (%s)", quests %>% pull(full_name_en), choices)
  checkboxGroupInput("quests", "Self-report Questionnaires", choices = choices, width = width)

}

get_range_string <- function(x){
  sprintf("[%s-%s]", min(x, na.rm = T), max(x, na.rm = T))
}

add_extra_info <- function(config){
  #browser()
  extra_info <- dots_test_params %>% 
    filter(type != "subscale") %>% 
    distinct(name, type, number_items, .keep_all = T) %>% 
    select(name, type, full_name_en, length = number_items, rel_cronbach_alpha:sem)
  config %>% left_join(extra_info, by = c("length", "name", "type"))
}

get_config <- function(input){
  test_ids <- setdiff(names(input), c("language", "quests"))
  tests <- purrr::map_dfr(test_ids, function(ti){
    value <- input[[ti]]
    if(nchar(value) > 0){
      tibble(name  = ti, length = as.integer(value), type = "performance_test")
    }
  })
  ret <- tibble()
  if(is.null(input$quests)){
    ret <- tests
    if(nrow(tests) == 0){
      ret <- tibble()
    }
  }
  else{
    quests <- dots_test_params %>% filter(type != "subscale", 
                                          language == "en", 
                                          name %in% input$quests) %>% 
      select(name, length = number_items, type) 
    if(nrow(tests) == 0){
      ret <- quests
    }
    else{
      ret <- bind_rows(quests, tests)
    }
  }
  if(nrow(ret) > 0){
    ret <- ret %>% add_extra_info()
  }
  ret
}