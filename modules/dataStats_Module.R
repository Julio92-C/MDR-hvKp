
# Data Stats Module

# Module UI function
dataStatsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             box(width = 12, 
                 solidHeader = TRUE, 
                 status = "teal",
                 title = "Basic stats results",
                 collapsible=TRUE,
                 gt_output(ns("my_gt_table"))
             )
             
      )
    )
  )
}




# Module Server function
dataStatsServer <- function(id, filter_data){
  stopifnot(is.reactive(filter_data))
  
  moduleServer(id, function(input, output, session){
    observe({
      # Build a gtsummary table
      output$my_gt_table <- render_gt ({
        data_stats = (filter_data() %>% 
          # build a gtsummary table
          tbl_summary(
            by = model,
            include = c("identity", "accuracy", "coverage", "seq_len"),
            statistic = list(
              all_continuous() ~ "{mean} ({sd})",
              all_categorical() ~ "{n} / {N} ({p}%)"
            ),
            digits = all_continuous() ~ 2,
            label = c(identity ~ "Identity", accuracy ~ "Accuracy", coverage ~ "Coverage",
                      seq_len ~ "Sequence Length"),
            sort = list(everything() ~ "frequency"),
            missing_text = "Missing",
            missing = "no"
          )) 
          
          # Conditional for the Level of the variable Model choose
          (
            if (length(levels(as.factor(filter_data()$model))) != 2){
              print("Length of the Level is different to two")
              add_p(data_stats)
              
            } else {
              print("Length of the Level is 2")
              add_difference(data_stats)

            }


          ) %>% add_overall() %>%

          # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
          as_gt() %>%
          tab_header(md("**Table 1. Metagenomics Statistics Results By Model Type**"))
        
        })
      

      })
    
  })
  
}