
# Data Stats Module

# Module UI function
MDRdataStatsUI <- function(id) {
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
MDRdataStatsServer <- function(id, filter_data){
  stopifnot(is.reactive(filter_data))
  
  moduleServer(id, function(input, output, session){
    observe({
      # Build a gtsummary table
      output$my_gt_table <- render_gt ({
        data_stats = (filter_data() %>% 
                        # build a gtsummary table
                        tbl_summary(
                          by = ST,
                          include = c("Genotype", "virulence_score","resistance_score", "Isolation.Source"),
                          statistic = list(
                            all_continuous() ~ "{mean} ({sd})",
                            all_categorical() ~ "{n} / {N} ({p}%)"
                          ),
                          digits = all_continuous() ~ 2,
                          label = c(Isolation.Source ~ "Isolation Source",
                                    virulence_score ~ "Virulence Score",
                                    resistance_score ~ "Resistance score"
                                    ),
                          sort = list(everything() ~ "frequency"),
                          missing_text = "Missing",
                          missing = "no"
                          
                        )) 
        
        add_p(data_stats) %>% 
          add_overall() %>%
          
          # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
          as_gt() %>%
          tab_header(md("**Table 1. Summary of ST groups and specimen sources**"))
        
      })
      
      
    })
    
  })
  
}