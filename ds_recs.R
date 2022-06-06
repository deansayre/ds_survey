pacman::p_load(rio, 
               shiny,
               flextable, 
               ggupset,
               cowplot,
               tmap,
               tidyverse)


dictionary <- rio::import("cleaning_rules.xlsx")

Countries <- rio::import( "pmi_country_recs_zl.xlsx") %>% 
  mutate(country_map = column, 
         rec_class = ifelse(!is.na(alt_code), alt_code, rec_class)) %>% 
  select(-alt_code) %>% 
  linelist::clean_data(guess_dates = F, protect = c(T, T, T, T, F)) %>% 
  linelist::clean_variable_spelling(wordlists = dictionary)


data("World")

pmi_map <- World %>% 
  mutate(name = as.character(name), 
         name = case_when(name == "Cote d'Ivoire" ~  "Côte d’lvoire",
                          name == "Dem. Rep. Congo" ~  "D.R. Congo", 
                          name == "Myanmar" ~ "Burma",
                          TRUE ~ name)) %>% 
  select(name) %>% 
  mutate(name1 = name) %>% 
  linelist::clean_data(guess_dates = F, protect = c(T, T, F)) %>% 
  filter(name1 %in% Countries$country_map)

Recommendations <- import("recs.xlsx")

country_join <- import("country_chw.xlsx")
a <- unique(Countries$rec_class) %>% 
  as.data.frame()
rio::export(a, file = "rec_class.xlsx")

rm(ui)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
  radioButtons("dataset", "Mode", c(Countries = "Countries", Recommendations = "Recommendations")),
  conditionalPanel(condition = "input.dataset == 'Countries'",
                   selectInput("elements", "", multiple = T, choices = unique(Countries$column))), 
  conditionalPanel(condition = "input.dataset == 'Recommendations'",
                   radioButtons("union", "Table Control", c("Intersection", "Union"), 
                                selected = "Intersection"),
                   div(selectInput("elements_a", "Recommendation 1", multiple = F, choices = unique(Countries$rec_class)), 
                       style = "background-color: #709DB0; "), 
                   div(selectInput("elements_b", "Recommendation 2", multiple = F, choices = unique(Countries$rec_class)),
                       style = "background-color: #993300; ")), 
  tags$img(src='pmi.png', height = 70, width = 200),
  shiny::img(src="digital_square.png", height = 70, width = 70)
  ),
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Selection", leaflet::leafletOutput("mymap"), 
                                 uiOutput("table")),
                        tabPanel("Context", plotOutput("workers", height=1000)
                                 ), 

                            tabPanel("Country Comparisons", plotOutput("country_comp", height=600))
                          ))
  )
  )


server <- function(input, output, session) {

  
  output$mymap <- leaflet::renderLeaflet({
    if (input$dataset == "Countries") {
      nat_chw <- Countries %>% 
        filter(column %in% input$elements)
      
      cont <- unique(nat_chw$column)
      
      #cont is list of countries with attribute
      dummy_shape <- pmi_map %>% 
        filter(name %in% cont) %>% 
        left_join(country_join, by = c("name" = "country")) %>% 
        sf::st_as_sf()
      
      popup <- paste0("<a href=",dummy_shape$link,">", "Country Profile:  ",dummy_shape$name,"</a>")
      
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas)%>% 
        leaflet::addPolygons(data = dummy_shape, fillColor = "grey50", popup = ~popup)
    }
    else {
      nat_chw_a <- Countries %>% 
        filter(rec_class %in% input$elements_a)
      
      nat_chw_b <- Countries %>% 
        filter(rec_class %in% input$elements_b)
      
      cont_a <- unique(nat_chw_a$column)
      cont_b <- unique(nat_chw_b$column)
      cont_c <- intersect(cont_a, cont_b)
      cont_a1 <- setdiff(cont_a, cont_c)
      cont_b1 <- setdiff(cont_b, cont_c)
      
      dummy_shape_a <- pmi_map %>% 
        filter(name %in% cont_a1) %>% 
        left_join(country_join, by = c("name" = "country")) %>% 
        sf::st_as_sf()
      
      dummy_shape_b <- pmi_map %>% 
        filter(name %in% cont_b1) %>% 
        left_join(country_join, by = c("name" = "country")) %>% 
        sf::st_as_sf()
      
      dummy_shape_c <- pmi_map %>% 
        filter(name %in% cont_c) %>% 
        left_join(country_join, by = c("name" = "country")) %>% 
        sf::st_as_sf()
      
      
      testa <- sf::st_drop_geometry(dummy_shape_a)
      testb <- sf::st_drop_geometry(dummy_shape_b)
      testc <- sf::st_drop_geometry(dummy_shape_c)
      
      
      popup_a <- paste0("<a href=",testa$link,">", "Country Profile:  ",testa$name,"</a>")
      popup_b <- paste0("<a href=",testb$link,">", "Country Profile:  ",testb$name,"</a>")
      popup_c <- paste0("<a href=",testc$link,">", "Country Profile:  ",testc$name,"</a>")
      
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas)%>% 
        leaflet::addPolygons(data = dummy_shape_a, fillColor = "#709DB0", popup = ~popup_a) %>% 
        leaflet::addPolygons(data = dummy_shape_b, fillColor = "#993300", popup = ~popup_b) %>% 
        leaflet::addPolygons(data = dummy_shape_c, fillColor = "white", popup = ~popup_c)
    }
  })

  output$table <- renderUI({
    #req(input$elements)
    if (input$dataset == "Countries") {
      Countries %>% 
        filter(column %in% input$elements) %>% 
        select(-c(col, country_map)) %>% 
        pivot_wider(names_from = rec_class, values_from = verbatim) %>% 
        t() %>% 
        as.data.frame() %>% 
        janitor::row_to_names(1) %>% 
        tibble::rownames_to_column() %>% 
        dplyr::rowwise(rowname) %>% 
        mutate(present = sum(is.na(c_across(where(is.character))))) %>% 
        ungroup() %>% 
        arrange(present) %>% 
        select(-present) %>% 
        rename(" " = 1) %>%
        mutate(across(everything(), ~na_if(.x, "NULL"))) %>% 
        rowwise() %>% 
        mutate(across(everything(), ~paste(.x, collapse = "; "))) %>% 
        ungroup() %>% 
        mutate(across(everything(), ~replace_na(.x, "-"))) %>% 
        flextable() %>% 
        autofit() %>%
        htmltools_value()
 
    }
  else {
    if (input$union == "Intersection"){
      a <- Countries %>% 
        filter(rec_class == input$elements_a)
      b <- Countries %>% 
        filter(rec_class == input$elements_b) 
      
      Countries %>%
        filter(column %in% a$column & column %in% b$column) %>% 
        filter(rec_class == input$elements_a |
                 rec_class == input$elements_b) %>% 
        select(-c(col, country_map)) %>% 
        pivot_wider(names_from = rec_class, values_from = verbatim) %>% 
        t() %>% 
        as.data.frame() %>% 
        janitor::row_to_names(1) %>% 
        tibble::rownames_to_column() %>% 
        rename(" " = 1) %>%
        mutate(across(everything(), ~na_if(.x, "NULL"))) %>% 
        rowwise() %>% 
        mutate(across(everything(), ~paste(.x, collapse = "; "))) %>% 
        ungroup() %>% 
        mutate(across(everything(), ~replace_na(.x, "-"))) %>% 
        flextable() %>%
        autofit() %>%
        htmltools_value()}
      else{
        Countries %>% 
          filter(rec_class ==  input$elements_a |
                 rec_class ==  input$elements_b) %>% 
          select(-c(col, country_map)) %>% 
          pivot_wider(names_from = rec_class, values_from = verbatim) %>% 
          t() %>% 
          as.data.frame() %>% 
          janitor::row_to_names(1) %>% 
          tibble::rownames_to_column() %>% 
          rename(" " = 1) %>%
          mutate(across(everything(), ~na_if(.x, "NULL"))) %>% 
          rowwise() %>% 
          mutate(across(everything(), ~paste(.x, collapse = "; "))) %>% 
          ungroup() %>% 
          mutate(across(everything(), ~replace_na(.x, "-"))) %>% 
          flextable() %>%
          autofit() %>%
          htmltools_value()}
      }
  })
 
  
    output$workers <-renderPlot({
      # check for the input variable
      if (input$dataset == "Countries") {
       nat_chw <- Countries %>% 
         filter(column %in% input$elements)
       
       cont <- unique(nat_chw$column)
      }
      else {
        if (input$union == "Intersection"){
          a <- Countries %>% 
            filter(rec_class == input$elements_a)
          b <- Countries %>% 
            filter(rec_class == input$elements_b) 
          
          nat_chw <- Countries %>% 
            filter(column %in% a$column & column %in% b$column)
          
          cont <- unique(nat_chw$column)
        }
        else{
          a <- Countries %>% 
            filter(rec_class == input$elements_a)
          b <- Countries %>% 
            filter(rec_class == input$elements_b) 
          
          nat_chw <- Countries %>% 
            filter(column %in% a$column | column %in% b$column)
          
          cont <- unique(nat_chw$column)
      }
      }
      cont1 <- country_join %>% 
        filter(country %in% cont)
      
      p <- ggplot(cont1, aes(x = country, y = hcw)) + 
        geom_bar(stat = "identity", fill = "#404756", colour = "black") +
        theme_minimal()+
        labs(x = "", 
                y = "HCW per 10,000 people")
      
      q <- ggplot(cont1, aes(x = country, y = inc)) + 
        geom_bar(stat = "identity",  fill = "#8D6E33", colour = "black") +
        theme_minimal()+
        labs(x = "Country", 
                y = "Annual incidence (per 1000)")
        
      r <- cowplot::plot_grid(p,q, ncol = 1)
      r
    })
    
    output$country_comp <-renderPlot({
      
      if (input$dataset == "Countries" &
          length(input$elements) ==1){
        
        test1 <- Countries %>% 
          select(column, rec_class) %>% 
          filter(column == input$elements) 
        
        vec_comp <- test1$rec_class  
        
        test <- Countries %>% 
          select(column, rec_class) %>% 
          filter(column != input$elements) %>% 
          group_by(column) %>% 
          summarise(all = list(rec_class)) %>% 
          ungroup() %>% 
          rowwise() %>% 
          mutate(n = length(intersect(all, vec_comp))) %>% 
          ungroup() %>% 
          arrange(n) %>% 
          ggplot(aes(x = column, y = n))+
          geom_bar(stat = "identity")+
          theme_minimal()+
          labs(x = "Country", 
               y = "Recs in Common")+
          theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5))
        
        test
      }
      else{
        test2 <- Countries %>% 
          filter(column %in% input$elements, 
                 !is.na(rec_class)) %>% 
          select(-c(verbatim, col, country_map)) %>% 
          group_by(rec_class) %>% 
          summarise(countries = list(column)) %>% 
          ungroup() %>% 
          ggplot(aes(x=countries)) +
          geom_bar() +
          scale_x_upset()+
          theme_minimal()+
          labs(x = "Country Combinations", 
               y = "Recs in Common")
          
        test2
        
      }
    })
}


shinyApp(ui = ui, server = server)



# END
