library(shiny)
library(leaflet)
library(sf)



server <- function(input, output, session) {
    
    #Update county dropdown choices based on selected state
      observeEvent(input$State, {
        county_choices <- filter(joineddf3_sf, State == input$State) %>%
          select(County) %>%
          unique()
        updateSelectInput(session, "County", choices = county_choices)
      })


  # Generate leaflet map
  output$map <- renderLeaflet({
    
    # Initialize leaflet map
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT") %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)
    
    
    # Add Circles of each ethnicity indicator
    if ("Asian" %in% input$ethnicity) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Asian*1000,
          label = ~paste(Asian, "% Asian", sep = " "),
          popup = ~County,
          group = "Asian",
          color = "coral",
          stroke = FALSE,
          fillOpacity = 0.8
        )
    }
    
    if ("Hispanic" %in% input$ethnicity) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Hispanic*1000,
          label = ~paste(Hispanic, "% Hispanic", sep = " "),
          popup = ~County,
          group = "Hispanic",
          color = "cyan",
          stroke = FALSE,
          fillOpacity = 0.4
        )
    }
    
    if ("Black" %in% input$ethnicity) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Black*1000,
          label = ~paste(Black, "% Black", sep = " "),
          popup = ~County,
          group = "Black",
          color = "deepskyblue",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    
    if ("Native_American" %in% input$ethnicity) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Native_American*1000,
          label = ~paste(Native_American, "% Native American", sep = " "),
          popup = ~County,
          group = "Native_American",
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    
    if ("White" %in% input$ethnicity) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~White*1000,
          label = ~paste(White, "% White", sep = " "),
          popup = ~County,
          group = "White",
          color = "lemonchiffon",
          stroke = FALSE,
          fillOpacity = 0.1
        )
    }
    
    #Add circles for each other indicator
    
    if ("Uninsured" %in% input$other) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf %>% filter(fips != 15005 & fips != 08014), # Filter out the row with FIPS 15005
          radius = ~Uninsured*1000,
          label = ~paste(Uninsured, "% Uninsured", sep = " "),
          popup = ~County,
          group = "Uninsured",
          color = "lightskyblue4",
          stroke = FALSE,
          fillOpacity = 0.1
        )
    }
    
    
    if ("Poverty" %in% input$other) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Poverty*1000,
          label = ~paste(Poverty, "% Poverty", sep = " "),
          popup = ~County,
          group = "Poverty",
          color = "black",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    
    if ("Uneducated" %in% input$other) {
      map <- map %>%
        addCircles(
          lng = ~Longitude,
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Uneducated*1000,
          label = ~paste(Uneducated, "% Uneducated", sep = " "),
          popup = ~County,
          group = "Uneducated",
          color = "navy",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    
    # Add circles for each health indicator
    if ("Diabetes" %in% input$health) {
      map <- map %>%
        addCircles(
          lng = ~Longitude, 
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Diabetes*1000,
          label = ~paste(Diabetes, "% diabetes", sep = " "),
          popup = ~County,
          group = "Diabetes",
          color = "blue",
          stroke = FALSE,
          fillOpacity = 0.4
        )
    }
    if ("Hypertension" %in% input$health) {
      map <- map %>%
        addCircles(
          lng = ~Longitude, 
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Hypertension*1000,
          label = ~paste(Hypertension, "% hypertension", sep = " "),
          popup = ~County,
          group = "Hypertension",
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    if ("Obesity" %in% input$health) {
      map <- map %>%
        addCircles(
          lng = ~Longitude, 
          lat = ~Latitude,
          data = joineddf3_sf,
          radius = ~Obesity*1000,
          label = ~paste(Obesity, "% obesity", sep = " "),
          popup = ~County,
          group = "Obesity",
          color = "green",
          stroke = FALSE,
          fillOpacity = 0.2
        )
    }
    
    selected_county <- joineddf3_sf %>%
      filter(State == input$State, County == input$County)
    
    map <- map %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        data = selected_county,
        popup = ~County,
        label = ~HTML(paste(County,", ", State, "<br>Population: ", Population)),
        group = "Selected County"
      )
    
    map
  })
  
  output$data <- renderDataTable({
    
    # Filter data based on user input
    filtered_data <- joineddf3_sf %>%
      filter(State == input$State) %>%
      select(State, County, Longitude, Latitude, Black, White, Asian, Native_American, Hispanic, Uninsured, Unemployed, Poverty, Uneducated, Diabetes, Hypertension, Obesity)
    
    # Return datatable
    datatable(filtered_data)
  
  })
  
  
  output$diabetes_boxplot <- renderPlot({
    joineddf2 %>%
      filter(!is.na(State)) %>%
      group_by(State, Diabetes) %>%
      summarise(total_diabetes = sum(Diabetes)) %>%
      ggplot(aes(x = State, y = Diabetes)) +
      geom_boxplot() +
      facet_wrap(~ State, nrow = 2, scale = "free_x") +
      ggtitle("Diabetes by State") +
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
                   geom = "crossbar", width = 0.5, col = "red", alpha = 0.5, 
                   aes(x = State, y = Diabetes)) +
      geom_text(aes(x = State, y = max(Diabetes), 
                    label = paste("Total Diabetes:\n", 
                                  round(total_diabetes))), 
                vjust = -0.5, size = 3, col = "blue") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.text = element_blank())
  })
  
  output$obesity_boxplot <- renderPlot({
    joineddf2 %>%
      filter(!is.na(State)) %>%
      group_by(State, Obesity) %>%
      summarise(total_obesity = sum(Obesity)) %>%
      ggplot(aes(x = State, y = Obesity)) +
      geom_boxplot() +
      facet_wrap(~ State, nrow = 2, scale = "free_x") +
      ggtitle("Obesity by State") +
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
                   geom = "crossbar", width = 0.5, col = "red", alpha = 0.5, 
                   aes(x = State, y = Obesity)) +
      geom_text(aes(x = State, y = max(Obesity), 
                    label = paste("Total Obesity:\n", 
                                  round(total_obesity))), 
                vjust = -0.5, size = 3, col = "blue") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.text = element_blank())
  })
  
  output$hypertension_boxplot <- renderPlot({
    joineddf2 %>%
      filter(!is.na(State)) %>%
      group_by(State, Hypertension) %>%
      summarise(total_hypertension = sum(Hypertension)) %>%
      ggplot(aes(x = State, y = Hypertension)) +
      geom_boxplot() +
      facet_wrap(~ State, nrow = 2, scale = "free_x") +
      ggtitle("Hypertension by State") +
      stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
                   geom = "crossbar", width = 0.5, col = "red", alpha = 0.5, 
                   aes(x = State, y = Hypertension)) +
      geom_text(aes(x = State, y = max(Hypertension), 
                    label = paste("Total Hypertension:\n", 
                                  round(total_hypertension))), 
                vjust = -0.5, size = 3, col = "blue") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.text = element_blank())
  })
  
  output$state_map <- renderPlot({
    
    indicator <- ifelse(length(input$health) > 0, input$health[1], "Diabetes")
    joineddf2 %>%
      filter(State == input$State) %>%
      mutate(group = group_indices(., County)) %>%
      ggplot(aes(x = long, y = lat, group = group, fill = get(indicator))) +
      geom_polygon() +
      scale_fill_gradient(low = "grey", high = "blue", name = paste(input$health, "Log"),
                          trans="log") +
      ggtitle(paste(input$health, "Prevalence by County")) +
      theme_void() +
      coord_equal()
  })
  
  output$diabetes_correlation <- renderPlot({
    
    ggplot(joineddf2, aes(x = Obesity, y = Diabetes)) +
      geom_point() +
      labs(title = "Correlation Between Obesity and Diabetes",
           x = "Obesity prevalence",
           y = "Diabetes prevalence") +
      theme_minimal() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_text(x = max(joineddf2$Obesity), y = min(joineddf2$Diabetes),
                label = paste0("p: ", round(cor(joineddf2$Obesity, joineddf2$Diabetes), 2))) +
      scale_x_log10() +
      scale_y_log10()
  })
  
  output$hypertension_correlation <- renderPlot({
    
    ggplot(joineddf2, aes(x = Obesity, y = Hypertension)) +
      geom_point() +
      labs(title = "Correlation Between Obesity and Hypertension",
           x = "Obesity prevalence",
           y = "Hypertension prevalence") +
      theme_minimal() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_text(x = max(joineddf2$Obesity), y = min(joineddf2$Hypertension),
                label = paste0("p: ", round(cor(joineddf2$Obesity, joineddf2$Hypertension), 2))) +
      scale_x_log10() +
      scale_y_log10()
  })
  
  output$density <- renderPlot({
    
    joineddf2 %>%
      select(State, County, Obesity, Diabetes, Hypertension) %>%
      filter(!is.na(State), !is.na(County), !is.na(Obesity),
             !is.na(Diabetes), !is.na(Hypertension)) %>%
      pivot_longer(cols = c(Obesity, Diabetes, Hypertension),
                   names_to = "variable",
                   values_to = "value") %>%
      group_by(State, County, value, variable) %>%
      summarise(diabetes_total = sum(value[variable == "Diabetes"]),
                obesity_total = sum(value[variable == "Obesity"]),
                hypertension_total = sum(value[variable == "Hypertension"])) %>%
      ggplot(aes(x = value, fill = variable)) +
      geom_density(alpha = 0.5) +
      labs(x = "Prevalence", y = "Density", fill = "") +
      ggtitle("Distribution of Obesity, Diabetes and High Blood Pressure in the USA")
    
  })
  
  
  
}
