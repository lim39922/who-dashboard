getPlot <- function(Region,avgBasicWater) {
  
  if (Region =="All")
  {
    qry1 <- data_2007[which(data_2007$clean_water_ratio<=avgBasicWater),]
  }
  else {
    qry1<-data_2007[which(data_2007$Region==Region,data_2007$clean_water_ratio<=avgBasicWater),]
  }
  
  plot_ly(qry1, type='scatter3d',x = ~clean_water_ratio, y = ~MMRatio, z=~Death,  color = ~Region, size = ~size, colors = colors,
          marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(6, 150),
          text = ~paste('Country:', Country, '<br>Maternal Mort. Rt.', MMRatio, '<br>drinking water sv %:', clean_water_ratio,
                        '<br>No.Maternal Death:', Death)) %>%
    layout(title = 'Maternal Mortality v. Basic drinking water service (%)',
           scene2 = list(xaxis = list(title = 'Basic drinking water sv(%)',
                                      size=6 ,
                                      gridcolor = 'rgb(255, 255, 255)',
                                      range = c(0,100),
                                      #type = 'log',
                                      zerolinewidth = 1,
                                      ticklen = 6,
                                      gridwidth = 2),
                         yaxis = list(title = 'Maternal Mortality rt',
                                      size =6,
                                      gridcolor = 'rgb(255, 255, 255)',
                                      range = range(qry1$MMRatio),
                                      zerolinewidth = 1,
                                      ticklen = 6,
                                      gridwith = 2),
                         zaxis = list(title = 'No. Maternal Death',
                                      size=6,
                                      gridcolor = 'rgb(255, 255, 255)',
                                      #type = 'log',
                                      zerolinewidth = 1,
                                      ticklen = 5,
                                      gridwith = 2)),
           paper_bgcolor = 'rgb(243, 243, 243)',
           plot_bgcolor = 'rgb(243, 243, 243)')
  
  
}

getAverage <-function (Region, avgBasicWater,indic){
  
  if (Region =="All")
  {
    
    qry1 <- data_2007[which(data_2007$clean_water_ratio<=avgBasicWater),]
  }
  else {
    qry1<-data_2007[which(data_2007$Region==Region,data_2007$clean_water_ratio<=avgBasicWater),]
  }
  
  if (indic==1) {
    avg1=round(mean(qry1$clean_water_ratio),1)
      if (avg1>= bdrink_avg)
      {avgicon<-'smile'}
      else {avgicon<-'frown'}
    
    return (list(avg1=avg1,avgicon=avgicon))
  }
  else if  (indic==2){
    avg1=round(mean(qry1$BDSR),1)
    if (avg1>= bdrink_avgRur)
    {avgicon<-'smile'}
    else {avgicon<-'frown'}
    
    return (list(avg1=avg1,avgicon=avgicon))
    
  }
  else {
  
    avg1=round(mean(qry1$BDSU),1)
    if (avg1>= bdrink_avgUrb)
      {avgicon<-'smile'}
    else {avgicon<-'frown'}
    
    return (list(avg1=avg1,avgicon=avgicon))
  
    }
}
  
pal <- colorNumeric(
  palette = "#FFA500",
  domain = data_2007$Death)

colpal <- rev(brewer.pal(8,"RdYlGn"))



#server_who<-function(input, output, session) {
function(input, output, session) {
  
  PlotP <- reactive({
     # Refresh if button clicked
    input$Region
    getavgThreshold <- input$rateThreshold
    getRegion <- input$Region
    
    getPlot(getRegion,getavgThreshold)
  })
  
   
  getAvg <- reactive({
    input$rateThreshold
    PlotP() 
    getavgThreshold <- input$rateThreshold
    getRegion <- input$Region
    getAverage(getRegion,getavgThreshold,1)
  })
  
  getAvgRur <- reactive({
    PlotP() 
    getavgThreshold <- input$rateThreshold
    getRegion <- input$Region
    getAverage(getRegion,getavgThreshold,2)
  })
  
  getAvgUrb <- reactive({
    PlotP() 
    # Get interval (minimum 30)
    getavgThreshold <- input$rateThreshold
    getRegion <- input$Region
    getAverage(getRegion,getavgThreshold,3)
  })
  
  
  output$Average <- renderValueBox({
    
    valueBox(
      value = getAvg()$avg1,
      subtitle = "Average basic drinking water(%)",
      icon = icon(getAvg()$avgicon)
      #color = if (100 >= 200) "yellow" else "aqua"
    )
  })
  
  output$Avg_Rural <- renderValueBox({
    valueBox(
      value = getAvgRur()$avg1,
      subtitle = "Average basic drinking water(%)-Rural",
      icon = icon(getAvgRur()$avgicon)
    )
  })
  
  output$Avg_Urban <- renderValueBox({
    valueBox(value =getAvgUrb()$avg1,
      subtitle ="Average basic drinking water(%)-Urban",
      icon = icon(getAvgUrb()$avgicon)
    )
  })

  
  output$packagePlot <- renderPlotly({
   # getPlot("All",100)
    PlotP()
  })
  
  output$selected_var <- renderText({
    if (getAvg()$avgicon =='smile') {text1="is higher"}
    else {text1="is lower"}
    
    
    if (getAvg()$avg1==bdrink_avg)
        {paste(" ")}
    else {
    paste("Avg Basic drinking water sv(%) of selected regions",text1," than overall average (",bdrink_avg,")")
    }
  })
  
  output$selected_var1 <- renderText({
    
    if (getAvgUrb()$avgicon =='smile') {text2="is higher"}
    else {text2="is lower"}

    if (getAvg()$avg1==bdrink_avg)
    {paste(" ")}
    else {
      paste("Avg Basic drinking water sv(%) of selected regions in Urban",text2," than overall average (",bdrink_avgUrb,")")
    }
  })
  
  
  output$selected_var2 <- renderText({
    
    
    if (getAvgRur()$avgicon =='smile') {text3="is higher"}
    else {text3="is lower"}
    
    
    if (getAvg()$avg1==bdrink_avg)
    {paste(" ")}
    else {
      paste("Avg Basic drinking water sv(%) of selected regions in Rural",text3," than overall average (",bdrink_avgRur,")")
    }
  })
  
  ##========== osming
  
  dataset <- reactive({
    subset(data_2007, Region == input$region_input 
           & SKP >= input$shp_input[1]
           & SKP <= input$shp_input[2])
  })
  #output$corrplot <- renderPlotly({
  #  corrplot <- ggplot(bcl, aes(x=Maternal_Mortality_Ratio, 
  #                              y=skilled_health_personnel_percent,
  #                              na.rm=TRUE,color=Region)) +
  #    geom_point() + 
  #    geom_smooth(method=lm, aes(fill=Region), se=FALSE) +
  #    labs(x="Maternal Mortality Ratio", y = "Skilled Health Personnel Percentage") +
  #    theme(legend.position = 'none')
  #  ggplotly(corrplot)
  #})
  output$corrplot <- renderPlotly({
    corrplot <- ggplot(data_2007, aes(x=MMRatio, 
                                y=SKP,
                                na.rm=TRUE,color=Region)) +
      geom_point() + 
      geom_smooth(method=lm, aes(fill=Region), se=FALSE) +
      labs(x="Maternal Mortality Ratio", y = "Skilled Health Personnel Percentage") +
      theme(legend.position = 'none')
    ggplotly(corrplot)
  })
  #output$countryplot <- renderPlotly({
  #  countryplot <- ggplot(dataset(), aes(x=Maternal_Mortality_Ratio, 
  #                                       y=skilled_health_personnel_percent,
  #                                       na.rm=TRUE,color=Country)) +
  #    geom_point() + 
  #    labs(x="Maternal Mortality Ratio", y = "Skilled Health Personnel Percentage") +
  #    theme(legend.position = 'none')
  #  ggplotly(countryplot)
  #})
  output$countryplot <- renderPlotly({
    countryplot <- ggplot(dataset(), aes(x=MMRatio, 
                                         y=SKP,
                                         na.rm=TRUE,color=Country)) +
      geom_point() + 
      labs(x="Maternal Mortality Ratio", y = "Skilled Health Personnel Percentage") +
      theme(legend.position = 'none')
    ggplotly(countryplot)
  })
  output$value1 <- renderValueBox({
    valueBox(
      formatC(mean(dataset()$SKP), format="d", big.mark=',')
      ,paste('Skilled Health Personnel (%):',input$region_input)
      ,icon = icon("heart-empty",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(mean(dataset()$MMRatio), format="d", big.mark=',')
      ,'Maternal Mortality Ratio'
      ,icon = icon("user",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(nrow(dataset()), format="d", big.mark=',')
      ,'Number of countries in the region'
      ,icon = icon("home",lib='glyphicon')
      ,color = "yellow")   
  })
  
  
  ##========= yan
  output$CountryMap <- renderLeaflet({
    leaflet(data_2007) %>% addTiles() %>% addProviderTiles("Esri.WorldStreetMap") %>%
      #setView(lng = 31.165580, lat = 48.379433, zoom = 2) %>%
      setView(lng = 70, lat = 30, zoom = 2) %>%
      addCircles(lng = as.numeric(data_2007$lon), 
                 lat = as.numeric(data_2007$lat), 
                 weight = 1, 
                 radius = sqrt(data_2007$Death)*1500, 
                 #radius = (mydata_df$MMRatio)*300,
                 popup = paste(data_2007$Country, ": ", data_2007$Death), 
                 color = "#FFA500", 
                 fillColor = ~colorBin(colpal, bins = 8, Death, na.color = "#8b0000", alpha = F)(Death),
                 fillOpacity = 0.7) %>%
      #addLegend("bottomleft", pal = pal, values = mydata_df$Death, title = "Population in Regions") %>%
      addLegend("bottomleft", 
                pal = colorBin(colpal, bins = 8, na.color = "#8b0000", domain = data_2007$Death), 
                values = data_2007$Death, title="Number of maternal death in Country") %>%
      
      #Easy buttons code
      
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  ##================ wong
  output$world_mortality_scatter_graph = renderPlotly({
    selected_area = input$world_area_select
    start_and_end_index_of_areas = get_start_and_end_index_of_area(selected_area)
    
    # start to draw the curves
    start_index = start_and_end_index_of_areas[1]
    end_index = start_and_end_index_of_areas[2]
    draw_plotly_mortality_by_continent(start_index, end_index)
  })
  
  output$mortality_of_one_country_data_table = DT::renderDataTable({
    year = input$year_slider
    p = mortality_of_countries_df %>% select(Country, ends_with(as.character(year)))
    
    p$ratio = p[, 2] / sum(p[, 2])
    p$ratio = specify_decimal(p$ratio, 4)
    
    mortality_of_countries_in_one_year = p
    datatable(
      mortality_of_countries_in_one_year,
      selection = 'single',
      options = list(pageLength = 10, searchHighlight = TRUE)
    ) %>%
      formatStyle('Country',  color = 'red')
  })
  
  # generate and show the country name cloud image
  output$mortality_country_cloud = renderPlot({
    year = input$year_slider
    mortality_of_countries_in_one_year = mortality_of_countries_df %>%
      select(Country, ends_with(as.character(year)))
    
    #set.seed(4363)
    wordcloud(
      mortality_of_countries_in_one_year$Country,
      mortality_of_countries_in_one_year[, 2],
      scale = c(6, 1),
      max.words=100,
      rot.per = 0.2,
      random.order=FALSE,
      colors = brewer.pal(8, "Dark2"),
      random.color = TRUE
    )
  })
  
  
  # render the global sphere
  output$global_sphere = renderGlobe({
    area = input$world_area_select_for_global_sphere
    names = get_country_names_by_area(area)
    countries_to_plot = country_locations %>% filter(country_names %in% names)
    
    # get the mortality of that year for the specified countries
    year = input$year_slider
    p = mortality_of_countries_df %>%
      select(Country, ends_with(as.character(year))) %>%
      filter(Country %in% names)
    
    # adjust bar height based on mortality
    bar_height = p[, 2] / max(p[, 2]) * 500
    
    background_color = ifelse(input$sphere_view_background, "black", "white")
    
    earth_surface_img = "data/img/world.topo.bathy.200412.3x5400x2700.jpg"
    #globejs(
    #  img = earth_surface_img,
    #  emmisive = "#000000",
    #  bodycolor = "#000000",
    #  lightcolor = "#aaaa44",
      
    #  lat = countries_to_plot$lat,
    #  long = countries_to_plot$lon,
    #  value = bar_height,
      
    #  fov = 45,
    #  color = "red", 
    #  bg = background_color,
    #  pointsize = 5
    #)
    
    globejs(img=earth_surface_img,
            bg=background_color, 
            lat=countries_to_plot$lat,     
            long=countries_to_plot$lon, 
            value=bar_height, 
            color="red",
            rotationlat=-0.34,     
            rotationlong=-0.38, 
            fov=45)
  })
  
  
  # render the leaflet map with polygons
  output$leaf_map = renderLeaflet({
    area = input$world_area_select_for_global_sphere
    names = get_country_names_by_area(area)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = subset(world_ogr, name %in% names),
        weight = 2,
        color = "red",
        fillColor = "green",
        fillOpacity = 0.3
      )
  })
  
  # render world map
  output$world_map_plot <- renderGvis({
    selected_year = input$year_slider
    mortality_of_countries_in_one_year = mortality_of_countries_df %>%
      select(Country, ends_with(as.character(selected_year)))
    
    gvisGeoChart(
      mortality_of_countries_in_one_year,
      locationvar = 'Country',
      colorvar = paste0("X", selected_year),
      options = list(
        width = 1000,
        height = 600,
        #backgroundColor = "black",
        colors = "['blue', 'green', 'cyan', 'yellow', 'magenta', 'red']"
      )
    )
  })
  
  output$mytable = DT::renderDataTable({
    data_2007
  })
  
}

#shinyApp(ui = ui_who, server = server_who)