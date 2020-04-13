function(session, input, output) {
  
  # subselect the country
  confirmed_case <- reactive({
    validate(need(input$which_country, 'select a country...'))
    confirmed_long_jhu %>%
      filter(country_region == input$which_country) 
  })
  
  observeEvent(input$which_country, {
    min_date <- confirmed_case()$Date %>% min()
    max_date <- confirmed_case()$Date %>% max()
    
    best_date <- confirmed_case() %>%
      group_by(Date) %>%
      summarise(cumulative_cases = sum(cumulative_cases)) %>%
      filter(cumulative_cases >= 50) %>%
      pull(Date) %>%
      min()
    
    updateAirDateInput(session, 'start_date', value = best_date)

  })
  
  # valuebox
  output$vb_total_cases <- renderValueBox({
    value <- confirmed_case()$incident_cases %>% sum()
    last_value <- confirmed_case() %>%
      group_by(Date) %>%
      summarise(cumulative_cases = sum(cumulative_cases)) %>%
      arrange(Date) %>%
      slice(n()-1) %>%
      pull(cumulative_cases)
    
    valueBox(
      formatC(value, format="f", big.mark=",", digits=0),
      HTML("# Cases: ", '\U25B2', value - last_value),
      icon = icon(""), color = 'red'
    )
  })
  
  output$vb_cases_pop <- renderValueBox({
    validate(need(input$which_country, 'select a country...'))
    
    value <- country_jhu %>%
      filter(country_region == input$which_country) %>%
      slice(n()) %>%
      pull(infection_pct)
    last_value <- country_jhu %>%
      filter(country_region == input$which_country) %>%
      slice(n()-1) %>%
      pull(infection_pct)
    
    valueBox(
      paste0(round(value * 1000, 2), '‰'),
      HTML("‰ infected of total population", '\U25B2', round((value - last_value)*1000, 2), '‰'),
      icon = icon(""), color = 'red'
    )
  })
  
  output$vb_double_in_xday <- renderValueBox({
    value <- incidence_rec()$incidence_fit$info$doubling
    valueBox(
      formatC(value, format="f", big.mark=",", digits=2),
      "Doubling time in days",
      icon = icon(""), color = 'red'
    )
  })
  
  output$vb_est_r0 <- renderValueBox({
    growth_R0 <- r2R0_rec()
    mu <- mean(growth_R0)
    value <- mu
    valueBox(
      formatC(value, format="f", big.mark=",", digits=2),
      "Estimated R0 since outbreak",
      icon = icon(""), color = 'red'
    )
  })
  
  output$vb_r0_7d <- renderValueBox({
    value <- r0_epiestm_rec()$R %>%
      tail(1) %>%
      pull(`Mean(R)`)
    
    last_value <- r0_epiestm_rec()$R %>%
      slice(n()-1) %>%
      pull(`Mean(R)`)
    
    diff <- (value - last_value) %>% round(2)
    
    which_triangle <- if_else(diff >=0, '\U25B2', '\U25BC')
    
    valueBox(
      formatC(value, format="f", big.mark=",", digits=2),
      HTML(paste0("R0 of last 7d - ", which_triangle, " ", diff)),
      icon = icon(""), color = 'red'
    )
  })
  
  # plot cumulative case
  output$plot_cum_case <- renderEcharts4r({
    if (input$cum_case_by_province) {
      plot_data <- confirmed_case() %>%
        mutate(cumulative_cases = if_else(cumulative_cases == 0, NA_real_, cumulative_cases)) %>%
        group_by(province)
    } else {
      plot_data <- confirmed_case() %>%
        group_by(Date) %>%
        summarise(cumulative_cases = sum(cumulative_cases)) 
    }
    
    plot_data %>%
      e_charts(x = Date) %>% # initialise and set x
      e_line(serie = cumulative_cases)%>% 
      e_tooltip(trigger = 'axis') %>%
      e_datazoom(x_index = 0, type = "slider") 
      
  })
  
  incidence_rec <- reactive({
    validate(need(input$start_date, 'please provide start date...'))

    incidence <- confirmed_case() %>%
      filter(Date >= as_date(input$start_date)) %>%
      select(Date, incident_cases, province) %>%
      filter(incident_cases > 0) %>%
      uncount(incident_cases) %>%
      arrange(Date)
    
    incidence_object <- incidence(incidence$Date, last_date = max(incidence$Date))
    incidence_fit <- incidence::fit(incidence_object, quiet = FALSE)
    
    return(list(incidence_object = incidence_object, incidence_fit = incidence_fit))
  })
  
  r2R0_rec <- reactive({
    mu <- input$si_mu
    sigma <- input$si_sd
    incidence_fit <- incidence_rec()$incidence_fit
    
    # si distribution
    param <- epitrix::gamma_mucv2shapescale(mu, sigma/mu)
    w <- distcrete::distcrete("gamma", interval = 1, shape = param$shape, scale = param$scale, w = 0)
    
    growth_R0 <- lm2R0_sample(incidence_fit$model, w, n = 1000) 
    
    return(growth_R0)
  })
  
  output$plot_r2R0 <- renderEcharts4r({
    growth_R0 <- r2R0_rec()
    mu <- mean(growth_R0)
    
    tibble(y=growth_R0) %>% 
      e_charts() %>% 
      e_histogram(y, breaks=50) %>% 
      e_tooltip() %>%
      e_mark_line(data = list(xAxis = mu, type='average', label=list(formatter='Avg: {c}')))

  })
  
  # projection with no social distancing
  projection_rec <- reactive({
    pred_fwd_days <- 7
    incidence_object <- incidence_rec()$incidence_object
    growth_R0 <- r2R0_rec()
    mu <- input$si_mu
    sigma <- input$si_sd
    
    # si distribution
    param <- epitrix::gamma_mucv2shapescale(mu, sigma/mu)
    w <- distcrete::distcrete("gamma", interval = 1, shape = param$shape, scale = param$scale, w = 0)
    
    pred_growth <- project(incidence_object, R = median(growth_R0), si=w, n_sim=10000, n_days = pred_fwd_days)
    pred_cum <- cumulate(pred_growth)
    
    return(list(pred_growth=pred_growth, pred_cum=pred_cum))
  })
  
  output$plot_projection <- renderPlot({
    incidence_object <- incidence_rec()$incidence_object
    pred_cum <- projection_rec()$pred_cum
    
    
    plot(cumulate(incidence_object)) %>%
      add_projections(pred_cum + incidence_object$n) +
      ggtitle('Cumulative incidence projection')+
      theme(axis.text.x=element_text(angle=90,hjust=1)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_date(breaks = scales::pretty_breaks(n = 20))
  })
  
  # estimate R0 ongoing
  r0_epiestm_rec <- reactive({
    incidence_object <- incidence_rec()$incidence_object
    
    res_uncertain_si <- estimate_R(incidence_object, method = "uncertain_si", 
                                      config = make_config(list(mean_si = 7.5, std_mean_si = 2, 
                                                                min_mean_si = 1, max_mean_si = 8.4, std_si = 3.4, std_std_si = 1, 
                                                                min_std_si = 0.5, max_std_si = 4, n1 = 100, n2 = 100)))
    return(res_uncertain_si)
  })
  
  output$plot_R0 <- renderPlot({
    plot_Ri(r0_epiestm_rec())
  })
  
}
