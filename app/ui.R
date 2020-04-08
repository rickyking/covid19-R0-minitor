useShinyjs()

dashboardPage(skin='red',
  dashboardHeader(title = 'COVID-19'),
  
  dashboardSidebar(
    sidebarMenu(
      fluidRow(column(12,
        selectInput('which_country', 'Select Country', choices = unique(confirmed_long_jhu$country_region), selected = 'Canada'),
        checkboxInput('cum_case_by_province', label = 'By Province', FALSE),
        airDatepickerInput('start_date', 'Select Date to cut for fit', value = as.Date('2019-12-01')),
        div(align='center',
        em('Value below extracted from WHO.'),
        ),
        numericInput('si_mu', 'Serial Inverval Mu:', value = 7.5, min = 1),
        numericInput('si_sd', 'Serial interval Sd:', value = 3.4, min = 0),
        div(align='center', 'By ', a("Yi, Jin", href="https://jinyi.me/")
            )
      ))
    )
  ),
  
  dashboardBody(

      # First tab content
              fluidRow(
                valueBoxOutput('vb_total_cases', width = 3),
                valueBoxOutput('vb_double_in_xday', width = 3),
                valueBoxOutput('vb_est_r0', width = 3),
                valueBoxOutput('vb_r0_7d', width = 3)
                
              ),
              fluidRow(
                box(
                  title = 'Cumulative Case',
                  echarts4rOutput('plot_cum_case'),
                  width=12
                ),
                
                box(
                  title = "Reproduction Number Estimates from Growth Rate",
                  echarts4rOutput('plot_r2R0')
                ),
                
                tabBox(
                  tabPanel('Realtime R0',
                           p('Assuming the first case is the imported, all other cases are local.'),
                           br(),
                           plotOutput('plot_R0')
                  ),
                  tabPanel('7d Projection',
                           plotOutput('plot_projection')
                           ),
                  title = 'Statistical epidemiology'
                )
              )
              

  )
)
