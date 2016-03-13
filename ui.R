library(data.table) 
library(dplyr)      
library(reshape2)   
library(ggplot2)

load('weather.rda'); # for deployment 
shinyUI(
  navbarPage("Advmall",
    tabPanel("EDA 分析", 
      navlistPanel(
        tabPanel("1. 先選地區",
          checkboxGroupInput(inputId='location_checkbox', label="選地區", choices=unique(all_mall_weather$Store), selected = NULL, inline = FALSE),
          plotOutput('plot_all_dept_sale_value'),
          plotOutput('plot_all_dept_sale_number')
        ),
        tabPanel("2. 選部門，並查看各產品營業額比較",
          selectInput(inputId='dept', label='選部門', choices=unique(as.character(all_mall_weather$Dept))),
          checkboxInput(inputId='from_top_to_low', '最高前10名', value = TRUE),
          h2('銷售金額'),
          plotOutput('plot_sale_value'),
          h2('銷售次數'),
          plotOutput('plot_sale_number')
        ),
        tabPanel("3. 選雨量，查看商品銷售額",
          checkboxInput(inputId='is_big_day', 'big day?', value = FALSE),
          checkboxInput(inputId='is_week_day', 'week day?', value = FALSE),
          checkboxInput(inputId='is_typhon', 'typhon?', value = FALSE),
          sliderInput("rain",
                      "雨量:",
                      min = 0,
                      max = 507,
                      value = c(0,507)),
          h2('符合條件 VS 不符合條件的 日期'),
          plotOutput('plot_rain_date'),
          h2('查看符合條件 VS 不符合條件的產品銷售總額'),
          dataTableOutput('table_most_sale_product')
        ),
        tabPanel("4. 工人智慧",
          h2('探索下雨前後，熱賣商品差異'),
          plotOutput('plot_pyramid_of_hotsale'),
          h2('相對比例長條圖'),
          plotOutput('plot_related_of_hotsale'),
          h2('銷售次數差異Top10'),
          dataTableOutput('table_sale_number_diff'),
          h2('使用LM找出Outlier/Leverge'),
          plotOutput('plot_lm_hotsale'),
          h2('使用LM autoplot畫出 outlier點'),
          plotOutput('plot_lm_curve')
        ),
        tabPanel("5. 人工智慧",
          h2('讓機器learning熱銷部門商品，並找出Outlier'),
          plotOutput('plot_ml_LM'),
          h2('最後診斷圖'),
          plotOutput('plot_ml_model_diagnose',width = "100%", height = "400px")
        )
      )
    ),
    tabPanel("揪竟，要如何組合？",
      h2('先濾掉符合big day、week day、typhon資料後，依雨量做分群，產品銷售金額'),
      plotOutput('plot_rain_data_speficic'),
      h2('查看資料'),
      dataTableOutput('table_most_sale_product_spefic'),
      h2('讓機器learning熱銷部門商品，並找出Outlier'),
      plotOutput('plot_ml_LM_specific'),
      h2('最後診斷圖'),
      plotOutput('plot_ml_model_diagnose_spcific',width = "100%", height = "400px")
    ),
    tabPanel("再分析！",
      navlistPanel(
        tabPanel("來分析吧",
          checkboxGroupInput(inputId='location_checkboxG', 
                            label="1. 選地區",
                            choices=unique(all_mall_weather$Store),
                            selected=unique(all_mall_weather$Store)),    
          selectInput(inputId='deptG', label='選部門', choices=unique(as.character(all_mall_weather$Dept))), 
          radioButtons(inputId='from_top_to_low',label='Top or tail',
                      choices = c("最高10名","最後10名" )),
          radioButtons('factor', 'Choose Factor:', choices=names(all_mall_weather),inline = TRUE),
          tableOutput('LM_mode')
        )         
      )
    ),
    tabPanel("結果！",
      h2("找出有下雨與沒有下雨的天數銷售額差異最大的產品們"),
      plotOutput('plot_max_diff_product')
      )
  )
)