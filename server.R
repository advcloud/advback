library(data.table) 
library(dplyr)      
library(reshape2)   
library(ggplot2)
library(ggfortify)

load('weather.rda'); # for deployment 

shinyServer(function(input, output) {  
  output$plot_all_dept_sale_value <- renderPlot({ 
    dat <<- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% group_by(Dept) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number))
    
    dat_with_dept <- all_mall_weather %>% filter(Dept == input$dept)
    
    thm <<- function() {
      theme_gray(base_family = "STHeiti") + # 讓Mac使用者能夠顯示中文, Windows使用者應省略這行
        theme(text=element_text(size=18)) # 將字體調整至18號
    }
    ggplot(dat, aes(x=Dept, y=Sales_Value, fill=Dept)) + geom_bar(stat="identity") + labs(x="部門", y="銷售金額") + coord_flip() + thm() 
  })
  
  output$plot_all_dept_sale_number <- renderPlot({
    dat <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% group_by(Dept) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number))
    
    #dat_with_dept <- all_mall_weather %>% filter(Dept == input$dept)
    
    ggplot(dat, aes(x=Dept, y=Sales_Number, fill=Dept)) + geom_bar(stat="identity") + labs(x="部門", y="銷售次數") + coord_flip() + thm() 
  }) 
  
  output$plot_sale_value <- renderPlot({
    
    dat_with_dept_product <<- all_mall_weather %>% filter(Dept == input$dept) %>% 
      group_by(Family) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>% 
      arrange(desc(Sales_Value))
    
    if(input$from_top_to_low == TRUE){
      dat_with_dept_product <<- dat_with_dept_product %>% head(10)
    }else{
      dat_with_dept_product <<- dat_with_dept_product %>% tail(10)
    }
    
    ggplot(dat_with_dept_product, aes(x=Family, y=Sales_Value, fill=Family)) + geom_bar(stat="identity") + labs(x="產品", y="銷售金額") + coord_flip() + thm() 
  })
  
  output$plot_sale_number <- renderPlot({
    dat_with_dept_product <<- all_mall_weather %>% filter(Dept == input$dept) %>% 
      group_by(Family) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>% 
      arrange(desc(Sales_Value))
    
    if(input$from_top_to_low == TRUE){
      dat_with_dept_product <<- dat_with_dept_product %>% head(10)
    }else{
      dat_with_dept_product <<- dat_with_dept_product %>% tail(10)
    }
    ggplot(dat_with_dept_product, aes(x=Family, y=Sales_Number, fill=Family)) + geom_bar(stat="identity") + labs(x="產品", y="銷售次數") + coord_flip() + thm() 
  })
  
  output$plot_rain_date <- renderPlot({
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain))
    
    specific_mall_weather <- data.table(specific_mall_weather)
    setkey(specific_mall_weather,"Date")
    uni_specific_mall_weather <- unique(specific_mall_weather)
   
    ggplot(uni_specific_mall_weather, aes(x=Date, y=Sales_Value,colour=is.match)) + geom_point(size=4) + 
      labs(x="日期", y="銷售金額") + thm() 
  })
  
  output$table_most_sale_product <- renderDataTable({
    specific_mall_weather <<- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number))
    
    specific_mall_weather <- data.table(specific_mall_weather)
    specific_mall_weather[order(-rank(Sales_Value), is.match)]
  }) 
  
  output$plot_pyramid_of_hotsale <- renderPlot({
    
    specific_mall_weather <- data.table(specific_mall_weather)
    item_we_want <- specific_mall_weather[order(-rank(Sales_Number),is.match)] %>% filter(is.match == FALSE) %>% head(10) %>% select(Family)
    specific_mall_weather <- specific_mall_weather %>% filter(Family %in% c(item_we_want$Family) )
    
    ggplot(specific_mall_weather, aes(x=Family, y=Sales_Number, fill=is.match)) + 
      geom_bar(data=filter(specific_mall_weather, is.match==TRUE), stat="identity") +
      geom_bar(aes(y=Sales_Number*(-1)), data=filter(specific_mall_weather, is.match==FALSE), stat="identity") + 
      labs(x="產品", y="銷售次數") + thm() + coord_flip()
  })
  
  output$plot_related_of_hotsale <- renderPlot({    
    specific_mall_weather <- data.table(specific_mall_weather)
    item_we_want <- specific_mall_weather[order(-rank(Sales_Number),is.match)] %>% filter(is.match == FALSE) %>% head(10) %>% select(Family)
    specific_mall_weather <- specific_mall_weather %>% filter(Family %in% c(item_we_want$Family) )
    
    ggplot(specific_mall_weather, aes(x=Family, y=Sales_Number, fill=is.match)) + 
      geom_bar(stat="identity", position = "fill") + 
      geom_hline(yintercept=0.5, lty=2, lwd=1, col="gray") + 
      labs(x="產品", y="銷售次數之相對比例") + thm() +
      coord_flip()
  })
  
  output$table_sale_number_diff <- renderDataTable({
    # ================資料準備區=======================
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <- dcast(specific_mall_weather, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    # =================資料準備區 end=====================
    
    # 列出下雨期間各部門最熱賣分類 (Family)
    tab %>% arrange(-Event)
    #arrange(tab, -abs(Diff)) %>% head(10)
    #filter(tab, Regular>0) %>% arrange(-Ratio) %>% head(10)
    arrange(tab, -abs(Diff)) %>% head(100) %>% arrange(-Ratio) %>% head(10)
  })
  
  output$plot_lm_hotsale <- renderPlot({
    # ================資料準備區=======================
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <- dcast(specific_mall_weather, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    # =================資料準備區 end=====================
    ggplot(tab, aes(x=Regular, y=Event)) + geom_point(size=4) + thm() + 
      stat_smooth(method="lm", formula=y~x, se=FALSE, size=2)
  })
  
  output$plot_lm_curve <- renderPlot({
    # ================資料準備區=======================
   
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <- dcast(specific_mall_weather, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    # =================資料準備區 end=====================
    
    fit <- lm(Event~Regular, data=tab)
    autoplot(fit, which=1)
    
  })
  
  output$plot_ml_LM <- renderPlot({
    # ================資料準備區=======================
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <- dcast(specific_mall_weather, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    fit <- lm(Event~Regular, data=tab)
    tab_ml <- mutate(tab, Fitted=predict(fit), Residuals=residuals(fit), 
                   Leverage=hatvalues(fit), StdRes=rstandard(fit), CookD=cooks.distance(fit))
    # =================資料準備區 end=====================
    
    tab.res10 <- arrange(tab_ml, -abs(Residuals)) %>% head(10)
    
    ggplot(tab_ml, aes(x=Regular, y=Event)) + geom_point() +
      stat_smooth(method="lm", formula=y~x, se=FALSE, size=2) + 
      geom_text(aes(label=Family, family="STHeiti"), data=tab.res10,
                hjust=0, vjust=-0.5, col=4) 
    
  })
  
  output$plot_ml_model_diagnose <- renderPlot({
    # ================資料準備區=======================
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      mutate(is.match = is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon & Rainfall < max(input$rain) & Rainfall > min(input$rain)) %>%
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <- dcast(specific_mall_weather, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    fit <- lm(Event~Regular, data=tab)
    tab_ml <- mutate(tab, Fitted=predict(fit), Residuals=residuals(fit), 
                     Leverage=hatvalues(fit), StdRes=rstandard(fit), CookD=cooks.distance(fit))
    # =================資料準備區 end=====================
    tab.CD10 <- arrange(tab_ml, -CookD) %>% head(10)
    
    ggplot(tab_ml, aes(x=Leverage, y=Residuals, size=CookD)) + geom_point(shape=1) + 
      scale_size(range=c(2,20)) +  geom_text(aes(label=Family, family="STHeiti"), data=tab.CD10,
                                             hjust=0, vjust=-0.5, col=4) 
  })
  
  output$plot_rain_data_speficic <- renderPlot({
    specific_mall_weather <- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      filter( is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon ) %>%
      mutate(is.match = Rainfall < max(input$rain) & Rainfall >= min(input$rain))
    
    #specific_mall_weather <- data.table(specific_mall_weather)
    #setkey(specific_mall_weather,"Date")
    #uni_specific_mall_weather <- unique(specific_mall_weather)
    ggplot(specific_mall_weather, aes(x=Date, y=Sales_Value,colour=is.match)) + geom_point(size=4) + 
      labs(x="日期", y="各產品銷售金額") + thm() 
  })
  
  output$table_most_sale_product_spefic <- renderDataTable({
    specific_mall_weather_filter_first <<- all_mall_weather %>% filter( Store %in% input$location_checkbox ) %>% 
      filter( Dept == input$dept ) %>% 
      filter( is.weekday == input$is_week_day & is.bigday == input$is_big_day & is.typhoon == input$is_typhon ) %>%
      mutate(is.match = Rainfall < max(input$rain) & Rainfall >= min(input$rain))
  })
  
  output$plot_lm_hotsale_specific <- renderPlot({
    specific_mall_weather_filter_first_local <- specific_mall_weather_filter_first %>% 
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <<- dcast(specific_mall_weather_filter_first_local, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <<- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    # =================資料準備區 end=====================
    ggplot(tab, aes(x=Regular, y=Event)) + geom_point(size=4) + thm() + 
      stat_smooth(method="lm", formula=y~x, se=FALSE, size=2)
  })
  
  output$plot_ml_LM_specific <- renderPlot({
    specific_mall_weather_filter_first_local <- specific_mall_weather_filter_first %>% 
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <<- dcast(specific_mall_weather_filter_first_local, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <<- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    # ================資料準備區=======================
    fit <<- lm(Event~Regular, data=tab)
    tab_ml <<- mutate(tab, Fitted=predict(fit), Residuals=residuals(fit), 
                     Leverage=hatvalues(fit), StdRes=rstandard(fit), CookD=cooks.distance(fit))
    # =================資料準備區 end=====================
    
    tab.res10 <- arrange(tab_ml, -abs(Residuals)) %>% head(10)
    
    ggplot(tab_ml, aes(x=Regular, y=Event)) + geom_point() +
      stat_smooth(method="lm", formula=y~x, se=FALSE, size=2) + 
      geom_text(aes(label=Family, family="STHeiti"), data=tab.res10,
                hjust=0, vjust=-0.5, col=4) 
  })
  
  output$plot_ml_model_diagnose_spcific <- renderPlot({
    specific_mall_weather_filter_first_local <- specific_mall_weather_filter_first %>% 
      group_by(Family,is.match) %>% 
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>%
      arrange(desc(Sales_Number))
    
    tab <<- dcast(specific_mall_weather_filter_first_local, Family~is.match, fill=0, value.var="Sales_Number") %>%
      setNames(c("Family", "Event", "Regular"))
    tab <<- mutate(tab, Diff=Event-Regular, Ratio=Event/Regular)
    fit <<- lm(Event~Regular, data=tab)
    tab_ml <<- mutate(tab, Fitted=predict(fit), Residuals=residuals(fit), 
                      Leverage=hatvalues(fit), StdRes=rstandard(fit), CookD=cooks.distance(fit))
    tab.CD10 <- arrange(tab_ml, -CookD) %>% head(10)
    ggplot(tab_ml, aes(x=Leverage, y=Residuals, size=CookD)) + geom_point(shape=1) + 
      scale_size(range=c(2,20)) +  geom_text(aes(label=Family, family="STHeiti"), data=tab.CD10,
                                             hjust=0, vjust=-0.5, col=4) 
  })
  
  
  output$plot_max_diff_product <- renderPlot({
    diff_data <- specific_mall_weather_filter_first %>% group_by(Family,is.match) %>% summarise(avg_Sales_Value = mean(Sales_Value)) 
    diff_data <- dcast(diff_data,Family~is.match, fill=0, value.var = "avg_Sales_Value") %>%
      setNames(c("Family", "not_match", "match"))
    diff_data <- diff_data %>% mutate(diff = match - not_match)
    diff_data <- arrange(diff_data, -abs(diff)) %>% head(10)
    ggplot(diff_data, aes(x=Family, y=abs(diff),fill=Family)) + 
      geom_bar(stat="identity") + 
      labs(x="產品", y="下雨天與非下雨天差異") + thm() + coord_flip()
  })
  
  output$LM_mode <- renderTable ({
    targetdata <- all_mall_weather %>% 
      filter( Store %in% input$location_checkboxG ) %>%
      #filter( Store %in% "H1" ) %>%
      filter(Dept == input$deptG) %>%
      #filter(Dept == "工具類") %>%
      group_by(Family) %>%
      summarise(Sales_Value=sum(Sales_Value), Sales_Number=sum(Sales_Number)) %>% 
      arrange(desc(Sales_Number))
    
    if(input$from_top_to_low == "最高10名"){
      targetdata10 <- targetdata %>% head(10)
    }else{
      targetdata10 <- targetdata %>% tail(10)
    }
    
    selectdata <- filter(all_mall_weather,Family == targetdata10$Family)      
    
    
    fit <<- lm( selectdata$Sales_Number ~ selectdata[,input$factor])
    summary(fit)
  })
  
  output$LM_mode_plot <- renderPlot ({
    ggplot(fit)+
      geom_line()
  })
  
})
