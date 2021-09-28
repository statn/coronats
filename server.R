library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)
library(DT)
library(ggrepel)
library(ggpubr)
library(forecast)
library(pryr)
library(shiny)
library(viridis)

#### Funktion für Timeseries ####
# Ursprünglich erstellt, um auch Genesen und Tod mit dynamischer Regression vorherzusagen; abgewandelt für Shiny
# Logik: Prä-allosziere Dataframe, fülle es mit Vorhersage; ermöglicht durch daymax-days auch retrospektive Vorhersage
# df: Dataframe Input; col_cases: Spaltenname mit relevanten Timeseries-Daten; method: Art der Vorhersage (arima, ets, ...)
# robust: Median ja/nein; daypred: Anzahl der vorhergesagten Tage; daymax: max(Meldedatum)-days; days: Tage Retrospektive; lambda: Anpassungsparameter, standardmäßig 'auto'
forecast_append <- function(df, col_cases, method, robust, daypred, daymax, days, lambda){
  df_append <- data.frame(Meldedatum = character(daypred), Landkreis = character(daypred))
  lk_temp <- df$Landkreis[1]
  df_temp <- df %>% gather(key = 'Type', value = 'Factor', col_cases) %>% filter(Meldedatum > daymax-days, Meldedatum <= daymax)
  rm(df) # Speicher freigeben
  df_append$Meldedatum <- daymax + 1:daypred
  df_append$Landkreis <- lk_temp
  df_append$Anteil <- col_cases
  counter = 0
  tryCatch({
    ts_obj <- ts(log(na.omit(df_temp$Factor)), frequency = 7) # Timeseries mit log-Transformation, damit Werte > 0
    fit <- ts_obj %>% stl(s.window='periodic', robust=robust) # Fitting
    rm(ts_obj) # Speicher freigeben
    fit <- fit %>% seasadj() %>% stlf(h=daypred,method=method, biasadj = T, lambda = lambda) # Vorhersage
    df_append['mean'] <- round(exp(fit$mean[1:daypred]))
    df_append['hi95'] <- round(exp(fit$upper[,1][1:daypred]))
    df_append['lo95'] <- round(exp(fit$lower[,1][1:daypred]))
    rm(fit)
  },
  error=function(cond){
    warning("STL didn't succeed. Set lambda to NULL.\n") # In seltenen Fällen kann Lambda Probleme verursachen
    test <- ts(log(na.omit(df_temp$Factor)), frequency = 7)
    fit <- test %>% stl(s.window = 'periodic', robust = robust)
    rm(test)
    fit <- fit %>% seasadj() %>% stlf(h=daypred,method=method, biasadj = T, lambda = NULL)
    df_append['mean'] <- round(exp(fit$mean[1:daypred]))
    df_append['hi95'] <- round(exp(fit$upper[,1][1:daypred]))
    df_append['lo95'] <- round(exp(fit$lower[,1][1:daypred]))
  })
  rm(df_temp) # Speicher freigeben
  return(df_append)
}

#### Server ####

server <- function(input, output, session) {
  
  cat("Retrieving dataset...")
  RKI_data = read_csv("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data")
  cat("Success.\n")
  #load("~/Desktop/rki.rda") # zu Entwicklungszwecken
  sprintf("Size of data frame: %sMB", round(pryr::object_size(RKI_data)/1048576))
  RKI_data$Meldedatum = as.Date(RKI_data$Meldedatum, format = "%Y/%m/%d")
  set_lambda='auto' # Parameter für Vorhersage-Input
  max_date <- max(RKI_data$Meldedatum)
  unique_city <- unique(RKI_data$Landkreis)
  
  makeReactiveBinding("RKI_data")
  values <- reactiveValues()
  
  updateSelectInput(session, inputId = "spot", choices = unique_city, selected = "SK Dortmund") # Zeige Auswahl, nachdem Daten verfügbar sind

  # Verschiedene Datentransformationen des Original-Datensatzes
  df_filter <- reactive({
    values$daymax = max_date - input$daysim
    values$set_days_use = input$set_days_use
    data_rki <- RKI_data
    data_rki <- data_rki %>%
      select(Landkreis, Meldedatum, AnzahlFall, AnzahlGenesen, AnzahlTodesfall) %>%
      filter(Landkreis == input$spot) %>%
      group_by(Landkreis, Meldedatum) %>%
      summarize(Neu = sum(AnzahlFall), Rehab = sum(AnzahlGenesen)+sum(AnzahlTodesfall), Tod = sum(AnzahlTodesfall)) %>%
      pivot_longer(cols=Neu:Tod, names_to = "Anteil", values_to = "Anzahl") %>%
      arrange(Meldedatum)
  }) 
  df_fltwide <- reactive({ 
   data_rki <- df_filter() %>% spread(Anteil, Anzahl) %>% mutate(Aktiv = Neu-Rehab) %>% group_by(Landkreis, Meldedatum)
  })
  # Dataframe mit Vorhersage
  df_forecast <- reactive({
   data_rki <- forecast_append(df_fltwide(), "Neu", method=input$method, robust=input$robust, daypred=input$daypred, daymax=isolate(values$daymax) , days=isolate(input$set_days_use), lambda=set_lambda)
    })
  rm(data_rki) # Speicher freigeben
  
  # Dynamische I/O-Berechnungen für Werte im Plot
  observe({
    values$aktval <- df_fltwide() %>% group_by(Landkreis) %>% summarize(Akt = sum(Aktiv)) %>% pull(Akt) # Aktive Fälle
    values$inzp <- df_forecast() %>% group_by(Landkreis) %>% summarize(Inz7p = sum(mean)) %>% pull(Inz7p) # Mittelwert Vorhersageintervall
    values$inzp_upi <- df_forecast() %>% group_by(Landkreis) %>% summarize(Inz7p = sum(hi95)) %>% pull(Inz7p) # Oberes Limit Vorhersageintervall
    values$inzp_lpi <- df_forecast() %>% group_by(Landkreis) %>% summarize(Inz7p = sum(lo95)) %>% pull(Inz7p) # Unteres Limit Vorhersageintervall
    values$vars <-  data.frame("Landkreis" = input$spot, "Population" = input$population)
    values$inz2 <-  df_filter() %>% filter(Meldedatum >= max(Meldedatum)-6) %>%
      group_by(Landkreis) %>% summarize(Summe7 = sum(Anzahl))
    values$inz <- full_join(values$vars, values$inz2, by = "Landkreis") %>% mutate(Inzidenz = round(Summe7*100000/input$population)) # Inzidenz
    values$inz_prognose <- round(values$inzp*100000/(input$population*(input$daypred/7))) # Mittlere Prognose der Inzidenz
    values$inz_prognose_min <- round(values$inzp_lpi*100000/(input$population*(input$daypred/7))) # Unteres Limit Inzidenz
    values$inz_prognose_max <- round(values$inzp_upi*100000/(input$population*(input$daypred/7))) # Oberes Limit der Inzidenz
  })
  
  # Plot
  output$plot <- renderPlot({
    
    max1 <- max(df_forecast()$Meldedatum)
    max2 <- max(df_filter()$Meldedatum)
    xlim_upper <- if_else(max1 > max2, max1, max2)
    xmax = input$xmax
    xmax = ifelse(input$sync == T, input$set_days_use, input$xmax) + input$daysim
    
    df_filter_ <- df_filter()
    df_filter_$Anteil <- plyr::revalue(df_filter_$Anteil, c(Rehab = "davon genesen", Tod = "davon gestorben"))
    df_filter_$Anteil <- factor(df_filter_$Anteil, levels=c("Neu", "davon genesen", "davon gestorben"))
    
    df_forecast_ <- df_forecast()
    df_filter_x <- filter(df_filter_, Anteil == "Neu" & Meldedatum %in% df_forecast_$Meldedatum)
    df_forecast_x <- filter(df_forecast_, Anteil == "Neu" & Meldedatum %in% df_filter_$Meldedatum)
    values$abw <- as.numeric(df_forecast_x$mean - df_filter_x$Anzahl)# Abweichung
    abw_abs <- round(sum(values$abw)/length(df_forecast_x$mean)) # Durchschnittliche Abweichung
    abw_sd <- round(sqrt(sum(values$abw^2)/length(df_forecast_x$mean))) #SD der Abweichung

    d <- ggplot(df_filter_,aes(x=Meldedatum, y=Anzahl, fill=Anteil, group=Anteil))+
      geom_area(position="identity", alpha=.8)+
      theme_minimal()+
      scale_x_date(date_breaks = "1 month", guide=guide_axis(angle=45))+
      scale_y_continuous(sec.axis = sec_axis(~ ., breaks = df_filter_ %>% group_by(Anteil) %>% top_n(1,Meldedatum) %>% pull(Anzahl)))+
      facet_grid(Landkreis~.)+ # damit der SK/LK am Rand der Zeile angezeigt wird
      labs(title=paste('Corona-Zeitreihenanalyse (für den Privatgebrauch)'), subtitle=sprintf("Aktive Fälle: %s   Aktuelle 7-Tage-Inzidenz: %s pro 100.000 Einwohner",
                                                                     values$aktval, as.character(values$inz$Inzidenz)))

    d <- d + geom_line(data=df_forecast_, aes(x=Meldedatum, y=mean), lty=2)+
      theme(legend.position="bottom")+
      coord_cartesian(xlim=c(Sys.Date()-xmax, xlim_upper), expand=F)+
      scale_color_viridis_d()+
      scale_fill_viridis_d()
    
    if (input$confid == T) {
      d <- d + geom_ribbon(data=df_forecast() %>% filter(Anteil=="Neu"), aes(x=Meldedatum,y=mean,ymin=lo95,ymax=hi95), alpha=0.3, lty=0)
    }
    
    if (input$daysim > 1) { # mind. 2 Datenpunkte notwendig
      d <- d + labs(y = "Täglich gemeldete Neuinfektionen",
                    caption = sprintf("7-Tage-Inzidenz im Vorhersagezeitraum (Mittelwert & 95%%-Vorhersageintervall): %s [%s, %s] pro 100.000 Einwohner\nAbsolute Vorhersageabweichung pro Tag (Mittelwert): %s (SD = %s)\n\nFallzahlen: Robert Koch-Institut", values$inz_prognose, values$inz_prognose_min, values$inz_prognose_max, abw_abs, abw_sd))
    } else {
      d <- d + labs(y = "Täglich gemeldete Neuinfektionen",
                    caption = sprintf("7-Tage-Inzidenz im Vorhersagezeitraum (Mittelwert & 95%%-Vorhersageintervall): %s [%s, %s] pro 100.000 Einwohner\n\nFallzahlen: Robert Koch-Institut", values$inz_prognose, values$inz_prognose_min, values$inz_prognose_max))
    }
    
    print(d)
    
  })
    
  output$plot2 <- renderPlot({ if(input$daysim > 1) plot(density(values$abw), # mind. 2 Datenpunkte notwendig
                                                         main = "Verteilung der Vorhersageabweichung",
                                                         ylab = "Dichte",
                                                         xlab = "Abweichung der Modellwerte von den tatsächlichen Werten",
                                                         sub = "Abweichung > 0: Modell überschätzt; Abweichung < 0: Modell unterschätzt"); abline(v=0) })
}

