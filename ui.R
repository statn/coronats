ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel( condition = "document.getElementById('plot').childElementCount == 0",
      helpText('Bitte zurücklehnen und entspannen, während die App initialisiert wird...\nDies kann etwa 10 Sekunden dauern.\n\nFalls die Meldung "Disconnected from the server" erscheint, bitte noch einmal probieren.')
      ),
      conditionalPanel( # Sidepanel wird erst gezeigt, nachdem der Datensatz heruntergeladen und aufbereitet wurde
        condition = "document.getElementById('plot').childElementCount != 0", # Plot ist erst verfügbar, wenn Daten vorhanden sind 
        selectInput(inputId = "spot", "Stadt-/Landkreis", choices = c("SK Dortmund", "Bitte warten..."), selected = "SK Dortmund"),
        numericInput(inputId = "population", "Einwohner (manuelle Eingabe)", value = 588250),
        selectInput(inputId = "method", "Methode für Zeitreihenanalyse", choices = c("arima", "naive", "ets", "rwdrift"), selected = "arima"),
        checkboxInput(inputId = "robust", "Median statt Mittelwert verwenden", value = T),
        checkboxInput(inputId = "confid", "95%-Vorhersageintervall anzeigen", value = T),
        sliderInput(inputId = "xmax", "Wie viele Tagesdaten sollen in der Grafik angezeigt werden?", min = 28, max = 364, value = 112, step = 7),
        sliderInput(inputId = "set_days_use", "Wie viele Tagesdaten sollen im Modell berücksichtigt werden?", min = 35, max = 364, value = 364, step = 7),
        checkboxInput(inputId = "sync", "Zeige in der Grafik alle Tagesdaten, die im Modell berücksichtigt werden", value = F),
        sliderInput(inputId = "daypred", "Vohersagezeitraum (Tage)", min = 7, max = 28, value = 14, step = 7),
        sliderInput(inputId = "daysim", "Rückblick: Wie viele Tage in der Vergangenheit soll die Vorhersage beginnen?", min = 0, max = 56, value = 3, step = 1)
        )
      ),
    mainPanel(
      plotOutput(outputId = "plot"),
      conditionalPanel( # Verteilung der Vorhersageabweichungen wird nur dann geplottet, wenn retrospektive Vorhersage verwendet wird
        condition="input.daysim > 1", # mind. 2 Datenpunkte notwendig
        plotOutput(outputId = "plot2")
      ))
  ))
