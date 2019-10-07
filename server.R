library(shiny)
library(readr)

indio <- gsub(" +", " ", gsub("[,.!?\r]+", " ", read_file("indio.txt")))
monte <- gsub(" +", " ", gsub("[,.!?\r]+", " ", read_file("monte.txt")))
cowboy = gsub(" +", " ", gsub("[,.!?\r]+", " ", read_file("cowboy.txt")))
letras = c(indio, monte, cowboy)

shinyServer(function(input, output, session) {
   observeEvent(c(input$texto, input$tamanho, input$amostrar), {
      ### POPULAÇÃO
      texto <- gsub(" +", " ", gsub("[,.\r!?+]", " ", input$texto))
      updateTextInput(session, 'texto', value = texto)
      if(!(texto %in% letras)){
         updateRadioButtons(session, inputId = 'txtsel', selected = 4)
      }
      
      chars <- strsplit(trimws(gsub("[\n ]+", " ", texto)), "[ +]")[[1]]
      num = nchar(chars)
      lim = max(15, max(num))
      output$plotP = renderPlot({
         hist(num, xlab = "Número de Letras", col="grey", axes = F, breaks = 1:(lim+1)-0.5, main = "", freq = F)
         axis(1, 1:lim)
         axis(2)
      })
      output$infoP = renderUI({
         tags$table(class="table shiny-table table-striped spacing-s",
                    style="width: auto;text-align: center; margin: 0 auto;",
            tags$thead(
               tags$tr(
                  tags$th("Número de Palavras"),
                  tags$th("Média"),
                  tags$th("Desvio Padrão"),
                  tags$th("Mínimo"),
                  tags$th("Mediana"),
                  tags$th("Máximo")
               )
            ), tags$tbody(
               tags$tr(
                  tags$td(sprintf("%.0f", length(num))),
                  tags$td(sprintf("%.2f", mean(num))),
                  tags$td(sprintf("%.2f", sd(num))),
                  tags$td(sprintf("%.0f", min(num))),
                  tags$td(sprintf("%.1f", median(num))),
                  tags$td(sprintf("%.0f", max(num)))
               )
            )
         )
      })
      
      
      ### AMOSTRA
      palavras=""
      try({
      	palavras <- sample(chars, input$tamanho)
      }, silent = T)
      numA = nchar(palavras)
      output$amostra = renderText({
      	paste(palavras, collapse = ', ')
      })
      output$plotA = renderPlot({
         if(input$tamanho >= 50){
            hist(numA, xlab = "Número de Letras", col="grey", axes = F, breaks = 1:(lim+1)-0.5, main = "", freq = F)
            axis(2)
         }
      	else
      	   stripchart(numA, "stack", at = 0, pch = 16, xlim=c(0.5, lim+0.5), axes = F, offset = 0.5, xlab = "Número de Letras")
      	axis(1, 1:lim)
      })
      err = qt(.975, length(numA))*sd(numA)/sqrt(length(numA))
      output$infoA = renderUI(tags$div(
         tags$table(class="table shiny-table table-striped spacing-s",
                    style="width: auto;text-align: center; margin: 0 auto;",
                    tags$thead(
                       tags$tr(
                          tags$th("Número de Palavras"),
                          tags$th("Média"),
                          tags$th("Desvio Padrão"),
                          tags$th("Mínimo"),
                          tags$th("Mediana"),
                          tags$th("Máximo")
                       )
                    ), tags$tbody(
                       tags$tr(
                          tags$td(sprintf("%.0f", length(numA))),
                          tags$td(sprintf("%.2f", mean(numA))),
                          tags$td(sprintf("%.2f", sd(numA))),
                          tags$td(sprintf("%.0f", min(numA))),
                          tags$td(sprintf("%.1f", median(numA))),
                          tags$td(sprintf("%.0f", max(numA)))
                       )
                    )
         ),
         tags$p(withMathJax(sprintf("$$ P(%.2f \\le \\mu \\le %.2f) = 0.95$$", mean(numA)-err, mean(numA)+err)))
      ))
      # output$infoA = renderUI({
         # withMathJax(
         #    tags$ul(
         #       tags$li("$$ \\overline{X} \\in [2,3] $$")
         #       
         #    )
         # )
      # })
      
   })

   observeEvent(input$txtsel, {
      texto <- letras[as.numeric(input$txtsel)]
      if(input$txtsel %in% 1:3){
         updateTextInput(session, inputId = 'texto', value = texto)
      }
   })
  
})
