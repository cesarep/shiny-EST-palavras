library(shiny)
library(readr)
source('katex.r', encoding = 'utf-8')

indio <- gsub("[,.!?\r ]", " ", read_file("letras/indio.txt"))

shinyUI(navbarPage("Amostragem", header = list(includeCSS("www/estilo.css"), KaTeX()),
		tabPanel("População", sidebarLayout(
			sidebarPanel(
				textAreaInput("texto", "Texto", value = indio, height = "15em"),
				radioButtons('txtsel', NULL, list("Índios" = 1, 
															 "Monte Castelo" = 2, 
															 "Cowboy Fora-da-Lei" = 3, 
															 "Personalizado" = 4), inline = F)
			),
			mainPanel(plotOutput("plotP"),
						 htmlOutput("infoP"))
		)),	tabPanel("Amostra", sidebarLayout(
			sidebarPanel(
				splitLayout(
					numericInput("tamanho", "Tamanho", 10, 1, step = 1),
					actionButton("amostrar", "Nova Amostra", style="margin-top: 25px;", width = '100%')
				),
				strong("Palavras"),
				textOutput("amostra")
			),
			mainPanel(plotOutput("plotA"),
						 htmlOutput("infoA"))
		)),
		footer = list(
			hr(),
			flowLayout(id = "cabecario",
						  p(strong("Apoio:"), br(), img(src="NEPESTEEM.png")),
						  p(strong("Agradecimento:"), br(), img(src="FAPESC.png")),
						  p(strong("Desenvolvido por:"), br(), "César Eduardo Petersen")
			)
		)
))
