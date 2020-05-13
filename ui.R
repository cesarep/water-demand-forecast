library(shiny)
library(DT)
library(shinycssloaders)
library(dplyr)

options(spinner.type = 5, spinner.color = '#AAAAAA')

shinyUI(navbarPage("Water Demand Forecast", fluid = FALSE, inverse = TRUE,  header = includeCSS("www/style.css"),
	#### IMPORT DATA ####
	tabPanel("Import", icon = icon('file-upload'), sidebarLayout(
		sidebarPanel(
			fileInput("file", "Choose a CSV or Excel File", accept = c("text/csv", "text/plain",
				"application/vnd.ms-excel",
				"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
			),
			conditionalPanel("output.filetype == 'csv'", ## CSV OPTIONS
				checkboxInput('csv_header', 'Use first row as Header', TRUE),
				selectInput('csv_separator', 'Separator', c(", (Comma)" = ",",
															"; (Semicolon)" = ";",
															"Space" = " ",
															"Tabulation" = "\t")
				),
				fluidRow(
					column(6, selectInput('csv_decimal', 'Decimal', c(". (Period)" = ".", ", (Comma)" = ","))),
					column(6, selectInput('csv_quote', 'Quote', c("\" (Double)" = "\"", "' (Single)" = "'")))
				),

			),
			conditionalPanel("output.filetype == 'xls' || output.filetype == 'xlsx'", ## EXCEL OPTIONS
				numericInput('excel_sheet', "Sheet", 1, 1)
			)
		),
		mainPanel(dataTableOutput('table'))
	)),
	#### DATA OPTIONS ####
	tabPanel('Options', icon = icon('sliders-h'), sidebarLayout(
		sidebarPanel(##width = 3,
			helpText("Incluir texto explicativo?"),
			fluidRow(
				column(6, selectInput('data_column', "Column", c('Import a file first' = 'V1'))),
				column(6, selectInput('data_frequency', 'Frequency',  c("Monthly" = 12,
																		"Quarterly" = 4,
																		"Semiannual" = 2,
																		"Annual" = 1,
																		"Daily" = 365,
																		"Custom" = -1)
				))
			),
			conditionalPanel("input.data_frequency == -1", numericInput('data_frequency_custom', "Frequency", 12)),
			fluidRow(
				column(6, numericInput('data_year', 'Start Year', format(Sys.Date(), "%Y"))),
				column(6, numericInput('data_period', 'Offset', 1))
			),
			checkboxInput('data_validation', 'Use last observations as validation data'),
			conditionalPanel("input.data_validation == true",
				numericInput('data_validation', "Qty.", 3, 1, 6)
			)
		),
		mainPanel(
			plotOutput('option_plot') %>% withSpinner()
		)
	)),
	#### DATA ANALYSIS ####
	tabPanel('Pre-Analysis', icon = icon('chart-bar'), fluidPage(
		splitLayout(
			plotOutput('analy_acf', height = '300px') %>% withSpinner(),
			plotOutput('analy_pacf', height = '300px') %>% withSpinner()
		),
		plotOutput('analy_stl', height = '450px') %>% withSpinner(),
		"INCLUIR DEMAIS TESTES"
	)),
	#### MODEL SELECTION ####
	tabPanel('Model', icon = icon('calculator'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			plotOutput('model_ets_res') %>% withSpinner()
		),
		tabPanel('ARIMA',
			plotOutput('model_arima_res') %>% withSpinner()
		)
	)),
	#### FORECAST ####
	tabPanel('Forecast', icon = icon('chart-line'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			plotOutput('fore_ets') %>% withSpinner()
		),
		tabPanel('ARIMA',
			plotOutput('fore_arima') %>% withSpinner()
		)
	))
))
