library(shiny)
library(DT)
library(readxl)
library(forecast)
library(ggplot2)
library(plotly)

theme_update(plot.title = element_text(hjust = 0.5, size = 16))

shinyServer(function(input, output, session) {

	#### IMPORT DATA TAB ####
	## Import file
	import = reactive({
		req(input$file)

		filetype = tolower(tail(strsplit(input$file$name, "\\.")[[1]], 1))
		file = input$file$datapath

		output$filetype <- reactive({ filetype })
		outputOptions(output, "filetype", suspendWhenHidden = FALSE)

		if(filetype == 'csv') {
			data = read.csv(file, input$csv_header, input$csv_separator, input$csv_quote, input$csv_decimal)
		}

		if(filetype == 'xls' || filetype == 'xlsx'){
			data = read_excel(file, input$excel_sheet)
		}

		updateSelectInput(session, 'data_column', choices = colnames(data))

		return(data)
	})


	## Display file preview
	output$table <- renderDataTable({
		datatable(import(), rownames = FALSE)
	})

	#### DATA OPTIONS TAB ####

	time_series = debounce(reactive({
		req(import())

		print(input$data_column)
		try({
			data = import()[[input$data_column]]

			updateTextInput(session, 'data_label', value=input$data_column)

			freq = as.numeric(ifelse(input$data_frequency != '-1', input$data_frequency, input$data_frequency_custom))
			start = c(input$data_year, input$data_period)

			#if(is.null(data)) return(NULL)

			time_s = ts(data, start = start, frequency = freq)
			return(tsclean(time_s))
		}, T)


	}), 1000)

	data_label = debounce(reactive({input$data_label}), 2500)

	output$option_plot <- renderPlot({
		req(time_series())
		autoplot(time_series(), xlab = "Period", ylab=data_label())
	})

	#### DATA ANALYSIS ####

	output$analy_acf <- renderPlot({
		req(time_series())
		ggAcf(time_series(), main='ACF')
	})

	output$analy_pacf <- renderPlot({
		req(time_series())
		ggPacf(time_series(), main='PACF')
	})

	output$analy_stl <- renderPlot({
		req(time_series())
		autoplot(stl(time_series(),s.window='periodic'))
	})

	#### MODEL ####
	## ETS ##

	ets_model <- reactive({
		req(time_series())
		return(ets(time_series()))
	})

	output$model_ets_res <- renderPlot({
		checkresiduals(ets_model())
	})

	## ARIMA ##

	arima_model <- reactive({
		req(time_series())
		return(auto.arima(time_series()))
	})

	output$model_arima_res <- renderPlot({
		checkresiduals(arima_model())
	})

	#### FORECAST ####
	## ETS

	ets_forecast <- reactive({
		return(forecast(ets_model() , h=3))
	})

	output$fore_ets <- renderPlot({
		autoplot(ets_forecast(), xlab="Period", ylab=data_label()) + autolayer(fitted(ets_forecast()),color="blue")
	})

	## ARIMA

	arima_forecast <- reactive({
		return(forecast(arima_model() , h=3))
	})

	output$fore_arima <- renderPlot({
		autoplot(arima_forecast(), xlab="Period", ylab=data_label()) + autolayer(fitted(arima_forecast()),color="blue")
	})

})
