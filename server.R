# Copyright 2014 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.



library(shiny)
library(ggplot2)
library(data.table)
library(reshape2)

path.prefix <- "."

load(paste0(path.prefix, "/alldata.Rdata"))
load(paste0(path.prefix, "/CES_dictionary.Rdata"))
source(paste0(path.prefix, "/functions.R"))

shinyServer(function(input, output) {

  ##############################################################################
  # Map tab
  ############################################################################## 

  # reactive data extraction
  ExtractCesDataMap <- reactive({
    # dataset 1 (under)
    #   extract
    col.var <- ConvertNameToVar(input$var1, input$type1)
    gran1 <- tolower(input$granularity1)
    if (input$var1 == "Users" & gran1 != "census") {
      tmp <- all.data[["registration.info"]][[paste0("registration.", gran1)]]
      tmp <- tmp[is.element(vehicle.type, input$user.restrictions) &
                   is.element(vehicle.type, input$user.restrictions),
                 sum(users), by = eval(parse(text = gran1))]
      setnames(tmp, c(gran1, "users"))
      if (nrow(tmp) == 0) {
        tmp <- NULL
      } else {
        tmp[, users.percentile := CreatePercentileColumn(users)]
      }
    } else if (is.element(col.var, names(all.data[["ces.info"]][[1]]))) {
      tmp <- all.data[["ces.info"]][[paste0("ces.", gran1)]]
      expr <- paste0("list(", gran1, ", ", col.var, ")")
      tmp <- tmp[, eval(parse(text = expr))]
      if (input$exclude1) {
        tmp <- tmp[tmp[[col.var]] != 0]
      }
    } else {
      tmp <- NULL
    }
    data1 <- tmp
    if (! is.null(data1)) {
      #   add lat/lon
      name.tmp <- paste0("latlon.", gran1)
      latlon.tmp <- all.data[["latlon.info"]][[name.tmp]]
      setkeyv(latlon.tmp, gran1)
      data1 <- latlon.tmp[data1]
      #   filter
      if (input$region.name != "california" && ! is.null(data1)) {
        data1 <- FilterBasedOnLatLon(data1, input$region.name)
      }
    }

    # dataset 2 (over)
    #   extract
    col.var <- ConvertNameToVar(input$var2, input$type2)
    gran2 <- tolower(input$granularity2)
    if (input$var2 == "Users" & gran2 != "census") {
      tmp <- all.data[["registration.info"]][[paste0("registration.", gran2)]]
      tmp <- tmp[is.element(vehicle.type, input$user.restrictions) &
                   is.element(fuel.type, input$user.restrictions),
                 sum(users),
                 by = eval(parse(text = gran2))]
      setnames(tmp, c(gran2, "users"))
      if (nrow(tmp) == 0) {
        tmp <- NULL
      } else {
        tmp[, users.percentile := CreatePercentileColumn(users)]
      }
    } else if (is.element(col.var, names(all.data[["ces.info"]][[1]]))) {
      tmp <- all.data[["ces.info"]][[paste0("ces.", gran2)]]
      expr <- paste0("list(", gran2, ", ", col.var, ")")
      tmp <- tmp[, eval(parse(text = expr))]
      if (input$exclude2) {
        tmp <- tmp[tmp[[col.var]] != 0]
      }
    } else {
      tmp <- NULL
    }
    data2 <- tmp
    if (! is.null(data2)) {
      #   add lat/lon
      name.tmp <- paste0("latlon.", gran2)
      latlon.tmp <- all.data[["latlon.info"]][[name.tmp]]
      setkeyv(latlon.tmp, gran2)
      data2 <- latlon.tmp[data2]
      #   filter
      if (input$region.name != "california" && ! is.null(data2)) {
        data2 <- FilterBasedOnLatLon(data2, input$region.name)
      }
    }
    
    # return
    return(list("data1" = data1,
                "data2" = data2))
  })

  output$dictionary.v1.t1 <- renderText({
    HelpConverter(dictionary, input$var1, input$type1)
  })
  output$dictionary.v2.t1 <- renderText({
    HelpConverter(dictionary, input$var2, input$type2)
  })
  
  # generate a plot of the requested variable
  output$california.plot <- renderPlot({
    data.list <- ExtractCesDataMap()
    if (is.null(data.list[["data1"]]) && is.null(data.list[["data2"]])) {
      california.plot <-
        ggplot(data = data.table("x" = 0, "y" = 0), aes(x = x, y = y)) +
        geom_point() +
        annotate("text", x = 0, y = 0, label = "NO DATA", col = "red")
    } else {
      california.plot <- PlotCalifornia(data.list[["data1"]],
                                        input$var1, input$type1,
                                        input$granularity1,
                                        data.list[["data2"]],
                                        input$var2, input$type2,
                                        input$granularity2,
                                        region.name = input$region.name,
                                        ca.borders = all.data[["ca.borders"]],
                                        pop.info = all.data[["pop.info"]])
    }
    print(california.plot)
  }, width = 800, height = 700)

  ##############################################################################
  # Correlation tab
  ##############################################################################

  # reactive data extraction
  ExtractCesDataCorrelation <- reactive({
    col.var1 <- ConvertNameToVar(input$var1, type = "raw")
    col.var2 <- ConvertNameToVar(input$var2, type = "raw")
    gran <- tolower(input$granularity.corr)
    if ((input$var1 == "Users" || input$var2 == "Users") &&
        gran == "census") {
      return(NULL)
    } else if (input$var1 == "Users" && input$var2 == "Users") {
      tmp <- all.data[["registration.info"]][[paste0("registration.", gran)]]
      tmp <- tmp[is.element(vehicle.type, input$user.restrictions) &
                   is.element(fuel.type, input$user.restrictions),
                 sum(users),
                 by = eval(parse(text = gran))]
      setnames(tmp, c(gran, "users"))
      if (nrow(tmp) == 0) {
        return(NULL)
      }
    } else if (input$var1 == "Users" || input$var2 == "Users") {
      tmp1 <- all.data[["ces.info"]][[paste0("ces.", gran)]]
      tmp2 <- all.data[["registration.info"]][[paste0("registration.", gran)]]
      tmp2 <- tmp2[is.element(vehicle.type, input$user.restrictions) &
                     is.element(fuel.type, input$user.restrictions),
                   sum(users),
                   by = eval(parse(text = gran))]
      if (nrow(tmp2) == 0) {
        return(NULL)
      }
      setnames(tmp2, c(gran, "users"))
      setkeyv(tmp2, gran)
      tmp <- tmp2[tmp1]
    } else {
      tmp <- all.data[["ces.info"]][[paste0("ces.", gran)]]
    }
    # merge pop if not there already
    if (! is.null(tmp) && ! is.element("population", names(tmp))) {
      pop.tmp <- all.data[["pop.info"]][[paste0("pop.", gran)]]
      setkeyv(pop.tmp, gran)
      tmp <- pop.tmp[tmp]
    }
    # add lat/lon
    if (! is.null(tmp)) {
      latlon.tmp <- all.data[["latlon.info"]][[paste0("latlon.", gran)]]
      setkeyv(latlon.tmp, gran)
      tmp <- latlon.tmp[tmp]
    }
    # filter
    if (! is.null(tmp) && input$region.name != "california") {
      tmp <- FilterBasedOnLatLon(tmp, input$region.name)
    }
    # exclude if requested
    if (input$exclude.corr) {
      tmp <- tmp[tmp[[col.var1]] != 0]
      tmp <- tmp[tmp[[col.var2]] != 0]
    }
    # NULL if no rows
    if (nrow(tmp) == 0) {
      tmp <- NULL
    }

    # return
    return(tmp)
  })

  output$dictionary.v1.t2 <- renderText({
    HelpConverter(dictionary, input$var1, type = "raw")
  })
  output$dictionary.v2.t2 <- renderText({
    HelpConverter(dictionary, input$var2, type = "raw")
  })

  # generate a plot of the requested variable
  output$correlation.plot <- renderPlot({
    data <- ExtractCesDataCorrelation()
    if (is.null(data)) {
      correlation.plot <-
        ggplot(data = data.table("x" = 0, "y" = 0), aes(x = x, y = y)) +
        geom_point() +
        annotate("text", x = 0, y = 0, label = "NO DATA", col = "orange")
    } else {
      correlation.plot <- PlotCorrelation(data,
                                          input$var1, type1 = "raw",
                                          input$var2, type2 = "raw")
    }
    print(correlation.plot)
  }, width = 800, height = 700)
  
  ##############################################################################
  # Top correlation tab
  ##############################################################################
  
  ExtractCesDataAllCorrelations <- reactive({
    gran.var <- tolower(input$granularity.topcorr)
    split.var <- tolower(input$splitby.topcorr)
    # exception
    if (gran.var == "census" &&
        (input$filter1 == "Users" || input$filter2 == "Users")) {
      return(NULL)
    }
    # merge ces with registration if gran.var != census
    tmp1 <- all.data[["ces.info"]][[paste0("ces.", gran.var)]]
    if (gran.var != "census") {
      tmp2 <- all.data[["registration.info"]][[paste0("registration.", gran.var)]]
      tmp2 <- tmp2[is.element(vehicle.type, input$user.restrictions) &
                     is.element(fuel.type, input$user.restrictions),
                   sum(users),
                   by = eval(parse(text = gran.var))]
      if (nrow(tmp2) == 0) {
        return(NULL)
      }
      setnames(tmp2, c(gran.var, "users"))
      setkeyv(tmp2, gran.var)
      tmp <- tmp2[tmp1]
    } else {
      tmp <- tmp1
    }
    # merge lat/long
    latlon.tmp <- all.data[["latlon.info"]][[paste0("latlon.", gran.var)]]
    setkeyv(latlon.tmp, gran.var)
    tmp <- latlon.tmp[tmp]
    # filter based on area of interest
    if (input$region.name != "california") {
      tmp <- FilterBasedOnLatLon(tmp, input$region.name)
    }
    # convert to dataframe for pairwise calculation
    tmp <- as.data.frame(tmp)
    # exclude if requested
    if (input$exclude.topcorr) {      
      tmp[tmp == 0] <- NA
    }

    # compute correlations
    all.vars <-
      c("population", "users", "ces.score", "ozone", "pm2.5", "diesel.pm",
        "drinking.water", "pesticides", "toxic.release", "traffic",
        "cleanup.sites", "groundwater.threats", "hazardous.waste",
        "impaired.water.bodies", "solid.waste", "pollution.burden", "age",
        "asthma", "low.birth.weight", "education", "linguistic.isolation",
        "poverty", "unemployment", "population.characteristics")
    if (split.var == "none") {
      tmp <- tmp[, intersect(names(tmp), all.vars)]
      tmp <- FormatCorrelationMatrix(tmp, input$min.sample.size)
    } else if (gran.var == split.var ||
               (gran.var == "county" && split.var == "zipcode")) {
      return(NULL)
    } else {
      # merge gran.var --> split.var
      mapping.dt <-
        all.data[["mappings"]][[paste0(gran.var, "2", split.var, ".map")]]
      setkeyv(mapping.dt, gran.var)
      tmp <- mapping.dt[tmp]
      expr.cols <-
        paste0("list(",
               paste0(intersect(names(tmp), all.vars), collapse = ", "), ")")
      tmp.list <- lapply(unique(tmp[[split.var]]), function(level) {
          tmp2 <- tmp[tmp[[split.var]] == level, eval(parse(text = expr.cols))]
          output <- FormatCorrelationMatrix(tmp2, input$min.sample.size)
          expr.addsplit <- paste0("split := '", level, "'")
          output[, eval(parse(text = expr.addsplit))]
          return(output)
        })
      tmp <- do.call("rbind", tmp.list)
      tmp <- tmp[order(tmp$Correlation, decreasing = TRUE)]
      tmp[, split := FirstWordCap(split)]
      setcolorder(tmp, c("Variable1","Variable2", "split", "Correlation",
                         "Sample_Size"))
      setnames(tmp, "split", FirstWordCap(input$splitby.topcorr))
    }
    # filter  
    if (input$filter1 != "None" && input$filter2 != "None") {
      tmp <- tmp[Variable1 == input$filter1 | Variable2 == input$filter1]
      tmp[Variable2 == input$filter1, ':='(Variable1 = input$filter1,
                                           Variable2 = Variable1)]
      tmp <- tmp[as.logical(Variable2 == input$filter2)]
    } else if (input$filter1 != "None" && input$filter2 == "None") {
      tmp <- tmp[Variable1 == input$filter1 | Variable2 == input$filter1]
      tmp[Variable2 == input$filter1, ':='(Variable1 = input$filter1,
                                           Variable2 = Variable1)]
    } else if (input$filter1 == "None" && input$filter2 != "None") {
      tmp <- tmp[Variable1 == input$filter2 | Variable2 == input$filter2]
      tmp[Variable2 == input$filter2, ':='(Variable1 = input$filter2,
                                           Variable2 = Variable1)]
    } else if (input$filter1 == "None" && input$filter2 == "None") {
      # do nothing
    }
    return(tmp)
  })

  output$top.cor <- renderTable({
    data <- ExtractCesDataAllCorrelations()
    if (nchar(input$num.obs) > 0 && ! is.null(data)) {
      head(data, n = as.numeric(input$num.obs))
    } else {
      data.table("Variable1" = NA, "Variable2" = NA, "Correlation" = NA)
    }
  })
  
})
