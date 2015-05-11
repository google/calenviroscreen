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


PlotCalifornia <- function(data1, var1, type1, granularity1,
                           data2, var2, type2, granularity2,
                           region.name, ca.borders, pop.info) {
  # misc handling
  col.var1 <- ConvertNameToVar(var1, type1)
  display.name1 <- paste0(var1, ifelse(type1 == "raw", "", " (percentile)"))
  col.var2 <- ConvertNameToVar(var2, type2)
  display.name2 <- paste0(var2, ifelse(type2 == "raw", "", " (percentile)"))
  if (region.name == "california") {
    region.display <- "California"
  } else if (region.name == "bay.area") {
    region.display <- "the Bay Area"
  } else if (region.name == "la.sd") {
    region.display <- " L.A. / San Diego"
  }

  # plot underlay
  if (is.null(data1)) {
    data.plot <- RestrictBorders(ca.borders[["ca.state.borders"]], region.name)
    california.plot <-
      ggplot() +
      geom_polygon(data = data.plot,
                   aes(x = long, y = lat), fill = "gray60")
  } else {
    borders.tmp <- ca.borders[[paste0("ca.", granularity1, ".borders")]]
    setkeyv(borders.tmp, granularity1)
    data.plot <- RestrictBorders(borders.tmp[data1], region.name)
    california.plot <-
      ggplot() +
      geom_polygon(data = data.plot,
                   aes_string(x = "long", y = "lat", fill = col.var1,
                              group = "group")) +
      scale_fill_continuous(name = display.name1, low = "green", high = "red")
  }

  # plot overlay
  if (! is.null(data2)) {
    if (is.null(pop.info)) {
      california.plot <-
        california.plot +
        geom_point(data = data2,
                   aes_string(x = "long", y = "lat", col = col.var2)) +
        scale_colour_continuous(name = display.name2,
                                low = "white", high = "blue")
    } else {
      # merge pop first
      pop.tmp <- pop.info[[paste0("pop.", granularity2)]]
      setkeyv(pop.tmp, granularity2)
      data2.copy <- pop.tmp[data2]
      # dot size
      max.range <- switch(granularity2,
                          "census" = 6,
                          "zipcode" = 8,
                          "county" = 20)
      min.range <- switch(granularity2,
                          "census" = 1,
                          "zipcode" = 2,
                          "county" = 5)
      # then plot
      california.plot <-
        california.plot +
        geom_point(data = data2.copy,
                   aes_string(x = "long", y = "lat", col = col.var2,
                              size = "population")) +
        scale_colour_gradientn(name = display.name2,
                               colours = c("white", "blue")) +
        scale_size_continuous(name = "Population",
                              guide = guide_legend(reverse = TRUE),
                              range = c(min.range, max.range))
    }
  }

  # plot common
  if (is.null(data1)) {
    if (is.null(data2)) {
      title <- ""
    } else {
      title <- paste0(var2, " in ")
    }
  } else {
    if (is.null(data2)) {
      title <- paste0(var1, " in ")
    } else {
      title <- paste0(var1, "/", var2, " in ")
    }
  }

  california.plot <-
    california.plot +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle(paste0(title, region.display))

  # return
  return(california.plot)
}


ConvertNameToVar <- function(var, type) {
  field.var <- tolower(var)
  field.var <- gsub(" ", ".", field.var)
  col.var <- paste0(field.var, ifelse(type == "raw", "", ".percentile"))
  return(col.var)
}


FirstWordCap <- function(strings) {
  capitalized <- sapply(strings, function(string) {
      splits <- unlist(strsplit(string, " "))
      paste(toupper(substring(splits, 1, 1)), substring(splits, 2),
            sep = "", collapse = " ")
    })
  return(capitalized)
}


ConvertVarToName <- function(var) {
  name <- gsub("[.]percentile$", " (percentile)", var)
  name <- gsub("[.]", " ", name)
  name <- sapply(name, FirstWordCap)
  # exception
  name <- gsub("^Pm2 5", "PM2.5", name)
  return(name)
}


PlotCorrelation <- function(data, var1, type1, var2, type2, num.col = 6) {
  data.tmp <- copy(data)
  col.var1 <- ConvertNameToVar(var1, type1)
  col.var2 <- ConvertNameToVar(var2, type2)
  correl <-
    cor(data.tmp[[col.var1]], data.tmp[[col.var2]], use = "complete.obs")
  data.tmp[, pointcolor := sample(1 : num.col, nrow(data.tmp), TRUE)]
  correlation.plot <-
    ggplot(data = data.tmp, aes_string(x = col.var1, y = col.var2,
                                       size = "population")) +
    geom_point(aes(col = as.factor(pointcolor)), guide = "none") +
    scale_colour_manual(values = rainbow(num.col), guide = 'none') +
    xlab(paste0(var1, ifelse(type1 == "raw", "", " (percentile)"))) +
    ylab(paste0(var2, ifelse(type2 == "raw", "", " (percentile)"))) +
    ggtitle(paste0("Correaltion Plot (r = ", round(correl, 2), ", ",
                   nrow(data), " obs.)"))
  return(correlation.plot)
}


HelpConverter <- function(dictionary, var, type, ignore.percentile = TRUE) {
  if (ignore.percentile) {
    lookup <- var
  } else {
    lookup <- paste0(var, ifelse(type == "raw", "", " Percentile"))
  }
  row <- dictionary[dictionary$variable == lookup, ]
  return(paste0(row[1, 1], ": ", row[1, 2]))
}


LoadLatLonAreas <- function() {
  # california
  california.limits <-
    c("xmin" = -124.3, "xmax" = -114.5, "ymin" = 32.5, "ymax" = 41.83)
  # la.sd
  la.sd.limits <-
    c("xmin" = -119, "xmax" = -116.5, "ymin" = 32.5, "ymax" = 34.5)
  # bay area
  bay.area.limits <-
    c("xmin" = -122.75, "xmax" = -121.5, "ymin" = 37.3, "ymax" = 38)
  # return
  return(list("california" = california.limits,
              "bay.area" = bay.area.limits,
              "la.sd" = la.sd.limits))
}


FilterBasedOnLatLon <- function(tmp, region.name) {
  # get region limits
  latlon.areas <- LoadLatLonAreas()
  limits <- latlon.areas[[region.name]]
  # filter
  filter.data <- tmp[lat  >= limits["ymin"] &
                     lat  <= limits["ymax"] &
                     long >= limits["xmin"] &
                     long <= limits["xmax"]]
  # return
  return(filter.data)
}


RestrictBorders <- function(borders, region.name) {
  # get region limits
  latlon.areas <- LoadLatLonAreas()
  limits <- latlon.areas[[region.name]]
  # filter
  borders.filt <- copy(borders)
  borders[, lat  := pmax(limits["ymin"], lat)]
  borders[, lat  := pmin(limits["ymax"], lat)]
  borders[, long := pmax(limits["xmin"], long)]
  borders[, long := pmin(limits["xmax"], long)]
  borders <- unique(borders)
  # adjust weird
  borders[, long := long - 0.045]
  # return
  return(borders)
}


CreatePercentileColumn <- function(vector) {
  missing.index <- (is.na(vector) | vector == 0)
  percentiles <- rep(NA, length(vector))
  percentiles[! missing.index] <-
    ecdf(vector[! missing.index])(vector[! missing.index])
  return(percentiles)
}


FormatCorrelationMatrix <- function(dt, min.sample.size = 0) {
  # correlation
  tmp.cor <- as.data.frame(cor(dt, use = "pairwise.complete.obs"))
  tmp.cor$id <- rownames(tmp.cor)
  tmp.cor <- melt(tmp.cor, ids = id)
  sorted.names <- t(apply(tmp.cor, 1, function(row) {
      sort(c(row["id"], row["variable"]))
    }))
  tmp.cor <- cbind(tmp.cor, sorted.names)
  names(tmp.cor) <- c("id", "variable", "Correlation", "Variable1", "Variable2")
  tmp.cor <- unique(tmp.cor[, c("Variable1", "Variable2", "Correlation")])
  tmp.cor <- as.data.table(tmp.cor)
  tmp.cor <- tmp.cor[Variable1 != Variable2]
  tmp.cor <- tmp.cor[order(tmp.cor$Correlation, decreasing = TRUE)]
  # merge sample size
  tmp.ss <- CorrelationSampleSize(dt)
  setnames(tmp.ss, c("Variable1", "Variable2", "Sample_Size"))
  setkey(tmp.ss, Variable1, Variable2)
  tmp <- tmp.ss[tmp.cor]
  # return
  tmp[, ':='(Variable1 = ConvertVarToName(Variable1),
             Variable2 = ConvertVarToName(Variable2),
             Correlation = round(Correlation, 2))]
  tmp <- tmp[! is.na(Correlation) & Sample_Size >= min.sample.size]
  return(tmp)
}


CorrelationSampleSize <- function(dt) {
  vars <- sort(names(dt))
  ss.grid <- expand.grid("var1" = vars, "var2" = vars, stringsAsFactors = FALSE)
  ss.grid$size <- sapply(1 : nrow(ss.grid), function(i) {
      return(sum(! is.na(dt[[ss.grid[i, 1]]]) & ! is.na(dt[[ss.grid[i, 2]]])))
    })
  return(as.data.table(ss.grid))
}

