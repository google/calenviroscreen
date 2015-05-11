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


LoadAndMergeRegistrationData <- function(data.path) {
  files <- paste0(data.path,
                  c("US_Dec2014_Chng1_Class1through6.csv",
                    "US_Dec2014_Chng3_Class7and8.csv"))
  data.list <- lapply(files, function(file) {
      return(read.csv(file))
    })
  data <- do.call("rbind", data.list)
  data <- as.data.table(data)
  setnames(data, c("zipcode", "cbsa", "make", "model", "vin.year.model",
                   "fuel.type", "gvw.code", "retail", "fleet"))
  # diesel type
  data[, fuel.type := c("ffv", "diesel")[1 + (fuel.type == "Diesel")]]
  # combine fleet/retail into long format
  data <- melt(data, id.vars = c("zipcode", "cbsa", "make", "model",
                                 "vin.year.model", "fuel.type", "gvw.code"),
                     measure.vars = c("retail", "fleet"))
  setnames(data, c("variable", "value"), c("vehicle.type", "users"))
  # return
  return(data)
}


CreateAllGranularityData <- function(data.path) {
  # borders
  ca.state.borders <- map_data("state", "california")
  ca.state.borders <- as.data.table(ca.state.borders)
  setnames(ca.state.borders, c("region", "subregion"), c("state", "county"))
  ca.county.borders <- map_data("county", "california")
  ca.county.borders <- as.data.table(ca.county.borders)
  setnames(ca.county.borders, c("region", "subregion"), c("state", "county"))
  ca.borders <- list("ca.state.borders" = ca.state.borders,
                     "ca.county.borders" = ca.county.borders)
  # CES
  ces.raw <- read.csv(paste0(data.path, "CES20UpdateOct2014_data.csv"))
  ces.raw <- CleanUpCsv(ces.raw, remove.vars = FALSE)
  save.for.mapping <- copy(ces.raw[, list(census, zipcode, county)])
  latlon.census <- ces.raw[, list(census, lat, long)]
  ces.raw[, c("total.population", "city", "lat", "long", "interactive.map",
              "hyperlink") := NULL]
  ces.census <- copy(ces.raw)[, c("county", "zipcode") := NULL]
  ces.zipcode <- copy(ces.raw)[, c("census", "county") := NULL]
  saved.names <- names(ces.zipcode)
  ces.zipcode <- AggregateCESdata(ces.zipcode, granularity = "zipcode")
  setnames(ces.zipcode, saved.names)
  ces.county <- copy(ces.raw)[, c("census", "zipcode") := NULL]
  saved.names <- names(ces.county)
  ces.county <- AggregateCESdata(ces.county, granularity = "county")
  setnames(ces.county, saved.names)
  # zipcode -> county mapping
  zipcode2county.map <- unique(save.for.mapping[, list(zipcode, county)])
  zipcode.county.override <-
    data.table("zipcode" = c(90631, 91301, 91307, 91361, 91362, 92274, 93514,
                             93555, 93622, 93631, 93646, 94303, 94707, 94708,
                             94952, 95033, 95076, 95620, 95837, 96022, 96056,
                             96161),
               "county"  = c("orange", "los angeles", "los angeles", "ventura",
                               "ventura", "riverside", "mono",
                             "kern", "fresno", "tulare", "fresno", "santa clara",
                                "alameda", "contra costa",
                             "sonoma", "santa cruz", "santa cruz", "solano",
                                "sacramento", "tehama", "lassen",
                             "placer"))
  for (i in 1 : nrow(zipcode.county.override)) {
    zipcode2county.map[zipcode == zipcode.county.override[i, zipcode],
                       county := zipcode.county.override[i, county]]
  }
  zipcode2county.map <- unique(zipcode2county.map)
  census2zipcode.map <- unique(save.for.mapping[, list(census, zipcode)])
  census2county.map <- unique(save.for.mapping[, list(census, county)])
  setkey(zipcode2county.map, zipcode)
  mappings <- list("zipcode2county.map" = zipcode2county.map,
                   "census2zipcode.map" = census2zipcode.map,
                   "census2county.map" = census2county.map)
  # registration
  registration.raw <- LoadAndMergeRegistrationData(data.path = data.path)
  registration.zipcode <-
    registration.raw[, sum(users), by = "zipcode,vehicle.type,fuel.type"]
  setnames(registration.zipcode, "V1", "users")
  registration.county <- zipcode2county.map[registration.zipcode]
  registration.county <-
    registration.county[, sum(users), by = "county,vehicle.type,fuel.type"]
  setnames(registration.county, "V1", "users")
  registration.county <- registration.county[! is.na(county)]
  registration.info <- list("registration.census" = NULL,
                            "registration.zipcode" = registration.zipcode,
                            "registration.county" = registration.county)
  # population
  pop.census <-
    read.csv(paste0(data.path, "ACS_13_5YR_DP05_with_ann_censustract.csv"),
             skip = 1)
  pop.census <- as.data.table(pop.census[, c(2, 4)])
  setnames(pop.census, c("census", "population"))
  pop.census <- pop.census[! is.na(census)]
  pop.census[, population.percentile := CreatePercentileColumn(population)]
  pop.zipcode <-
    read.csv(paste0(data.path, "ACS_13_5YR_DP05_with_ann_zipcode.csv"),
             skip = 1)
  pop.zipcode <- as.data.table(pop.zipcode[, c(2, 4)])
  setnames(pop.zipcode, c("zipcode", "population"))
  pop.zipcode <- pop.zipcode[! is.na(zipcode)]
  pop.zipcode[, population.percentile := CreatePercentileColumn(population)]
  pop.county <- zipcode2county.map[pop.zipcode]
  pop.county <- pop.county[, sum(population, na.rm = TRUE), by = county]
  setnames(pop.county, "V1", "population")
  pop.county[, county := tolower(county)]
  pop.county[, population.percentile := CreatePercentileColumn(population)]
  pop.info <- list("pop.census" = pop.census,
                   "pop.zipcode" = pop.zipcode,
                   "pop.county" = pop.county)
  # ces + pop info
  setkey(pop.census, census)
  full.census <- pop.census[ces.census]
  setkey(pop.zipcode, zipcode)
  full.zipcode <- pop.zipcode[ces.zipcode]
  setkey(pop.county, county)
  full.county <- pop.county[ces.county]
  ces.info <- list("ces.census" = full.census,
                   "ces.zipcode" = full.zipcode,
                   "ces.county" = full.county)
  # lat lon info
  latlon.zipcode <-
    GetZipcodeLatLonFromCensustractLatLon(census2zipcode.map, latlon.census)
  latlon.county <-
    GetCountyLatLonFromZipcodeLatLon(zipcode2county.map, latlon.zipcode)
  latlon.info <- list("latlon.census" = latlon.census,
                      "latlon.zipcode" = latlon.zipcode,
                      "latlon.county" = latlon.county)
  
  # return
  return(list("ca.borders" = ca.borders,
              "mappings" = mappings,
              "latlon.info" = latlon.info,
              "ces.info" = ces.info,
              "pop.info" = pop.info,
              "registration.info" = registration.info))
}


CleanUpCsv <- function(csv.data, remove.vars = FALSE) {
  # rename
  names(csv.data) <-
    c("census", "total.population", "county", "zipcode", "city", "long",
      "lat", "interactive.map", "ces.score", "ces.score.percentile",
      "hyperlink",
      "ozone", "ozone.percentile", "pm2.5", "pm2.5.percentile", "diesel.pm",
      "diesel.pm.percentile", "drinking.water", "drinking.water.percentile",
      "pesticides", "pesticides.percentile", "toxic.release",
      "toxic.release.percentile", "traffic", "traffic.percentile",
      "cleanup.sites", "cleanup.sites.percentile", "groundwater.threats",
      "groundwater.threats.percentile", "hazardous.waste",
      "hazardous.waste.percentile", "impaired.water.bodies",
      "impaired.water.bodies.percentile", "solid.waste",
      "solid.waste.percentile", "pollution.burden", "pollution.burden.score",
      "pollution.burden.percentile", "age", "age.percentile", "asthma",
      "asthma.percentile", "low.birth.weight", "low.birth.weight.percentile",
      "education", "education.percentile", "linguistic.isolation",
      "linguistic.isolation.percentile", "poverty", "poverty.percentile",
      "unemployment", "unemployment.percentile", "population.characteristics",
      "population.characteristics.score",
      "population.characteristics.percentile")
  # data.table
  csv.data <- as.data.table(csv.data)
  # special ces.score.percentile case
  ecd.func <- ecdf(csv.data$ces.score)
  csv.data[, ces.score.percentile := ecd.func(ces.score)]
  # special impaired water bodies handling
  csv.data[, impaired.water.bodies := as.numeric(impaired.water.bodies)]
  # special county handling
  csv.data[, county := SimplifyWhiteSpace(tolower(county))]
  # remove
  if (remove.vars) {
    csv.data[, c("census", "county", "zip", "city", "interactive.map",
                 "hyperlink") := NULL]
  }
  # return
  return(csv.data)
}


GetZipcodeLatLonFromCensustractLatLon <- function(census2zipcode.map,
                                                  latlon.census) {
  setkey(census2zipcode.map, "census")
  setkey(latlon.census, "census")
  merged <- latlon.census[census2zipcode.map]
  latlon.zipcode <- merged[, list(mean(lat), mean(long)), by = "zipcode"]
  setnames(latlon.zipcode, c("zipcode", "lat", "long"))
  return(latlon.zipcode)
}


GetCountyLatLonFromZipcodeLatLon <- function(zipcode2county.map,
                                             latlon.zipcode) {
  setkey(zipcode2county.map, "zipcode")
  setkey(latlon.zipcode, "zipcode")
  merged <- latlon.zipcode[zipcode2county.map]
  latlon.county <- merged[, list(mean(lat), mean(long)), by = "county"]
  setnames(latlon.county, c("county", "lat", "long"))
  return(latlon.county)
}


AggregateCESdata <- function(data, granularity, method = "median") {
  # aggregate
  cols <- setdiff(names(data), c(granularity))
  new.cols <- paste0(method, "(", cols, ", na.rm = TRUE)", collapse = ", ")
  expr.j <- paste0(" list(", new.cols, ")")
  expr.by <- paste0("by = ", granularity)
  data.agg <- data[, eval(parse(text = expr.j)), eval(parse(text = expr.by))]
  # recompute percentile
  percent.cols <- names(data)[grep("[.]percentile$", names(data))]
  for (colname in percent.cols) {
    short.name <- gsub("[.]percent$", "", colname)
    expr <- paste0(colname, " := CreatePercentileColumn(", short.name, ")")
    data[, eval(parse(text = expr))]
  }
  # return
  return(data.agg)
}


CreatePercentileColumn <- function(vector) {
  missing.index <- (is.na(vector) | vector == 0)
  percentiles <- rep(NA, length(vector))
  percentiles[! missing.index] <-
    ecdf(vector[! missing.index])(vector[! missing.index])
  return(percentiles)
}


SimplifyWhiteSpace <- function (string) {
  string <- sub("^\\s+", "", string)
  string <- sub("\\s+$", "", string)
  string <- gsub("[ ]+", " ", string, perl = TRUE)
  return(string)
}


CreateDictionary <- function(data.path) {
  dictionary <- read.csv(paste0(data.path, "CES20UpdateOct2014_dict.csv"),
                         skip = 7, header = TRUE, stringsAsFactors = FALSE)
  dictionary <- dictionary[, 1 : 2]
  names(dictionary) <- c("variable", "description")
  dictionary <- dictionary[dictionary$variable != "", ]
  add.to.dictionary <-
    data.frame("variable" = c("Users", "None"),
               "description" = c("Number of registered FFV/Diesel vehicles",
                                 "No variable selected"),
               stringsAsFactors = FALSE)
  dictionary <- rbind(dictionary, add.to.dictionary)
  dictionary[dictionary$variable == "CES 2.0 Score", "variable"] <- "CES Score"
  dictionary[dictionary$variable == "CES 2.0 Percentile Range", "variable"] <-
    "CES Score Pctl"
  dictionary[dictionary$variable == "Total Population", "variable"] <-
    "Population"
  dictionary$variable <- gsub("^Tox[.]", "Toxic", dictionary$variable)
  dictionary$variable <- gsub("^Haz[.]", "Hazardous", dictionary$variable)
  dictionary$variable <- gsub("^Imp[.]", "Impaired", dictionary$variable)
  dictionary$variable <-
    gsub("^Pop[.] Char[.]", "Population Characteristics", dictionary$variable)
  dictionary$variable <-
    gsub("^Population Characteristics $", "Population Characteristics",
         dictionary$variable)
  return(dictionary)
}

