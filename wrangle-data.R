library(tidyverse)
library(caret)
library(R.utils)
gunzip("data/covtype.data.gz", overwrite = TRUE, remove = FALSE)
covtype <- read_csv(
  "data/covtype.data",
  col_names = c(
    "elevation", # Elevation in meters
    "aspect",    # Aspect in degrees azimuth
    "slope",     # Slope in degrees
    "x_hydro",   # Horz Dist to nearest surface water features
    "y_hydro",   # Vert Dist to nearest surface water features
    "x_road",    # Horz Dist to nearest roadway
    "hs_9am",    # Hillshade index at 9am, summer solstice
    "hs_12pm",   # Hillshade index at noon, summer soltice 
    "hs_3pm",    # Hillshade index at 3pm, summer solstice
    "x_fire",    # Horz Dist to nearest wildfire ignition points
    "wa1",       # Wilderness area designation (4 binary columns. 0 (absence) or 1 (presence))
    "wa2",
    "wa3",
    "wa4",
    "st1",       # Soil Type designation (40 binary columns. 0 (absence) or 1 (presence))
    "st2",
    "st3",
    "st4",
    "st5",
    "st6",
    "st7",
    "st8",
    "st9",
    "st10",
    "st11",
    "st12",
    "st13",
    "st14",
    "st15",
    "st16",
    "st17",
    "st18",
    "st19",
    "st20",
    "st21",
    "st22",
    "st23",
    "st24",
    "st25",
    "st26",
    "st27",
    "st28",
    "st29",
    "st30",
    "st31",
    "st32",
    "st33",
    "st34",
    "st35",
    "st36",
    "st37",
    "st38",
    "st39",
    "st40",
    "y"            # Forest Cover Type designation (integer 1 to 7. 1 - Spruce-fir, 2 - Lodgepole-pine, 3 - Ponderosa-pine, 4 - Cottonwood-willow, 5 - Aspen, 6 - Douglas-fir, 7 - Krummholz)
  ),
  col_types = cols(
    elevation = col_double(),           # Elevation in meters
    aspect = col_double(),              # Aspect in degrees azimuth
    slope = col_double(),               # Slope in degrees
    x_hydro = col_double(),             # Horz Dist to nearest surface water features
    y_hydro = col_double(),             # Vert Dist to nearest surface water features
    x_road = col_double(),              # Horz Dist to nearest roadway
    hs_9am = col_integer(),             # Hillshade index at 9am, summer solstice
    hs_12pm = col_integer(),            # Hillshade index at noon, summer soltice 
    hs_3pm = col_integer(),             # Hillshade index at 3pm, summer solstice
    x_fire = col_double(),              # Horz Dist to nearest wildfire ignition points
    wa1 = col_integer(),                # Wilderness area designation (4 binary columns. 0 (absence) or 1 (presence))
    wa2 = col_integer(),
    wa3 = col_integer(),
    wa4 = col_integer(),
    st1 = col_integer(),                # Soil Type designation (40 binary columns. 0 (absence) or 1 (presence))
    st2 = col_integer(),
    st3 = col_integer(),
    st4 = col_integer(),
    st5 = col_integer(),
    st6 = col_integer(),
    st7 = col_integer(),
    st8 = col_integer(),
    st9 = col_integer(),
    st10 = col_integer(),
    st11 = col_integer(),
    st12 = col_integer(),
    st13 = col_integer(),
    st14 = col_integer(),
    st15 = col_integer(),
    st16 = col_integer(),
    st17 = col_integer(),
    st18 = col_integer(),
    st19 = col_integer(),
    st20 = col_integer(),
    st21 = col_integer(),
    st22 = col_integer(),
    st23 = col_integer(),
    st24 = col_integer(),
    st25 = col_integer(),
    st26 = col_integer(),
    st27 = col_integer(),
    st28 = col_integer(),
    st29 = col_integer(),
    st30 = col_integer(),
    st31 = col_integer(),
    st32 = col_integer(),
    st33 = col_integer(),
    st34 = col_integer(),
    st35 = col_integer(),
    st36 = col_integer(),
    st37 = col_integer(),
    st38 = col_integer(),
    st39 = col_integer(),
    st40 = col_integer(),
    y = col_factor()                   # Forest Cover Type designation (integer 1 to 7. 1 - Spruce-fir, 2 - Lodgepole-pine, 3 - Ponderosa-pine, 4 - Cottonwood-willow, 5 - Aspen, 6 - Douglas-fir, 7 - Krummholz)
  )
)
covtype <- covtype %>%
  mutate(
    wa1 = factor(ifelse(wa1 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    wa2 = factor(ifelse(wa2 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    wa3 = factor(ifelse(wa3 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    wa4 = factor(ifelse(wa4 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st1 = factor(ifelse(st1 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st2 = factor(ifelse(st2 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st3 = factor(ifelse(st3 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st4 = factor(ifelse(st4 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st5 = factor(ifelse(st5 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st6 = factor(ifelse(st6 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st7 = factor(ifelse(st7 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st8 = factor(ifelse(st8 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st9 = factor(ifelse(st9 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st10 = factor(ifelse(st10 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st11 = factor(ifelse(st11 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st12 = factor(ifelse(st12 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st13 = factor(ifelse(st13 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st14 = factor(ifelse(st14 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st15 = factor(ifelse(st15 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st16 = factor(ifelse(st16 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st17 = factor(ifelse(st17 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st18 = factor(ifelse(st18 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st19 = factor(ifelse(st19 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st20 = factor(ifelse(st20 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st21 = factor(ifelse(st21 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st22 = factor(ifelse(st22 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st23 = factor(ifelse(st23 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st24 = factor(ifelse(st24 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st25 = factor(ifelse(st25 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st26 = factor(ifelse(st26 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st27 = factor(ifelse(st27 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st28 = factor(ifelse(st28 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st29 = factor(ifelse(st29 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st30 = factor(ifelse(st30 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st31 = factor(ifelse(st31 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st32 = factor(ifelse(st32 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st33 = factor(ifelse(st33 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st34 = factor(ifelse(st34 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st35 = factor(ifelse(st35 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st36 = factor(ifelse(st36 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st37 = factor(ifelse(st37 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st38 = factor(ifelse(st38 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st39 = factor(ifelse(st39 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    st40 = factor(ifelse(st40 == 0, "Absence", "Presence"), levels = c("Absence", "Presence")),
    y = factor(ifelse(y == 1, "Spruce", ifelse(y == 2, "Lodgepole", ifelse(y == 3, "Panderosa", ifelse(y == 4, "Cottonwood", ifelse(y == 5, "Aspen", ifelse(y == 6, "Douglas", "Krummholz")))))))
  )
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y = covtype$y, times = 1, p = 0.5, list = FALSE)
train <- covtype[-test_index,]
test <- covtype[test_index,]
dat <- list(train = train, test = test)
save(dat, file = "rda/dat.rda")
