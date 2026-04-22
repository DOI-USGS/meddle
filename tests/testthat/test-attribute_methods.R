context("testing attribute methods")

test_that("attribute table creation", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  expect_is(df, 'data.frame')
})

test_that("attr_names for different classes", {
  df <- data.frame('dog'='harry','cat'='betty', stringsAsFactors=FALSE)
  expect_equal(attr_names(df), c('dog','cat'))

  expect_is(attr_names(system.file(package='meddle','extdata','example_shapefile')), 'character')
  sf <- read_data(system.file(package='meddle','extdata','example_shapefile'))
  expect_equal(attr_names(system.file(package='meddle','extdata','example_shapefile')), attr_names(sf))
})

context("creating attribute skeleton doesn't override")

test_that("can write a manual attribute table", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  df[1,]$`attr-def` <- 'crazy uncle jim'
  df[1,]$`attr-defs` <- '5824 37th Street Michigan City MI'
  df <- df[1:2, ] # drop z
  attr.file <- tempfile(fileext = '.csv')
  meddle:::write_attr_file(df, attr.file)
  df.read <- meddle:::read_attr_file(attr.file)
  expect_equal(df[1,]$`attr-def`, df.read[1,]$`attr-def`)
})

test_that("can write a manual attribute table and not have changes overwritten", {
  df <- meddle:::create_attr_table(c('x','y','x'))
  df[1,]$`attr-def` <- 'crazy uncle jim'
  df[1,]$`attr-defs` <- '5824 37th Street Michigan City MI'
  df <- df[1:2, ] # drop z
  attr.file <- tempfile(fileext = '.csv')
  meddle:::write_attr_file(df, attr.file)
  attribute_skeleton(system.file(package='meddle','extdata','example_shapefile'), attr.file)
  df.read <- meddle:::read_attr_file(attr.file)
  expect_equal(df[1,]$`attr-def`, df.read[1,]$`attr-def`)
  expect_equal(df.read$`attr-label`, c('x','y','z'))
})

context("long/tidy table support")

test_that("create_attr_table_long produces correct rdom/edom rows", {
  f <- system.file(package='meddle', 'extdata', 'example_tidy_forecast.csv')
  data <- read.csv(f, stringsAsFactors = FALSE)
  tbl <- meddle:::create_attr_table_long(data, 'variable')

  rdom.rows <- tbl[tbl$`domain-type` == 'rdom', ]
  edom.rows <- tbl[tbl$`domain-type` == 'edom', ]

  # all non-key columns get one rdom row each
  expect_equal(nrow(rdom.rows), ncol(data) - 1)
  # key column gets one edom row per unique value
  expect_equal(nrow(edom.rows), length(unique(data$variable)))
  expect_equal(sort(edom.rows$edomv), sort(unique(data$variable)))
})

test_that("merge_attr_table_long preserves user-filled edomvd/edomvds", {
  f <- system.file(package='meddle', 'extdata', 'example_tidy_forecast.csv')
  data <- read.csv(f, stringsAsFactors = FALSE)
  tbl <- meddle:::create_attr_table_long(data, 'variable')

  # simulate user filling in a definition
  filled <- tbl
  edom.i <- which(filled$`domain-type` == 'edom')[1]
  filled$edomvd[edom.i] <- 'Chlorophyll concentration'
  filled$edomvds[edom.i] <- 'User source'

  merged <- meddle:::merge_attr_table_long(tbl, filled)
  expect_equal(merged$edomvd[edom.i], 'Chlorophyll concentration')
  expect_equal(merged$edomvds[edom.i], 'User source')
})

test_that("as.attr_list groups edom rows under a single attr entry", {
  f <- system.file(package='meddle', 'extdata', 'example_tidy_forecast.csv')
  data <- read.csv(f, stringsAsFactors = FALSE)
  tbl <- meddle:::create_attr_table_long(data, 'variable')

  al <- as.attr_list(tbl)
  attrs <- al$attributes

  # number of entries == number of columns (non-key cols + 1 for key col)
  expect_equal(length(attrs), ncol(data))

  # find the variable entry
  var.entry <- Filter(function(a) a$`attr-label` == 'variable', attrs)
  expect_equal(length(var.entry), 1)
  expect_true(!is.null(var.entry[[1]]$edom))
  expect_equal(length(var.entry[[1]]$edom), length(unique(data$variable)))
})

test_that("attribute_skeleton with key.col writes valid attr file with edom rows", {
  f <- system.file(package='meddle', 'extdata', 'example_tidy_forecast.csv')
  attr.file <- tempfile(fileext = '.csv')
  attribute_skeleton(f, attr.file, key.col = 'variable')

  tbl <- meddle:::read_attr_file(attr.file)
  expect_true('domain-type' %in% names(tbl))
  expect_true('edom' %in% tbl$`domain-type`)
  expect_true('rdom' %in% tbl$`domain-type`)
})

test_that("attribute_skeleton with key.col does not overwrite filled values", {
  f <- system.file(package='meddle', 'extdata', 'example_tidy_forecast.csv')
  attr.file <- tempfile(fileext = '.csv')
  attribute_skeleton(f, attr.file, key.col = 'variable')

  tbl <- meddle:::read_attr_file(attr.file)
  edom.i <- which(tbl$`domain-type` == 'edom')[1]
  tbl$edomvd[edom.i] <- 'My definition'
  meddle:::write_attr_file(tbl, attr.file)

  # re-run skeleton — should not overwrite
  attribute_skeleton(f, attr.file, key.col = 'variable')
  tbl2 <- meddle:::read_attr_file(attr.file)
  expect_equal(tbl2$edomvd[edom.i], 'My definition')
})

test_that("backward compat: old attr file without domain-type produces flat list", {
  df <- meddle:::create_attr_table(c('a','b','c'))
  al <- as.attr_list(df)
  expect_equal(length(al$attributes), 3)
  expect_true(is.list(al$attributes[[1]]))
  # should not have rdom sub-list — flat structure
  expect_true(is.null(al$attributes[[1]]$rdom))
})

