library(rcap)
context("Testing that the geoms run and return the correct objects")

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline runs correctly", {
  earthquake_data <- load_data() %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(earthquake_data, ggplot2::aes(x = date, y = COUNTRY,
                                            color = as.numeric(TOTAL_DEATHS),
                                            size = as.numeric(EQ_PRIMARY),
                                            label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE")
  
  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline_label runs correctly", {
  earthquake_data <- load_data() %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  plt <-   ggplot2::ggplot(earthquake_data, ggplot2::aes(x = date, y = COUNTRY,
                                            color = as.numeric(TOTAL_DEATHS),
                                            size = as.numeric(EQ_PRIMARY),
                                            label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
    geom_timeline_label(data=earthquake_data)
  
  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
