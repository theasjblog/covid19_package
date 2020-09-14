#' @slot JHUData_raw data.frame
#' @slot JHUData_smooth data.frame
#' @slot JHUData_diffRaw data.frame
#' @slot JHUData_diffSmooth data.frame
#' @slot populationDf data.frame
#' @slot keys character
setClass('covidData',
         representation(JHUData_raw = 'data.frame',
                        JHUData_smooth = 'data.frame',
                        JHUData_diffRaw = 'data.frame',
                        JHUData_diffSmooth = 'data.frame',
                        populationDf = 'data.frame',
                        keys = 'character'),
         prototype(JHUData_raw = NULL,
                   JHUData_smooth = NULL,
                   JHUData_diffRaw = NULL,
                   JHUData_diffSmooth = NULL,
                   populationDf = NULL,
                   keys = NULL)
)

#' @slot world sf
#' @slot filterByCountry character
#' @slot th numeric
setClass('worldMap',
         representation(world = 'sf',
                        filterByCountry = 'character',
                        th = 'numeric'),
         prototype(world = NULL,
                   filterByCountry = NULL,
                   th = NULL)
)
