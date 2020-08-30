#' @slot JHUData_raw data.frame of settings
#' @slot JHUData_smooth data.frame
#' @slot JHUData_diffSmooth data.frame

setClass('covidData',
         representation(JHUData_raw = 'data.frame',
                        JHUData_smooth = 'data.frame',
                        JHUData_diffSmooth = 'data.frame'),
         prototype(JHUData_raw = NULL,
                   JHUData_smooth = NULL,
                   JHUData_diffSmooth = NULL)
)
