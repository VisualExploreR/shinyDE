#' Launch VisualToolkit in the default browser
#'
#' @details See \url{http://www.google.com} for documentation and tutorials
#'
#' @examples
#' if (interactive()) {
#'   visualToolkit()
#' }
#' @export
visualToolkit <- function() {
  
  runApp(system.file(app[1], package='visualToolkit'))
}

# #' Create a launcher for Windows (.bat)
# #'
# #' @details On Windows a file named `radiant.bat` will be put on the desktop. Double-click the file to launch the specified Radiant app
# #'
# #' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
# #'
# #' @examples
# #' if (interactive()) {
# #'   if(Sys.info()["sysname"] != "Windows") {
# #'     win_launcher()
# #'     fn <- paste0(Sys.getenv("USERPROFILE") ,"/Desktop/radiant.bat")
# #'     if(!file.exists(fn))
# #'        stop("Windows launcher not created")
# #'     else
# #'       unlink(fn)
# #'   }
# #' }
# #'
# #' @export
# win_launcher <- function(app = c("marketing", "quant", "base")) {
#   
#   if(Sys.info()["sysname"] != "Windows")
#     return("This function is for Windows only. For Mac use the mac_launcher() function")
#   
#   local_dir <- Sys.getenv("R_LIBS_USER")
#   if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
#   
#   filepath <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/')
#   launch_string <- paste0(Sys.which('R'), " -e \"if(!require(radiant)) { options(repos = c(XRAN = 'http://vnijs.github.io/radiant_miniCRAN/')); install.packages('radiant'); }; require(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
#   cat(launch_string,file=paste0(filepath,"radiant.bat"),sep="\n")
# }
# 
# #' Create a launcher for Mac (.command)
# #'
# #' @details On Mac a file named `radiant.command` will be put on the desktop. Double-click the file to launch the specified Radiant app
# #'
# #' @param app App to run when the desktop icon is double-clicked ("marketing", "quant", or "base"). Default is "marketing"
# #'
# #' @examples
# #' if (interactive()) {
# #'   if(Sys.info()["sysname"] != "Darwin") {
# #'     mac_launcher()
# #'     fn <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
# #'     if(!file.exists(fn))
# #'        stop("Mac launcher not created")
# #'     else
# #'       unlink(fn)
# #'   }
# #' }
# #'
# #' @export
# mac_launcher <- function(app = c("marketing", "quant", "base")) {
#   
#   if(Sys.info()["sysname"] != "Darwin")
#     return("This function is for Mac only. For windows use the win_launcher() function")
#   
#   local_dir <- Sys.getenv("R_LIBS_USER")
#   if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
#   
#   filename <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
#   launch_string <- paste0("#!/usr/bin/env Rscript\n if(!require(radiant)) {\n options(repos = c(XRAN = 'http://vnijs.github.io/radiant_miniCRAN/'))\n install.packages('radiant')\n }\n\nrequire(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
#   cat(launch_string,file=filename,sep="\n")
#   Sys.chmod(filename, mode = "0755")
# }


