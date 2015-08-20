
#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @param app Choose the app to run. One of "base", "quant", "analytics", "marketing". "analytics" is the default
#'
#' @examples
#' if (interactive()) {
#'   radiant("base")
#'   radiant("quant")
#'   radiant("marketing")
#'   radiant("analytics")
#' }
#' @export
radiant <- function(app = c("analytics", "marketing", "quant", "base")) {
  if (!"package:radiant" %in% search())
    if (!require(radiant)) stop("Calling radiant start function but radiant is not installed.")
  runApp(system.file(app[1], package = "radiant"), launch.browser = TRUE)
}


#' Create a launcher and updater for Windows (.bat)
#'
#' @details On Windows a file named 'radiant.bat' and one named 'update_radiant.bat' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Windows") {
#'     win_launcher()
#'     fn <- paste0(Sys.getenv("USERPROFILE") ,"/Desktop/radiant.bat")
#'     if (!file.exists(fn))
#'       stop("Windows launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
win_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {
  
  if (!interactive()) stop("This function can only be used in an interactive R session")
  
  if (Sys.info()["sysname"] != "Windows")
    return(message("This function is for Windows only. For Mac use the mac_launcher() function"))
  
  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {
    
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
    
    pt <- normalizePath(paste0(Sys.getenv("USERPROFILE") ,"/Desktop/"), winslash='/')
    fn1 <- paste0(pt, "radiant.bat")
    launch_string <- paste0(Sys.which('R'), " -e \"if (!require(radiant)) { install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/') }; library(radiant); shiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\"")
    cat(launch_string, file=fn1, sep="\n")
    
    fn2 <- paste0(pt, "update_radiant.bat")
    launch_string <- paste0(Sys.which('R'), " -e \"install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')\"")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")
    
    if (file.exists(fn1) & file.exists(fn2))
      message("Done! Look for a file named radiant.bat on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.bat you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Mac (.command)
#'
#' @details On Mac a file named 'radiant.command' and one named 'update_radiant.command' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Darwin") {
#'     mac_launcher()
#'     fn <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
#'     if (!file.exists(fn))
#'       stop("Mac launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
mac_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {
  
  if (!interactive()) stop("This function can only be used in an interactive R session")
  
  if (Sys.info()["sysname"] != "Darwin")
    return(message("This function is for Mac only. For windows use the win_launcher() function"))
  
  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {
    
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
    
    fn1 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/radiant.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\nif (!require(radiant)) {\n  install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')\n}\n\nlibrary(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
    cat(launch_string,file=fn1,sep="\n")
    Sys.chmod(fn1, mode = "0755")
    
    fn2 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/update_radiant.command")
    launch_string <- paste0("#!/usr/bin/env Rscript\ninstall.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")
    
    if (file.exists(fn1) & file.exists(fn2))
      message("Done! Look for a file named radiant.command  on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.command you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")
    
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher and updater for Linux (.sh)
#'
#' @details On Linux a file named 'radiant.sh' and one named 'update_radiant.sh' will be put on the desktop. Double-click the file to launch the specified Radiant app or update Radiant to the latest version
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @examples
#' if (interactive()) {
#'   if (Sys.info()["sysname"] == "Linux") {
#'     lin_launcher()
#'     fn <- paste0("/home/",Sys.getenv("USER"),"/Desktop/radiant.sh")
#'     if (!file.exists(fn))
#'       stop("Linux launcher not created")
#'     else
#'       unlink(fn)
#'   }
#' }
#'
#' @export
lin_launcher <- function(app = c("analytics", "marketing", "quant", "base")) {
  
  if (!interactive()) stop("This function can only be used in an interactive R session")
  
  if (Sys.info()["sysname"] != "Linux")
    return(message("This function is for Linux only. For windows use the win_launcher() function and for mac use the mac_launcher() function"))
  
  answ <- readline("Do you want to create shortcuts for Radiant on your Desktop? (y/n) ")
  if (substr(answ, 1, 1) %in% c("y","Y")) {
    
    local_dir <- Sys.getenv("R_LIBS_USER")
    if (!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
    
    fn1 <- paste0("/home/",Sys.getenv("USER"),"/Desktop/radiant.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\nif (!require(radiant)) {\n  install.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')\n}\n\nlibrary(radiant)\nshiny::runApp(system.file(\'", app[1], "\', package='radiant'), port = 4444, launch.browser = TRUE)\n")
    cat(launch_string,file=fn1,sep="\n")
    Sys.chmod(fn1, mode = "0755")
    
    fn2 <- paste0("/Users/",Sys.getenv("USER"),"/Desktop/update_radiant.sh")
    launch_string <- paste0("#!/usr/bin/env Rscript\ninstall.packages('radiant', repos = 'http://vnijs.github.io/radiant_miniCRAN/')")
    cat(launch_string,file=fn2,sep="\n")
    Sys.chmod(fn2, mode = "0755")
    
    if (file.exists(fn1) & file.exists(fn2))
      message("Done! Look for a file named radiant.sh on your desktop. Double-click it to start Radiant in your default browser. There is also a file called update_radiant.sh you can double click to update the version of Radiant on your computer.\n")
    else
      message("Something went wrong. No shortcuts were created.")
    
  } else {
    message("No shortcuts were created.\n")
  }
}

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named radiant.bat/radiant.command/radiant.sh will be put on the desktop. Double-click the file to launch the specified Radiant app
#'
#' @seealso \code{\link{win_launcher}} to create a shortcut on Windows
#' @seealso \code{\link{mac_launcher}} to create a shortcut on Mac
#' @seealso \code{\link{lin_launcher}} to create a shortcut on Linux
#'
#' @param app App to run when the desktop icon is double-clicked ("analytics", "marketing", "quant", or "base"). Default is "analytics"
#'
#' @export
launcher <- function(app = c("analytics", "marketing", "quant", "base")) {
  
  if (Sys.info()["sysname"] == "Darwin")
    mac_launcher(app[1])
  else if (Sys.info()["sysname"] == "Windows")
    win_launcher(app[1])
  else if (Sys.info()["sysname"] == "Linux")
    lin_launcher(app[1])
  else
    return(message("This function is not available for your platform."))
}
