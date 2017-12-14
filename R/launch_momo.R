#' Alternative launcher for EuroMoMo code.
#'
#' It is an alternative launcher for the EuroMoMo code that accepts data in R format. It avoids the need to create
#' intermediate text/Stata files if data has been captured in R previously.
#' @usage launch_momo(MOMOfile, hfile)
#' @param MOMOfile mortality data \code{data.frame}. It must contain columns \code{DoD} (\code{Date} class, date of death), 
#' \code{DoR} (\code{Date} class, reporting date), and \code{age} (\code{numeric}); other columns can be added 
#' (see EuroMoMo documentation).
#' @param hfile bank holidays \code{data.frame}. It must contain columns \code{date} and \code{closed}.
#' The master start button for running all of the MoMo code. \code{\link{SetOpts}} must be run first.
#' @import stringr
#' @export launch_momo
#' @examples
#' SetOpts(
#'   DoA=as.Date("2013-12-31"),
#'   DoPR=as.Date("2008-1-1"),
#'   WStart=1,
#'   WEnd=52,
#'   country = "Denmark",
#'   source = "SSI",
#'   MFILE = "DoD_DoR.txt",
#'   HFILE = "holidays.txt",
#'   INPUTDIR = system.file("testdata",package="momo"),
#'   WDIR = tempdir(),
#'   back = 3,
#'   WWW = 290,
#'   Ysum = 2013,
#'   Wsum = 40,
#'   USEglm2 = TRUE,
#'   useAUTOMN = TRUE,
#'   datesISO = FALSE,
#'   plotGraphs = FALSE
#' )
#'
#' momo_file_name <- system.file("testdata", "DoD_DoR.txt", package = "momo")
#' 
#' MOMOfile <- read.csv(momo_file_name, sep = ";", stringsAsFactors = FALSE)
#' MOMOfile$DoD <- as.Date(MOMOfile$DoD)
#' MOMOfile$DoR <- as.Date(MOMOfile$DoR)
#' 
#' holidays_file_name <- system.file("testdata", "holidays.txt", package = "momo")
#' hfile <- read.csv(holidays_file_name, sep = ";", stringsAsFactors = FALSE)
#' hfile$date <- as.Date(hfile$date)
#'
#' launch_momo(MOMOfile, hfile)
launch_momo <- function(MOMOfile, hfile){

  if(!opts$setByUser){
    stop("You have not set the options in function momo::SetOpts. You need to do this before using momo::RunMoMo")
  }

  if(opts$verbose) cat("Welcome to MOMOpack for R.\n\n")

  t0 <- system.time({

    if(opts$verbose) cat("DONE\n")

    if(opts$verbose) cat("\nCreating MOMO input... ")
      t2 <- system.time({
        MOMOinput <- makeMOMOinput(MOMOfile, opts$DoA, opts$DoPR, hfile,
                                   country=opts$country, source=opts$source, 
                                   colnames=c("DoD", "DoR", "age"),
                                   WStart=opts$WStart, WEnd=opts$WEnd, Ysum=opts$Ysum, 
                                   Wsum=opts$Wsum, groups=opts$MOMOgroups, 
                                   models=opts$MOMOmodels, delayCorr=opts$back, histPer=opts$WWW,
                                   compatibility.mode=TRUE)
      })
      
      if(opts$verbose) cat(sprintf("DONE (in %s seconds)\n", round(t2[3], 2)))
      
      if(opts$verbose) cat("Iterating over age groups:\n")
      
      MOMOoutput <- analyzeMOMO(MOMOinput, datesISO=opts$datesISO, useAUTOMN=opts$useAUTOMN,
                                USEglm2=opts$USEglm2, compatibility.mode=TRUE, verbose=opts$verbose)

      dataExport$toSave <- vector("list", length=length(MOMOoutput))
      
      for(j in 1:length(dataExport$toSave)){
        dataExport$toSave[[j]] <- MOMOoutput[[j]]$toSave
        MOMOoutput[[j]]$toSave <- NULL
      }
      
      if(opts$verbose) cat("Joining output... ")
      MOMOjoinedOutput <- joinMOMOoutput(MOMOoutput)
      if(opts$verbose) cat("DONE\n")

      if (opts$verbose) cat("Creating MOMO directories and writing all output to disk... ")
      MOMOdirs <- createMOMOdirectories(MOMOoutput, opts$WDIR)
      writeMOMOoutput(MOMOjoinedOutput, MOMOdirs, MOMOoutput)
      if (opts$verbose) cat("DONE\n")


      if (opts$plotGraphs) {
        t3 <- system.time({
          if (opts$verbose) cat("\nPlotting graphs:")
          if (opts$verbose) cat(" (Control graphs)")
          
          controlGraphsMOMO(MOMOoutput, MOMOdirs)
          if (opts$verbose) cat(" (Excess graphs)")
          
          excessGraphsMOMO(MOMOoutput, MOMOdirs)
          if (opts$verbose) cat(" (Fit graphs)")
          
          fitGraphsMOMO(MOMOoutput, MOMOdirs)
          if (opts$verbose) cat(" (CUSUM graphs)")
          
          CUSUMgraphsMOMO(MOMOoutput, MOMOdirs)
        })
        
        if (opts$verbose) cat(sprintf(" DONE \n\t(in %s seconds)\n", round(t3[3], 1)))
      }

  })

  if(opts$verbose) cat("\nCompleted the analysis in ",round(t0[3],1)," seconds total.\n")
}
