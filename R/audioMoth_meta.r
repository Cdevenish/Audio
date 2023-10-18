
#'Read audiomoth wav file metadata

#'@param x Character vector of audiomoth wav file(s).
#'@returns Data.frame of metadadata, containing the time and date of recording (POSIXct), 
#' time zone, temperature, voltage remaining on battery, gain setting, audiomoth id, 
#' filename, filesize on disk, and full text of comment in these columns, 
#' time|tz|temp|volt|gain|id|filename|filesize|comment, respectively


audioMoth_meta <- function(x){
  
  out <- lapply(x, function(i){
  
  
  ## Check wav extension
  if(tolower(sub(".*\\.(wav)$", "\\1", i, ignore.case = TRUE)) != "wav") {
    stop("File doesn't have .wav extension?")
  }
  
  con <- file(i, open = "rb")
  on.exit(close(con))
  if(!isSeekable(con)) stop("seek error")
  
  ## Check file internally (from bioacoustics metadata function)
  RIFF <- readChar(con, 4)
  
  if (length(RIFF) == 0 || RIFF != "RIFF"){
    stop("This does not seem to be a valid wav file.")
  }
  
  # #riff.size <- readBin(con, integer(), size = 4, endian = "little")
  # 
  # WAVE <- readChar(con, 4)  # "WAVE"
  # 
  # if (length(WAVE) == 0 || WAVE != "WAVE")  {
  #   stop("This does not seem to be a valid wav file.")
  # }
  # 
  
  ## Comment begins at byte 56 and is 488 bytes long
  
  seek(con, 56) # set to start of comment
  #head <- readChar(con, nchars = 488, useBytes = TRUE) # can do this, which truncates anyway... 
  #head <- rawToChar(readBin(con, what = "raw", n = 488, size = 1)) # throws error with nuls - but can see full text of comment 
  head_bin <- readBin(con, what = "raw", n = 488, size = 1)
  
  # find embedded nuls, and truncate at first
  nuls <- grep("00", head_bin, fixed = TRUE)
  if(length(nuls) > 0) nchar <- nuls[1] - 1 else nchar <- 488
  seek(con, 56) # reset to start of comment
  head <- readChar(con, nchars = nchar, useBytes = TRUE)
  #head
  #close(con)
  
  # extract info with regexes
  time <- sub(".*(\\d\\d:\\d\\d:\\d\\d\\s\\d\\d\\/\\d\\d\\/[[:digit:]]{4}).*", "\\1", head)
  tz <- sub(".*(UTC[-|\\+]?\\d?:?\\d?\\d?).*", "\\1", head)
  time <- strptime(time, "%H:%M:%S %d/%m/%Y", tz = "UTC") ## add in tz here... TODO
  temp <- as.numeric(sub(".*(-?\\d{1,2}\\.\\d)C.*", "\\1", head))
  v <- as.numeric(sub(".*(\\d\\.\\d)V.*", "\\1", head))
  gain <- sub(".*\\s([[:alpha:]]*)\\sgain.*", "\\1", head)
  
  id <- sub(".*AudioMoth\\s([[:alnum:]]*)\\s.*", "\\1", head)
  
  res <- data.frame(time = time, tz = tz, temp = temp, 
                    volt = v, gain = gain, id = id, 
                    filename = basename(i), 
                    filesize = file.size(i)/1000, 
                    comment = head)
  return(res)
  
  }
  )
  
  return(do.call(rbind, out))
  
}
