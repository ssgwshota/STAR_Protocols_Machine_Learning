cnv_readprofile = function(input,
                           is_dir = FALSE,
                           pattern = NULL,
                           ignore_case = FALSE,
                           sep = "\t",
                           cols = c("chromosome", "start", "end", "segVal"),
                           have_sampleCol = TRUE,
                           sample_col = "sample") {
  stopifnot(
    is.logical(is_dir),
    is.logical(have_sampleCol),
    is.character(sample_col),
    length(sample_col) == 1
  )
  if (is_dir) {
    message("Treat input as a directory...")
    if (length(input) != 1) {
      stop("Only can take one directory as input!")
    }
    # get files and exclude directories
    all.files <- list.files(
      path = input,
      pattern = pattern,
      all.files = FALSE,
      recursive = FALSE,
      ignore.case = ignore_case
    )
    files = all.files[!file.info(file.path(input, all.files))$isdir]
    if (length(files) == 0)
      stop("No files exist, please check!")
    files_path = file.path(input, files)
    files_list = list()
    for (i in seq_along(files_path)) {
      temp = read.csv(
        file = files_path[i],
        sep = sep,
        comment.char = "#",
        stringsAsFactors = FALSE
      )
      if (!all(cols %in% colnames(temp)))
        stop("not all cols are in file, please check.")
      if (have_sampleCol) {
        tempName = unique(temp[, sample_col])
        if (length(tempName) > 1) {
          stop("When input is a directory, a file contains only one sample.")
        }
        temp = temp[, cols]
        colnames(temp) = c("chromosome", "start", "end", "segVal")
        if (nrow(temp) <= 0) {
          warning("Sample ",
                  tempName,
                  " is discarded because of few segments (<=22)")
        } else {
          files_list[[tempName]] = temp
        }
        
      } else {
        message("Select file names as sample names.")
        temp = temp[, cols]
        colnames(temp) = c("chromosome", "start", "end", "segVal")
        if (nrow(temp) <= 0) {
          warning("File ",
                  files[i],
                  " is discarded because of few segments (<=22)")
        } else {
          files_list[[files[i]]] = temp
        }
        
      }
    }
    return(files_list)
  } else if (all(is.character(input))) {
    message("Treat input as a file...")
    if (length(input) > 1) {
      stop("Muliple files are not a valid input, please use directory as input.")
    }
    
    if (!file.exists(input))
      stop("input file not exists")
    if (!have_sampleCol)
      stop("When input is a file, sample column must set.")
    input = read.csv(
      file = input,
      sep = sep,
      comment.char = "#",
      stringsAsFactors = FALSE
    )
  }
  
  if (!sample_col %in% colnames(input))
    stop("sample column user set not exists in input file.")
  if (!all(cols %in% colnames(input)))
    stop("not all cols are in file, please check.")
  samples = unique(input[, sample_col])
  
  res_list = list()
  for (i in seq_along(samples)) {
    tempDF = input[input[, sample_col] == samples[i],]
    tempDF = tempDF[, cols]
    colnames(tempDF) = c("chromosome", "start", "end", "segVal")
    
    if (nrow(tempDF) <= 0) {
      warning("Sample ",
              samples[i],
              " is discarded because of few segments (<=22)")
    } else {
      res_list[[samples[i]]] = tempDF
    }
  }
  
  return(res_list)
}