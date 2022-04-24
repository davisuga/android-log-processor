let _ =
  let fileName = Array.get Sys.argv 1 in
  AndroidLogProcessor.Filter.processFile fileName
