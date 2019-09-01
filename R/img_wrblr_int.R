#internal warbleR function to save image files in jpeg and tiff format, arguments similar to jpeg
# filename must include the image type (jpeg, jpg or tiff)
img_wrlbr_int <- function(filename, path = NULL, res = 160, units = "in", width = 8.5, height = 11){
  
  # add path to filename
  flnm <- file.path(path, filename)

  # jpeg 
  if (grepl("jpeg$|jpg$", filename)) 
        jpeg(filename = flnm, res = res, units = units, width = width, height = height) else # or tiff
          tiff(filename = flnm, res = res, units = units, width = width, height = height) 
}
  
# author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on aug-31-2019 (MAS)
