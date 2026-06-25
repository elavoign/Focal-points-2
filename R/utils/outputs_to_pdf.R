get_output_images <- function(out_dir = "outputs") {

  exts <- c("png", "jpg", "jpeg", "webp", "tif", "tiff")
  pattern <- paste0("\\.(", paste(exts, collapse = "|"), ")$")

  files <- list.files(
    path = out_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  files <- files[grepl(pattern, files, ignore.case = TRUE)]
  files <- sort(files)

  files
}

outputs_to_pdf <- function(image_files,
                           pdf_path = file.path("outputs", "outputs_all.pdf")) {

  if (length(image_files) == 0) {
    stop("No se encontraron imágenes en outputs/.")
  }

  dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)

  img_list <- lapply(image_files, magick::image_read)
  img <- do.call(c, img_list)

  magick::image_write(img, path = pdf_path, format = "pdf")

  pdf_path
}
