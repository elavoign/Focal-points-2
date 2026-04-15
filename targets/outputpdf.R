# targets/outputpdf.R

outputpdf <- function() {
  list(
    tar_target(
      outputs_pdf,
      {
        # Anclas
        graphs_outputs_files
        heatmaps_outputs_files

        pdf_path <- "outputs/outputs_all.pdf"
        index_path <- "outputs/outputs_all_index.csv"

        all_imgs <- unique(c(graphs_outputs_files, heatmaps_outputs_files)) |> sort()

        if (length(all_imgs) == 0) {
          stop("No hay imágenes (graphs/heatmaps) para armar el PDF.")
        }

        dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)

        img_index <- tibble::tibble(
          n = seq_along(all_imgs),
          file = all_imgs
        )

        readr::write_csv(img_index, index_path)
        print(img_index)

        img_list <- lapply(all_imgs, magick::image_read)
        img <- do.call(c, img_list)

        magick::image_write(img, path = pdf_path, format = "pdf")

        pdf_path
      },
      format = "file"
    )
  )
}