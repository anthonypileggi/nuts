#' Stitch recent screenshots together into a gif
#' @param n number of recent screenshots
#' @param outfile name of output file
#' @param dir screenshot directory
#' @param fps frames per second for resulting gif
#' @return full path to gif (also in your clipboard)
#' @importFrom magrittr "%>%"
#' @export
stitch <- function(n,
                   outfile,
                   outdir = getwd(),
                   dir = "/Users/anthony/Dropbox/Screenshots",
                   fps = 1) {

  recent_files <- dir %>%
    list.files(full.names = TRUE) %>%
    file.info() %>%
    dplyr::mutate(file = row.names(.)) %>%
    dplyr::arrange(desc(mtime)) %>%
    head(n)

  # load target files
  img <- magick::image_read(recent_files$file)

  # scale all to the size of the smallest image
  info <- magick::image_info(img)
  id <- which.min(info$width * info$height)
  img <- magick::image_scale(img, paste0(info$width[id], "x", info$height[id]))

  # stitch files into a gif
  gif <- magick::image_animate(img, fps = fps)

  # write gif to 'outfile'
  out <- magick::image_write(gif, paste0(file.path(outdir, outfile), ".gif"))

  # copy to clipboard (if available)
  if (clipr::clipr_available())
    clipr::write_clip(out)

  out
}