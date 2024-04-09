#' Tucker decomposition
#'
#' @param params_tuckerD Parameters dim_reduced of the reduced array dimension (default c(10,10) for 2D images), and max_iter, tol as described in `rTensor::tucker()`
#' @param img_dir_path Path to the directory with the images
#' @param imgs_array nx(pixel)-array of images
#'
#' @return X Feature representations of the images of dimension as vectorization of dim_reduced array
#' @export
tucker_decomposition <- function(params_tuckerD=list(dim_reduced = c(10,10)), img_dir_path=NULL, imgs_array=NULL){
  if(is.null(imgs_array)){
    img_dir_path <- append_slash(img_dir_path)
    img_file_names <- list.files(img_dir_path, full.names = TRUE)
    if(requireNamespace("progressr", quietly = TRUE)){
      progress_measure <- progressr::progressor(along = img_file_names)
    }

    img_check_dim <- png::readPNG(img_file_names[1])
    img_dim <-length(dim(img_check_dim))
    n <- length(img_file_names)

    imgs <- array(NA, dim=c(n, dim(img_check_dim)))
    for (i in seq_along(img_file_names)){
      if(img_dim == 2){
        imgs[i,,] <- png::readPNG(img_file_names[i])
      }else if(img_dim == 3){
        imgs[i,,,] <- png::readPNG(img_file_names[i])
      }else{
        stop('Tucker decomposition is currently only implemented for 2D and 3D images')
      }
      if(requireNamespace("progressr", quietly = TRUE)){
        progress_measure(message = sprintf("Adding %s", img_file_names[i]))
      }
    }
  }else if(is.null(img_dir_path)){
    imgs <- imgs_array
  }else{
    stop('Neither path to image directory nor images are given for Tucker decomposition.')
  }

  params_tuckerD$'tnsr' <- rTensor::as.tensor(imgs)
  reduced_dim <- params_tuckerD$'dim_reduced'
  params_tuckerD$'ranks' <- c(n, reduced_dim)
  params_tuckerD$'dim_reduced' <- NULL
  updated_tucker_params <- update_params(rTensor::tucker, params_tuckerD)
  tuckerD <- do.call(rTensor::tucker, updated_tucker_params)

  X <- rTensor::rs_unfold(tuckerD$Z, 1)@data
  return(X)
}
