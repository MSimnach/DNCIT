#' Open AI CLIP
#'
#' @param params_open_ai_clip Specification which open ai clip model to use, default is (ViT-B-32, pretrained=laion2bs34bb79k)
#' @param img_dir_path Path to the directory containing the images
#'
#' @return Feature representation of the image obtained by open ai model
#' @export
r_open_ai_clip <- function(params_open_ai_clip=NULL, img_dir_path=NULL){
  if(!reticulate::py_module_available('open_clip')){
    stop("open_clip not found. Please install the open_clip package in your python environment. You can find help regarding the installion in `vignette(Installation)`.")
  }

  img_dir_path <- append_slash(img_dir_path)
  img_file_names <- list.files(img_dir_path, full.names = TRUE)
  if(requireNamespace("progressr", quietly = TRUE)){
    progress_measure <- progressr::progressor(along = img_file_names)
  }

  device <- ifelse(py_torch$cuda$is_available(), "cuda", "cpu")
  clip_model <- params_open_ai_clip$'clip_model'
  pretrained_on <- params_open_ai_clip$'pretrained_on'

  # load specified open ai model
  if (is.null(clip_model)){
    model_and_preprocess <- open_ai_clip$create_model_and_transforms('ViT-B-32', device=device)
  }else{
    if(is.null(pretrained_on)){
      model_and_preprocess <- open_ai_clip$create_model_and_transforms(clip_model, device=device)
    }else{
      model_and_preprocess <- open_ai_clip$create_model_and_transforms(clip_model, pretrained=pretrained_on, device=device)
    }
  }

  model <- model_and_preprocess[[1]]
  preprocess <- model_and_preprocess[[3]]

  X_list <- list()
  for (i in seq_along(img_file_names)){
    img <- PIL_image$open(img_file_names[i])

    img_preprocessed <- preprocess(img)$unsqueeze(as.integer(0))

    feature_rep <- model$encode_image(img_preprocessed)$float()
    feature_rep <- feature_rep/feature_rep$norm(dim=as.integer(-1), keepdim = reticulate::r_to_py(TRUE))
    feature_rep <- feature_rep$view(as.integer(-1))$detach()$numpy()

    X_list <- append(X_list, list(feature_rep))
    if(requireNamespace("progressr", quietly = TRUE)){
      progress_measure(message = sprintf("Adding %s", img_file_names[i]))
    }
  }

  X <- do.call(rbind, X_list)
  X_list <- NULL
  model_and_preprocess <- NULL
  return(X)
}
