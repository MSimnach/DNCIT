#' Open AI CLIP
#'
#' @param params_open_ai_clip Specification which open ai clip model to use, default is (ViT-B-32, pretrained=laion2bs34bb79k)
#' @param dir_path Path to the directory containing the images
#' @param all_files List of all files in the directory
#'
#' @return Feature representation of the image obtained by open ai model
#' @export
r_open_ai_clip <- function(params_open_ai_clip=NULL, img_dir_path=dir_path, img_file_names=all_files){
  progress_measure <- progressr::progressor(along = img_file_names)
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
  for (file in img_file_names){
    file_path <- paste0(img_dir_path, file)
    img <- PIL_image$open(file_path)

    img_preprocessed <- preprocess(img)$unsqueeze(as.integer(0))

    feature_rep <- model$encode_image(img_preprocessed)$float()
    feature_rep <- feature_rep/feature_rep$norm(dim=as.integer(-1), keepdim = reticulate::r_to_py(TRUE))
    feature_rep <- feature_rep$view(as.integer(-1))$detach()$numpy()

    X_list <- append(X_list, list(feature_rep))
    progress_measure(message = sprintf("Adding %s", img_file_names[file]))
  }

  X <- do.call(rbind, X_list)
  X_list <- NULL
  model_and_preprocess <- NULL
  return(X)
}
