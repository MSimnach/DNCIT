#' Title
#'
#' @param PIL_img image loaded with PIL.Image.open
#' @param params_open_ai_clip Specification which open ai clip model to use, default is (ViT-B-32, pretrained=laion2b_s34b_b79k)
#'
#' @return Feature representation of the image obtained by open ai model
#' @export
r_open_ai_clip <- function(PIL_img, params_open_ai_clip=NULL){
  if (is.null(params_open_ai_clip)){
    model_and_preprocess <- open_ai_clip$create_model_and_transforms('ViT-B-32', pretrained='laion2b_s34b_b79k')
    model <- model_and_preprocess[[1]]
    preprocess <- model_and_preprocess[[3]]
    img_preprocessed <- preprocess(PIL_img)$unsqueeze(as.integer(0))
    feature_rep = model$encode_image(img_preprocessed)$float()
    feature_rep <- feature_rep/feature_rep$norm(dim=as.integer(-1), keepdim = r_to_py(TRUE))
    #to numpy
    feature_rep <- feature_rep$view(as.integer(-1))$detach()$numpy()
  }
  return(feature_rep)
}
