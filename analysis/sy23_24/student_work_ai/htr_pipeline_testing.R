library(reticulate)

# Install necessary packages #
# py_install(packages = c( "onnxruntime", "numpy", "opencv-python", "scikit-learn", "editdistance", "path"), envname = "r-reticulate")
# py_install(packages = "matplotlib", envname = "r-reticulate")
py_install(packages = "htr_pipeline", envname = "r-reticulate")
# Using r-reticulate package #
use_virtualenv("r-reticulate", required = TRUE)

source_python("~/Downloads/HTRPipeline/setup.py")
source_python("~/Downloads/HTRPipeline/scripts/demo.py")
