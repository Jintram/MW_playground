


# Libs
import napari
import numpy as np
from skimage.measure import label, regionprops
from skimage import io
from skimage.morphology import disk
import cv2 # for erosion

# Load example image
img_toseg = io.imread('/Users/m.wehrens/Data_UVA/example-datasets/HeLa-Cells-MW/example_cell_image.tif')

# Create the viewer and add the image
viewer = napari.view_image(np.log(img_toseg), name='toseg_log', opacity=1)

# Create a very simple threshold image for show
threshold = np.median(img_toseg)*2.5
mask_threshold = img_toseg > threshold
label_mask = label(mask_threshold)

# Add layers with the mask and the labeled mask
viewer.add_image(mask_threshold, name='mask_threshold', opacity=0.5)
viewer.add_labels(label_mask, name='label_mask')

# Now also create points in a very simple way
mask_eroded = cv2.erode(mask_threshold.astype(np.uint8), disk(100/4), iterations=1) # assuming cellular diameter of 100
centroids = [r.centroid for r in regionprops(label(mask_eroded)) if r.area > 25]
viewer.add_points(centroids, name='centroids', size=20, face_color='red')


viewer.close()
