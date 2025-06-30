


# Simple script that loads an image, takes the fourier transform, selects the inner circle and then takes the inverse fourier transform to get the image back.

import numpy as np
import matplotlib.pyplot as plt

from skimage import io
from skimage.color import rgb2gray

# load the image
img = io.imread('/Users/m.wehrens/Desktop/MartijnWehrens_bw_HC.jpeg')

# convert to grayscale
img_gray = rgb2gray(img)

# take the fourier transform
img_ft = np.fft.fft2(img_gray)

# show the fourier transform
plt.figure()
plt.imshow(np.log(np.abs(np.fft.fftshift(img_ft))), cmap='gray')
plt.title('Fourier transform')
plt.show()

# select the inner square
rows, cols = img_ft.shape       # dims
crow, ccol = rows//2 , cols//2  # center
r = 50                          # radius
mask = np.zeros((rows, cols), np.uint8) 
mask[crow-r:crow+r, ccol-r:ccol+r] = 1 # set inner square to 1

# now apply the mask
img_ft_masked = img_ft * mask

# now transform back
img_masked = np.abs(np.fft.ifft2(img_ft_masked))

# normalize the image
img_masked_norm = img_masked.copy()
img_min01 = np.percentile(img_masked.flatten(), 1)
img_max99 = np.percentile(img_masked.flatten(), 99)
# rescale the image, using the 1st and 99th percentile
img_masked_norm[img_masked_norm < img_min01] = img_min01
img_masked_norm[img_masked_norm > img_max99] = img_max99
img_masked_norm = (img_masked_norm - img_min01) / (img_max99 - img_min01)

# show a histogram of the image
plt.figure()
plt.hist(img_masked.flatten(), bins=100)
plt.axvline(img_min01, color='k', linestyle='-', linewidth=.5)
plt.axvline(img_max99, color='k', linestyle='-', linewidth=.5)
plt.title('Histogram of masked image')
plt.show()

# now show using a cmap
plt.figure()
plt.imshow(img_masked_norm, cmap='jet') # hot, inferno, cividis, viridis
plt.title('Masked image')
plt.show()

# now save the image to a file, using the same cmap
plt.imsave('/Users/m.wehrens/Desktop/MartijnWehrens_masked_hot2.png', img_masked_norm, cmap='hot')
plt.imsave('/Users/m.wehrens/Desktop/MartijnWehrens_masked_jet2.png', img_masked_norm, cmap='jet')
plt.imsave('/Users/m.wehrens/Desktop/MartijnWehrens_masked_civi2.png', img_masked_norm, cmap='cividis')
plt.imsave('/Users/m.wehrens/Desktop/MartijnWehrens_masked_grey2.png', img_masked_norm, cmap='grey')