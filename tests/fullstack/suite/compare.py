# Manhattan norm - how much the image is off
# Zero norm - how many pixels differ 

import sys
from scipy.misc import imread
from scipy.linalg import norm
from scipy import sum, average

def main():
  file1, file2 = sys.argv[1:1+2]
  img1 = to_grayscale(imread(file1).astype(float))
  img2 = to_grayscale(imread(file2).astype(float))
  n_m, n_0 = compare_images(img1, img2)
  # print "Manhattan:", n_m, "/ per pixel:", n_m/img1.size
  # print "Zero:", n_0, "/ per pixel:", n_0*1.0/img1.size
  print n_0 # Zero norm 

def compare_images(img1, img2):
  img1 = normalize(img1)
  img2 = normalize(img2)
  diff = img1 - img2 
  m_norm = sum(abs(diff))
  z_norm = norm(diff.ravel(), 0)
  return (m_norm, z_norm)

def to_grayscale(arr):
  if len(arr.shape) == 3:
    return average(arr, -1)
  else:
    return arr

def normalize(arr):
  rng = arr.max() - arr.min()
  amin = arr.min()
  return (arr - amin)*225/rng

if __name__ == "__main__":
  main()