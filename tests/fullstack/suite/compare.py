import numpy as np
import cv2
import sys

def main():
	compare(sys.argv[1], sys.argv[2])

def compare(filename_a, filename_b):

	imageA = cv2.imread(filename_a)
	imageB = cv2.imread(filename_b)

	err = np.sum((imageA.astype("float") - imageB.astype("float")) ** 2)
	err /= float(imageA.shape[0] * imageA.shape[1])

	print err == 0

main()

