import os
import sys
import numpy as np
import argparse

# The output of the package Plant Segmentation with Graph Cuts returns only 
# the image after the parameter 'skip' therefore, there is a label frame 
# in the npz file for each 'crop' image in the folder.

def options():
    """Parse command-line options."""
    description = 'split labels from nympy block'

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument("--indir", help="Input directory.", required=True)

    parser.add_argument("--debug", help="Set debugging mode.", default=None)
    args = parser.parse_args()

    return args

def main():
    """Run main program."""
    args = options()

    # Splits the block of labels into single image numpy files
    folder_name = args.indir
    file_names = sorted(os.listdir(folder_name))

    crop_files = [f for f in file_names if f[-8:] == 'crop.jpg']
    seg_gc_files = [f for f in file_names if f[-7:] == 'seg.jpg']

    all_labels = np.load(folder_name + '/labels.npz')['labels']
    h,w,N = all_labels.shape
    if N != len(crop_files):
        sys.exit('Different number of segmentations and cropped images')

    for i in range(N):
        labels_file = folder_name + '/' + crop_files[i][:-4] + '_labels'
        print(labels_file)
        np.save(labels_file, all_labels[:,:,i])
        if i==-1:
            break

if __name__ == '__main__':
    main()
