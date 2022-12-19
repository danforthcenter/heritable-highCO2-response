#!/usr/bin/env python
# This workflow analyzes the color and shape of the plants
# in multi-plant trays. For each image, it requires a numpy file with the
# labels indicating the segmented plants.

import os
import cv2
import numpy as np
import argparse

from plantcv import plantcv as pcv

# In order to set a fixed index to each plant depending on its position
# we create a six by four grid (for a 24 plants tray) and each plant is
# associated to the square where it has the most pixels.
ROWS = 6
COLUMNS = 4
N_PLANTS = ROWS*COLUMNS

def options():
    """Parse command-line options."""
    description = 'Analyze multi-plant form labels PlantCV workflow.'

    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("--image", help="Input image.", required=True)
    parser.add_argument("--result", help="Results JSON file.", required=True)
    parser.add_argument("--outdir", help="Output images directory.",
                        required=True)
    parser.add_argument("--writeimg", help="Save output images.",
                        action="store_true")
    parser.add_argument("--debug", help="Set debugging mode.", default=None)
    args = parser.parse_args()

    return args




def main():
    """Run main program."""
    args = options()

    pcv.params.debug = args.debug
    pcv.params.line_thickness = 5
    pcv.params.dpi = 200

    # Open image
    img, imgpath, imgname = pcv.readimage(filename=args.image)
    h, w,_ = img.shape
    # load labels
    labels = np.load(imgpath+'/'+imgname[:-4]+'_labels.npy')

    # order the labels to also use as index for each plant
    # grid of index
    X,Y = np.meshgrid(np.linspace(0,COLUMNS, num=w, endpoint=False),
                        np.linspace(1,ROWS+1, num=h, endpoint=False))
    idx_image = ROWS*np.floor(X)+np.floor(Y)

    # re-labeling with the new index depending on position
    re_labels = labels.copy()
    idx_present = []
    for idx in range(1,N_PLANTS+1):
        mask_region = ((labels==idx)*idx_image).astype(np.uint8)
        bcount = np.bincount(mask_region.reshape(-1)) # count number of pixels
                                                     #    per value (index)
        bcount[0] = 0 # Ignore 0
        new_idx = bcount.argmax()
        if new_idx != 0:
            re_labels[labels==idx] = new_idx
            if bcount[new_idx] > 50:
                idx_present.append(new_idx)

    # The result file should exist if plantcv-workflow.py was run
    if os.path.exists(args.result):
        # Open the result file
        results = open(args.result, "r")
        # The result file would have image metadata in it from
        # plantcv-workflow.py, read it into memory
        metadata = results.read()
        # Close the file
        results.close()
        # Delete the file, we will create new ones
        os.remove(args.result)
    else:
        # If the file did not exist (for testing),
        # initialize metadata as an empty string
        metadata = "{}"

    # Create a copy of the image for visualization
    plant_shape = np.copy(img)
    # Loop over all the seeds
    for i in idx_present:
        mask = (re_labels == i).astype(np.uint8)

        plant, plant_str = pcv.find_objects(img=img, mask=mask)
        splant, smask = pcv.object_composition(img=img, contours=plant, hierarchy=plant_str)
        plant_shape = pcv.analyze_object(img=plant_shape, obj=splant, mask=smask)

        _ = pcv.analyze_color(rgb_img=img, mask=smask,
                                      hist_plot_type="hsv")
        #Add custom output
        pcv.outputs.add_observation(
                        variable='plant_index',
                        trait='plant position in tray',
                        method='Top-to-bottom, left-to-right',
                        scale='int',
                        datatype=int,
                        value=int(i),
                        label='index')

        # At this point we have observations for one plant
        # We can write these out to a unique results file
        # Here the name of the results file will have the
        # plant index combined with the original result filename
        filename = os.path.splitext(args.result)[0] + "_" + str(i) + ".txt"
        # Save the existing metadata to the new file
        with open(filename, "w") as r:
            r.write(metadata)
        pcv.print_results(filename=filename)
        # The results are saved, now clear out the observations
        # so the next loop adds new ones for the next plant
        pcv.outputs.clear()

    if args.writeimg:
        bname = os.path.splitext(imgname)[0]
        outname = os.path.join(args.outdir, bname + "_shapes.jpg")
        pcv.print_image(img=plant_shape, filename=outname)


if __name__ == '__main__':
    main()
