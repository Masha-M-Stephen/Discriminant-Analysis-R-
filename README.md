# Discriminant-Analysis-R-
The goal here is to predict the type of ground cover from a satellite image broken up into pixels.

Description from UCI Machine Learning database:
The database consists of the multi-spectral values of pixels in 3x3 neighborhoods in a satellite image, and
the classification associated with the central pixel in each neighborhood. The aim is to predict this
classification, given the multi-spectral values. In the sample database, the class of a pixel is coded as a
number.

The Landsat satellite data is one of the many sources of information available for a scene. The
interpretation of a scene by integrating spatial data of diverse types and resolutions including
multispectral and radar data, maps indicating topography, land use etc. is expected to assume significant
importance with the onset of an era characterized by integrative approaches to remote sensing (for
example, NASA's Earth Observing System commencing this decade). Existing statistical methods are illequipped for handling such diverse data types. Note that this is not true for Landsat MSS data considered
in isolation (as in this sample database). This data satisfies the important requirements of being numerical
and at a single resolution, and standard maximum-likelihood classification performs very well.
Consequently, for this data, it should be interesting to compare the performance of other methods against
the statistical approach.
One frame of Landsat MSS imagery consists of four digital images of the same scene in different spectral
bands. Two of these are in the visible region (corresponding approximately to green and red regions of
the visible spectrum) and two are in the (near) infra-red. Each pixel is a 8-bit binary word, with 0
corresponding to black and 255 to white. The spatial resolution of a pixel is about 80m x 80m. Each
image contains 2340 x 3380 such pixels.
The database is a (tiny) sub-area of a scene, consisting of 82 x 100 pixels. Each line of data corresponds
to a 3x3 square neighborhood of pixels completely contained within the 82x100 sub-area. Each line
contains the pixel values in the four spectral bands (converted to ASCII) of each of the 9 pixels in the 3x3
neighborhood and a number indicating the classification label of the central pixel. The number is a code
for the following classes:

Number Class
1 red soil
2 cotton crop
3 grey soil
4 damp grey soil
5 soil with vegetation stubble
6 mixture class (all types present)
7 very damp grey soil
Note: There are no examples with class 6 in this dataset.
2

The data is given in random order and certain lines of data have been removed so you cannot reconstruct
the original image from this dataset.
In each line of data the four spectral values for the top-left pixel are given first followed by the four
spectral values for the top-middle pixel and then those for the top-right pixel, and so on with the pixels
read out in sequence left-to-right and top-to-bottom. Thus, the four spectral values for the central pixel are
given by attributes 17,18,19 and 20. 
