Hereunder the topics and approachs. 
See `report.pdf` file for the interpretations and results.

# Part 1.

Using the data set CATSnDogs.

This is a small subset of the original data which comprises tens of thousands of images of cats and dogs.
This smaller data set consists of 99 cats and 99 dogs (the order they appear in the data set), and images have been re-scaled to 64 by 64 pixels (which is pretty low resolution!).

## (A) Classification: Can you tell cats from dogs based on low-resolution images?

Provide reliable estimates for the misclassification error rate on new data - discuss how you estimate this and why you think it’s a reliable estimate for future data.
How many features do you need? How do you decide this?
Which classification technique do you prefer and why.
(In image classification there are more complex methods for extracting features than we have discussed - you don’t need to use these but work with the methods we’ve discussed in class).

## (B) Dimension reduction and data representation:

Is there a ”typical cat” or a ”typical dog”? Explain how you deduced this from methods you applied.
What do low-dimensional data representations extract from this data set? Apply and interpret results from SVD, NMF, MDS and SOMS.
## (C) Clustering:
Is it easy to learn that there are two types of animals in the image data? Explain Are there any particular cats or dogs that don’t fit with the rest of their species? Any particular features important for clustering?
1

# Part 2

About the spam data.
The data set comprises roughly 4000 emails and features extracted from them. The emails have been labeled ”spam” and ”nonspam”. Note, I have added random noise to the data so it cannot be matched to the original data set. In addition, I have mislabeled about 5% of the observations (changing their labels).
Try to identify the mislabeled observations. You can use any methods you want. A mislabeled observation would probably be consistently wrongly predicted across methods and subsamples - can you exploit this?

# Part 3

The TCGA data comprising 6 types of cancers with measured gene expression for 20530 genes.
Compare one-against-all, pairwise-classification and multi-class classification techniques. Discuss the results - are some cancers easy to tell from other cancers using one method over another? Which cancers are difficult to tell apart and why?
How do the methods compare with respect to selected features? Are the same features selected? Why/Why not? Roughly same number of features selected? Why/Why not?
