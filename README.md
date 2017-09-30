# quad-pic-effect

Scala script to split an image recursively into quadrants that are filled with the avergae RGB of all the pixels in it. The recursionn picks the next quadrant with the most variance between its RGB(the average of its constituent pixels) and RGB of each pixel.

This produces a neat effect where the more detailed sections of an image have more quadrants in them.

Gif of process:
![Final GIF](https://github.com/owaisiqbal91/quad-pic-effect/blob/master/out/finalgif.gif "Final GIF")
