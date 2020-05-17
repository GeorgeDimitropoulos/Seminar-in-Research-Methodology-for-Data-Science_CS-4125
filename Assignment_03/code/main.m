%load source and destination images
destinationImage = im2double(imread('target.jpg'));
sourceImage = im2double(imread('source.jpg'));
 %let user specify the region of interest, where the source image will be blended into   
 RegionOfInterest=roipoly(destinationImage);
 [y x]=find(RegionOfInterest);
 
 %rescale source image, so that it is rescaled in the size of the ROI, and
 %the rest is padded with zeros, to be the same size as the destination
 %image
 adaptedSource=zeros(size(destinationImage));
 y1 = min(y)-1; 
 y2 = max(y)+1; 
 x1 = min(x)-1; 
 x2 = max(x)+1;
    
 yIndex = (y1:y2);
 xIndex = (x1:x2);
 sourceResized=imresize(sourceImage,[y2-y1+1,x2-x1+1]);
 [yk,xk,zk]=size(sourceResized);
   
 yIndex2=(1:yk);
 xIndex2=(1:xk);
 adaptedSource(yIndex, xIndex, :) = sourceResized(yIndex2,xIndex2,:);

%computing the final blended image with the use of mixedBlend funciton
 blended = mixedGradients(adaptedSource, RegionOfInterest, destinationImage);
 imshow(blended)
 imwrite(blended,'out.png')