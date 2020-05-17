function FinalImage = mixedGradients(sourceImage, maskROI, targetImage);
% INPUTS:
% 1) sourceImage: the resized source image that we want to blend into the
% target image
% 2) targetImage:the image inside which the source will be blended into
% 3) maskROI: a logical matrix indicating the region where we want to blend
% 
% OUTPUT:
% FinalImage : the final blended image


% find indices of the region of interest pixels
[yMaskIndex xMaskIndex] = find(maskROI > 0);
numberOfPixels = sum(sum(maskROI));

% computing dimensions of target image.
[height, width, channels] = size(targetImage);
PixelIndex = zeros(height, width);

%initialize the final image , the sparse matrix and the gradient vector g.
FinalImage = targetImage; 
G = sparse([], [], []);
g = zeros(numberOfPixels, channels);

%assigning an index to each pixel that is inside the ROI. this is very
%helpful during the computation of the sparse matrix values, as it will be
%easier to determine which index of the sparse matrix we have to assign a value.
for j=1:numberOfPixels
    PixelIndex(yMaskIndex(j),xMaskIndex(j)) = j;
end



%temporary matrices that will help us in the gradient computation
sourceDiffs = zeros(1, channels);
targetDiffs = zeros(1, channels);

% for each one of the pixels inside the region of interest we compute the
% sparse matrix values as well as the values of the mixed gradients at this
% pixel
 
for j=1:numberOfPixels
    %getting the coordinated of the pixel
    y = yMaskIndex(j);
    x = xMaskIndex(j);
    % assigning values to sparse matrix.  4for the pixel itself, and -1 for
    % each of its 4 neighbors in the 2 d, if they exist.
    G(j,PixelIndex(y,x)) = 4;
    %checking if neighbor exists in ROI in the x axis.
    if (maskROI(y,x-1) == 1)
        G(j,PixelIndex(y,x-1)) = -1;
        % For each of the r g b channels, compute the highest gradient
        % value between the source and the destination image and add this
        % value to the corresponding g vector element.        
        for i=1:channels
            sourceDiffs(1,i) = sourceImage(y,x,i) - sourceImage(y,x-1,i);
            targetDiffs(1,i) = targetImage(y,x,i) - targetImage(y,x-1,i);
        end
        if abs(sourceDiffs(1,:)) > abs(targetDiffs(1,:))
            g(j,:) = g(j,:) + sourceDiffs(1,:);
        else
            g(j,:) = g(j,:) + targetDiffs(1,:);
        end
    else
        % if neighbor pixel is not within the ROI,which means it is a
        % boundary pixel, we add the value of the target image gradient
        % without checking the source of course.        
        for i=1:channels
            g(j,i) = g(j,i) + targetImage(y,x-1,i);
        end
    end
    
    if (maskROI(y,x+1) == 1)
        G(j,PixelIndex(y,x+1)) = -1;
        for i=1:channels
            sourceDiffs(1,i) = sourceImage(y,x,i) - sourceImage(y,x+1,i);
            targetDiffs(1,i) = targetImage(y,x,i) - targetImage(y,x+1,i);
        end
        if abs(sourceDiffs(1,:)) > abs(targetDiffs(1,:))
            g(j,:) = g(j,:) + sourceDiffs(1,:);
        else
            g(j,:) = g(j,:) + targetDiffs(1,:);
        end
    else
        for i=1:channels
            g(j,i) = g(j,i) + targetImage(y,x+1,i);
        end
    end    
    
    
    %repeating the same procedure in the y axis now.
    if (maskROI(y-1,x) == 1)
        G(j,PixelIndex(y-1,x)) = -1;
        for i=1:channels
            sourceDiffs(1,i) = sourceImage(y,x,i) - sourceImage(y-1,x,i);
            targetDiffs(1,i) = targetImage(y,x,i) - targetImage(y-1,x,i);
        end
        if abs(sourceDiffs(1,:)) > abs(targetDiffs(1,:))
            g(j,:) = g(j,:) + sourceDiffs(1,:);
        else
            g(j,:) = g(j,:) + targetDiffs(1,:);
        end
    else
        for i=1:channels
            g(j,i) = g(j,i) + targetImage(y-1,x,i);
        end
    end
    
    if (maskROI(y+1,x) == 1)
        G(j,PixelIndex(y+1,x)) = -1;
        for i=1:channels
            sourceDiffs(1,i) = sourceImage(y,x,i) - sourceImage(y+1,x,i);
            targetDiffs(1,i) = targetImage(y,x,i) - targetImage(y+1,x,i);
        end
        if abs(sourceDiffs(1,:)) > abs(targetDiffs(1,:))
            g(j,:) = g(j,:) + sourceDiffs(1,:);
        else
            g(j,:) = g(j,:) + targetDiffs(1,:);
        end
    else
        for i=1:channels
            g(j,i) = g(j,i) + targetImage(y+1,x,i);
        end
    end       
end

% for each channel in r,g,b we solve the linear system UG=g. If the result
% is negative, we set it to zero, as we are processing images, so negative
% values are not valid
Ur = G\g(:,1);
Ug = G\g(:,2);
Ub = G\g(:,3);

Ur(Ur < 0) = 0;
Ug(Ug < 0) = 0;
Ub(Ub < 0) = 0;

% merging the computed blended image with the destination image part that
% is outside the ROI
for i=1:numberOfPixels
    y = yMaskIndex(i);
    x = xMaskIndex(i);
    FinalImage(y,x,1) = Ur(i);
    FinalImage(y,x,2) = Ug(i);
    FinalImage(y,x,3) = Ub(i);
   
end
