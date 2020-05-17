% load the image 
image=imread('blurryImage.png');
% get the size of the image  h:height of the image w:width of image and d:depth of image
[h w d]=size(image);

% split a color image into its 3 RGB channels and recombine separate color channels into an RGB image.
redChannel = image(:, :, 1)';
greenChannel = image(:, :, 2)';
blueChannel = image(:, :, 3)';
rgbImage = cat(3, redChannel, greenChannel, blueChannel);

U = double(reshape(rgbImage,w*h,d))/255;

% print the image

figure,imshow(image)

%% Main method
% constant parameters 
cu = 0.5;
cs = 3;
% compute the Identity matrix
I=speye(w*h);
% call the gradient function with arguments h,w in order to obtain the gradient matrix G
G=gradient(h,w);
% the computation of vector g which contains the image gradients based on the equation g = G*U
g = G*U;
% compute the Laplace matrix
L=G'*G;
% solve the  (G'*G +cu*I)*U=cs*G'*g+cu*U
ParameterA=L+cu*I;
ParameterB = cs*G'*g+cu*U;

U=ParameterA\ParameterB;
% reshape the result to image
image =uint8(reshape(U,w,h,d)*255);


redChannel2 = image(:, :, 1)';
greenChannel2 = image(:, :, 2)';
blueChannel2 = image(:, :, 3)';
Sharpening_Image = cat(3, redChannel2, greenChannel2, blueChannel2);

% print the Sharpening_Image and save it 
figure, imshow(Sharpening_Image)
imwrite(Sharpening_Image,'out.png')
