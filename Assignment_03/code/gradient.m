function [ G ] = gradient( h, w )
% h:height of the image w:width of image 
% i,j,v are initialised with zero values and dimensions 2*h*(w-1)+2*w*(h-1)
% i vector contains the row indicators
% j vector contains the column indicators and
% v vector contains the values - 1/2 and 1/2
i = zeros(2*h*(w-1)+2*w*(h-1),1);
j = zeros(2*h*(w-1)+2*w*(h-1),1);
v = zeros(2*h*(w-1)+2*w*(h-1),1);

count = 0;
position = 1;  
columns_counter = 1;    
% The variable k is the number for the columns pixels (in the last column there are no right neighbouring pixel)
% The variable m is the number for the rows pixels (in the last row there are no lower neighbouring pixel)
k = h*(w-1);
m = w*(h-1); 

% find the horizontal differences for each row
% when the count=w-1 then the comparisons for the first row have finisihed and you increase the columns_counter

for rows_counter = 1:k
    if count == w-1 
        columns_counter = columns_counter+1;   
        count = 0;
    end
    
	% v vector contains the values - 1/2
    i(position) = rows_counter;
    j(position) = columns_counter;
    v(position) = v(position)-1/2;
    columns_counter = columns_counter+1;
    position = position+1;

    
    % v vector contains the values + 1/2
    i(position) = rows_counter;
    j(position) = columns_counter;
    v(position) = 1/2;
    position = position+1;
    
    
    count = count+1;
    
end

% first the vertical differences for each column in order to get the vertical differences for each column

for columns_counter = 1:m 
	%v vector contains the values - 1/2
    i(position) = h*(w-1)+columns_counter;
    j(position) = columns_counter;
    v(position) = v(position)- 1/2;
    position = position+1; 
    
	%v vector contains the values + 1/2
    i(position) = h*(w-1)+columns_counter;
    j(position) = columns_counter+w; 
    v(position) = 1/2;
    position = position+1;  

end
% create the sparse matrix
G = sparse(i,j,v);


