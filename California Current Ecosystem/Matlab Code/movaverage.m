function output = movaverage(x,y,window)

count = 1;
for i=min(x):1:max(x)
    i
    ind = find(abs(x-i)<window);
    output(count,1) = i;
    output(count,2) = mean(y(ind));
    count = count+1;
end