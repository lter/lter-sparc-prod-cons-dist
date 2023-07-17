function [int] = vertint(x,z,mxdep)

%This code performs a correct trapezoidal integration if the 
%integration depth is greater than or equal to the deepest depth.  If the
%integration depth is no more than 10 m deeper than the deepest sampling
%depth, it performs a crude approximate trapezoidal integration.  Otherwise
%it returns NaN.

data=[z,x];
data=sortrows(data);
z=data(:,1);
x=data(:,2);
clear data
ind=max(find(z<=mxdep));
xint=x(1)*z(1);
for i=2:ind
    xint(i)=xint(i-1)+(z(i)-z(i-1))*(x(i-1)+x(i))/2;
end
if mxdep>z(ind)
    if ind<length(z)
        xtemp=x(ind)*(z(ind+1)-mxdep)/(z(ind+1)-z(ind)) + x(ind+1)*(mxdep-z(ind))/(z(ind+1)-z(ind));
        xint(i+1)=xint(i)+(mxdep-z(ind))*(x(ind)+xtemp)/2;
    else
        if mxdep-z(ind)<10
            xint(i+1)=xint(i)+(mxdep-z(ind))*x(ind);
        else
            xint=NaN;
        end
    end
end

int=xint(end);
