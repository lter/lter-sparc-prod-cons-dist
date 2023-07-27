function [output] = MakeBox(x,y,sz,col)

fill([x-sz(1),x+sz(1),x+sz(1),x-sz(1),x-sz(1)],[y-sz(2),y-sz(2),y+sz(2),y+sz(2),y-sz(2)],'r','FaceColor',col)