function gTextLeft(ax,T,rightPadFrac,topPadFrac,edgecolor,fontsize)
%GTEXT Adds the supplied text to the upper-right corner in the given axes

padFrac = 0.02;

if(nargin == 4)
    edgecolor = 'k';
    fontsize = 12;
elseif(nargin == 5)
    fontsize = 12;
end

xlim = get(ax,'xlim');
ylim = get(ax,'ylim');

text(xlim(1) + range(xlim)*rightPadFrac, ylim(2) - range(ylim)*topPadFrac,T,'HorizontalAlignment','left','VerticalAlignment', ...
    'top','edgecolor',edgecolor,'parent',ax,'fontsize',fontsize,'BackgroundColor',[1 1 1]);

end